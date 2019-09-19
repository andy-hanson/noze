#include "./diff.h"

#include "./arrUtil.h"
#include "./mutSlice.h"
#include "./writerUtils.h"

namespace {
	template <typename T>
	struct ReversedArr {
		Arr<T> inner;
	};

	template <typename T>
	ReversedArr<T> reversed(const Arr<T> a) {
		return ReversedArr<T>{a};
	}

	template <typename T>
	size_t size(const ReversedArr<T> m) {
		return ::size(m.inner);
	}

	template <typename T>
	T at(const ReversedArr<T> a, const size_t i) {
		return ::at(a.inner, size(a) - 1 - i);
	}

	template <typename T>
	struct ReversedMutSlice {
		MutSlice<T> inner;
	};

	template <typename T>
	ReversedMutSlice<T> reversed(MutSlice<T> m) {
		return ReversedMutSlice<T>{m};
	}

	template <typename T>
	size_t size(ReversedMutSlice<T> m) {
		return ::size(m.inner);
	}

	template <typename T>
	T mutSliceAt(ReversedMutSlice<T> m, const size_t i) {
		return ::mutSliceAt(m.inner, size(m) - 1 - i);
	}

	template <typename T>
	void mutSliceSetAt(ReversedMutSlice<T> m, const size_t i, const T value) {
		::mutSliceSetAt(m.inner, size(m) - 1 - i, value);
	}

	template <typename T>
	void mutSliceFill(ReversedMutSlice<T> m, const T value) {
		for (size_t i : Range{size(m)})
			mutSliceSetAt(m, i, value);
	}

	// Returns the maximum subsequence length between a and each prefix of b.
	// (If inputs are ReversedArr or ReversedMutSlice, this is for suffixes.)
	// Result[i] is the max subseq length between a and slice(b, 0, i).
	// 'scratch' is an input for performance -- it's treated as uninitialized.
	// Based on https://www.ics.uci.edu/~dan/pubs/p341-hirschberg.pdf
	template <typename ArrLike, typename MutSliceLike>
	void getMaximumCommonSubsequenceLengths(
		const ArrLike a,
		const ArrLike b,
		const MutSliceLike result
	) {
		static_assert(std::is_same<ArrLike, Arr<const Sym>>::value
			|| std::is_same<ArrLike, ReversedArr<const Sym>>::value,
			"must be Arr or ReversedArr");
		static_assert(std::is_same<MutSliceLike, MutSlice<size_t>>::value
			|| std::is_same<MutSliceLike, ReversedMutSlice<size_t>>::value,
			"must be MutSlice or ReversedMutSlice");

		// The buffers need to be 1 more than the length of b,
		// because they have an entry at size(b).
		assert(size(result) == size(b) + 1);

		// We are actually calculating a matrix. But we only need to store one row.
		// matrix[r][c] = maximum subsequence length of slice(a, 0, i) and slice(b, 0, j).
		// So the final row (matrix[size(a)]) is the maximum subsequence lengths given all or a, and all prefixes of b.

		// 0th row is all 0s.
		mutSliceFill<size_t>(result, 0);

		for (const size_t rowI : Range{1, size(a) + 1}) {
			// Each row element depends on the left, up, and left-up diagonal entries.
			// So having only one row in memory at a time is tricky.
			// We want to preserve the left-up and up entries from the previous row, so we keep 'left' in a variable
			// and don't write it out until after.

			// First column is always a 0.
			size_t left = 0;
			for (const size_t colI : Range{1, size(b) + 1}) {
				const size_t curRowValue = symEq(at(a, rowI - 1), at(b, colI - 1))
					// if a and b match here, use the diagonal
					? mutSliceAt(result, colI - 1) + 1
					// If a and b don't match, use the left or up value.
					: max<const size_t, comparePrimitive<size_t>>(
						left, // value from left
						mutSliceAt(result, colI)); // value from above
				// Now that we're done reading from result[i - 1], we can write over it.
				mutSliceSetAt(result, colI - 1, left);
				left = curRowValue;
			}
			mutSliceSetAt(result, size(b), left);
		}
	}

	// For each prefix and suffix of b, get the maximum common subsequence length between a and that prefix/suffix.
	// Then the maximum common subsequence is where a prefix and suffix of b meet with the greatest sum of those sizes.
	// Returns index to split 'b' at.
	size_t findBestSplitIndex(const Arr<const Sym> a, const Arr<const Sym> b, MutSlice<size_t> scratch) {
		const size_t i = size(a) / 2;
		// 1 greater because it goes from 0 to size(b) inclusive
		size_t subseqsSize = size(b) + 1;
		assert(size(scratch) >= subseqsSize * 2);
		MutSlice<size_t> leftSubsequenceLengths = mutSlice(scratch, 0, subseqsSize);
		MutSlice<size_t> rightSubsequenceLengths = mutSlice(scratch, subseqsSize, subseqsSize);
		getMaximumCommonSubsequenceLengths(slice(a, 0, i), b, leftSubsequenceLengths);
		getMaximumCommonSubsequenceLengths(
			reversed(slice(a, i + 1)), reversed(b), reversed(rightSubsequenceLengths));
		return arrMaxIndex<size_t, comparePrimitive<const size_t>>{}(
			tempAsArr(leftSubsequenceLengths),
			[&](const size_t leftLength, size_t j) {
				// Note: rightSubsequenceLengths was computed in reverse, so 'j' is from the right here.
				return leftLength + mutSliceAt(rightSubsequenceLengths, j);
			});
	}

	void longestCommonSubsequenceRecur(
		Arena* arena,
		const Arr<const Sym> a,
		const Arr<const Sym> b,
		MutSlice<size_t> scratch,
		ArrBuilder<const Sym>* out
	) {
		if (size(b) == 0) {
			// No output
		} else if (size(a) == 1) {
			const Sym sa = only(a);
			if (contains<const Sym, symEq>(b, sa))
				add<const Sym>(arena, out, sa);
		} else {
			// Always slice 'a' exactly in half. Then find the best way to slice 'b'.
			const size_t aSplit = size(a) / 2;
			const size_t bSplit = findBestSplitIndex(a, b, scratch);
			longestCommonSubsequenceRecur(arena, slice(a, 0, aSplit), slice(b, 0, bSplit), scratch, out);
			longestCommonSubsequenceRecur(arena, slice(a, aSplit), slice(b, bSplit), scratch, out);
		}
	}

	const Arr<const Sym> longestCommonSubsequence(Arena* arena, const Arr<const Sym> a, const Arr<const Sym> b) {
		Arena temp {};
		MutSlice<size_t> scratch = newUninitializedMutSlice<size_t>(&temp, (size(b) + 1) * 2);
		ArrBuilder<const Sym> out {};
		longestCommonSubsequenceRecur(arena, a, b, scratch, &out);
		return finishArr(&out);
	}

	void printDiff(Writer* writer, const Arr<const Sym> a, const Arr<const Sym> b, const Arr<const Sym> commonSyms) {
		const Sym expected = shortSymAlphaLiteral("expected");
		// + 2 for a margin
		const size_t columnSize = 2 + max<size_t, comparePrimitive<size_t>>(
			arrMax<size_t, comparePrimitive<size_t>>{}(a, symSize),
			symSize(expected));

		writeNlIndent(writer);
		writeSymPadded(writer, expected, columnSize);
		writeStatic(writer, "you wrote");

		// This gave us the list of symbols that they have in common.
		// Now we just walk them together.
		size_t ai = 0;
		size_t bi = 0;
		const auto extraA = [&]() -> void {
			writeNlIndent(writer);
			writeRed(writer);
			writeSym(writer, at(a, ai));
			writeReset(writer);
			ai++;
		};
		const auto extraB = [&]() -> void {
			writeNlIndent(writer);
			writeSpaces(writer, columnSize);
			writeRed(writer);
			writeSym(writer, at(b, bi));
			writeReset(writer);
			bi++;
		};
		const auto misspelling = [&]() -> void {
			writeNlIndent(writer);
			writeRed(writer);
			writeSymPadded(writer, at(a, ai), columnSize);
			writeSym(writer, at(b, bi));
			writeReset(writer);
			ai++;
			bi++;
		};
		const auto common = [&]() -> void {
			assert(symEq(at(a, ai), at(b, bi)));
			writeNlIndent(writer);
			writeSymPadded(writer, at(a, ai), columnSize);
			writeSym(writer, at(b, bi));
			ai++;
			bi++;
		};

		for (const Sym commonSym : commonSyms) {
			while (!symEq(at(a, ai), commonSym) && !symEq(at(b, bi), commonSym))
				misspelling();
			while (!symEq(at(a, ai), commonSym))
				extraA();
			while (!symEq(at(b, bi), commonSym))
				extraB();
			common();
		}
		while (ai < size(a) && bi < size(b))
			misspelling();
		while (ai < size(a))
			extraA();
		while (bi < size(b))
			extraB();
	}
}

void diffSymbols(Writer* writer, const Arr<const Sym> a, const Arr<const Sym> b) {
	Arena temp {};
	printDiff(writer, a, b, longestCommonSubsequence(&temp, a, b));
}
