#pragma once

#include "./arrUtil.h"
#include "./sym.h"
#include "./writer.h"

struct Sexpr;

struct SexprRecord {
	const Sym name;
	const Arr<const Sexpr> children;
};

struct Sexpr {
private:
	enum class Kind {
		arr,
		record,
		str,
		symbol,
	};

	const Kind kind;
	union {
		const Arr<const Sexpr> arr;
		const SexprRecord record;
		const Str str;
		const Sym symbol;
	};
public:

	explicit inline Sexpr(const Arr<const Sexpr> _arr) : kind{Kind::arr}, arr{_arr} {}
	explicit inline Sexpr(const SexprRecord _record) : kind{Kind::record}, record{_record} {}
	explicit inline Sexpr(const Str _str) : kind{Kind::str}, str{_str} {}
	explicit inline Sexpr(const Sym _symbol) : kind{Kind::symbol}, symbol{_symbol} {}

	template <
		typename CbArr,
		typename CbRecord,
		typename CbStr,
		typename CbSymbol
	> inline auto match(
		CbArr cbArr,
		CbRecord cbRecord,
		CbStr cbStr,
		CbSymbol cbSymbol
	) const {
		switch (kind) {
			case Kind::arr:
				return cbArr(arr);
			case Kind::record:
				return cbRecord(record);
			case Kind::str:
				return cbStr(str);
			case Kind::symbol:
				return cbSymbol(symbol);
			default:
				assert(0);
		}
	}
};

template <typename T>
using ToSexpr = const Sexpr (*)(Arena* arena, const T);

template <typename T, typename CbToSexpr>
const Sexpr arrToSexpr(Arena* arena, const Arr<T> a, CbToSexpr cbToSexpr) {
	return Sexpr(map<const Sexpr>{}(arena, a, [&](const T t) {
		return cbToSexpr(t);
	}));
}

void writeSexpr(Writer& writer, const Sexpr s);
