#pragma once

#include "./arrUtil.h"
#include "./writer.h"

struct Sexpr;

struct SexprRecord {
	const Str name;
	const Arr<const Sexpr> children;
};

struct Sexpr {
private:
	enum class Kind {
		arr,
		record,
		str,
	};

	const Kind kind;
	union {
		const Arr<const Sexpr> arr;
		const SexprRecord record;
		const Str str;
	};
public:

	explicit inline Sexpr(const Arr<const Sexpr> _arr) : kind{Kind::arr}, arr{_arr} {}
	explicit inline Sexpr(const SexprRecord _record) : kind{Kind::record}, record{_record} {}
	explicit inline Sexpr(const Str _str) : kind{Kind::str}, str{_str} {}

	template <
		typename CbArr,
		typename CbRecord,
		typename CbStr
	> inline auto match(
		CbArr cbArr,
		CbRecord cbRecord,
		CbStr cbStr
	) const {
		switch (kind) {
			case Kind::arr:
				return cbArr(arr);
			case Kind::record:
				return cbRecord(record);
			case Kind::str:
				return cbStr(str);
			default:
				assert(0);
		}
	}
};

template <typename T>
using ToSexpr = const Sexpr (*)(Arena& arena, const T);

template <typename T, ToSexpr<T> tToSexpr>
const Sexpr arrToSexpr(Arena& arena, const Arr<T> a) {
	return Sexpr(map<const Sexpr>{}(arena, a, [&](const T t) {
		return tToSexpr(arena, t);
	}));
}

void writeSexpr(Writer& writer, const Sexpr s);
