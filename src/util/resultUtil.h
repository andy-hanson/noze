#pragma once

#include "./result.h"

template <typename Success, typename Failure>
inline Result<Success, Failure> success(Success s) {
	return Result<Success, Failure>{s};
}

template <typename Success, typename Failure>
inline Result<Success, Failure> failure(Failure f) {
	return Result<Success, Failure>{f};
}

template <typename OutSuccess>
struct mapSuccess {
	template <typename InSuccess, typename Failure, typename Cb>
	inline const Result<OutSuccess, Failure> operator()(const Result<InSuccess, Failure> r, Cb cb) {
		return r.match(
			[&](const InSuccess s) {
				return success<OutSuccess, Failure>(cb(s));
			},
			[&](const Failure f) {
				return failure<OutSuccess, Failure>(f);
			});
	}
};

template <typename OutFailure>
struct mapFailure {
	template <typename Success, typename InFailure, typename Cb>
	inline const Result<Success, OutFailure> operator()(const Result<Success, InFailure> r, Cb cb) {
		return r.match(
			[&](const Success s) {
				return success<Success, OutFailure>(s);
			},
			[&](const InFailure f) {
				return failure<Success, OutFailure>(cb(f));
			});
	}
};

template <typename OutSuccess, typename Failure>
struct flatMapSuccess {
	template <typename InSuccess, typename Cb>
	inline const Result<OutSuccess, Failure> operator()(const Result<InSuccess, Failure> r, Cb cb) {
		return r.match(
			cb,
			[](const Failure f) {
				return failure<OutSuccess, Failure>(f);
			});
	}
};
