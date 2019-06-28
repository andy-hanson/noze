#pragma once

#include "./bool.h"

template <typename Success, typename Failure>
struct Result {
private:
	const Bool _isSuccess;
	union {
		Success success;
		Failure failure;
	};

public:
	using SuccessType = Success;
	using FailureType = Failure;

	inline Result(const Success s) : _isSuccess{True}, success{s} {}
	inline Result(const Failure f) : _isSuccess{False}, failure{f} {}

	inline const Bool isSuccess() const {
		return _isSuccess;
	}

	inline Success asSuccess() const {
		assert(isSuccess());
		return success;
	}

	inline Failure asFailure() const {
		assert(!isSuccess());
		return failure;
	}

	template <typename CbSuccess, typename CbFailure>
	inline auto match(CbSuccess cbSuccess, CbFailure cbFailure) const {
		if (_isSuccess)
			return cbSuccess(success);
		else
			return cbFailure(failure);
	}
};
