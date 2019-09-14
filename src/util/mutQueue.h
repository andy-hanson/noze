#pragma once

#include "./memory.h"

template <typename T>
struct MutQueue {
	T* _begin;
	size_t capacity;

	// Invariant:
	// If queue is empty:
	//   queueBegin = queueEnd = capacity
	// Else:
	//   0 <= queueBegin < capacity
	//   0 <= queueEnd < capacity
	//   If queueBegin == queueEnd, queue is full

	// Queue is *full* iff queueEnd == queueBegin (wraps around)
	// Queue is *empty* iff

	// Points to the first element. (== capacity if queue is empty)
	size_t queueBegin;
	// Points past the last element.
	size_t queueEnd;

	MutQueue() : _begin{nullptr}, capacity{0}, queueBegin{0}, queueEnd{0} {}
};

template <typename T>
const Bool isEmpty(const MutQueue<T>* q) {
	assert((q->queueBegin == q->capacity) == (q->queueEnd == q->capacity));
	return eq(q->queueBegin, q->capacity);
}

template <typename T>
const Opt<const T> popLeft(MutQueue<T>* q) {
	if (isEmpty(q))
		return none<const T>();
	else {
		assert(q->queueBegin < q->capacity && q->queueEnd < q->capacity);
		const T res = q->_begin[q->queueBegin];
		q->queueBegin++;
		if (q->queueBegin == q->capacity)
			q->queueBegin = 0;
		if (q->queueBegin == q->queueEnd) {
			// Now empty
			q->queueBegin = q->capacity;
			q->queueEnd = q->capacity;
			assert(isEmpty(q));
		}
		return some<const T>(res);
	}
}

template <typename T>
void pushRight(Arena* arena, MutQueue<T>* q, const T value) {
	if (q->queueBegin == q->queueEnd) {
		if (q->queueBegin == q->capacity) {
			// empty
			if (q->capacity == 0) {
				q->capacity = 8;
				q->_begin =  static_cast<T*>(alloc(arena, sizeof(T) * q->capacity));
			}
			q->queueBegin = 0;
			q->queueEnd = 1;
			initMemory<const T>(&q->_begin[0], value);
		} else {
			// full
			todo<void>("expand queue");
		}
	} else {
		// not full
		assert(q->queueBegin < q->capacity && q->queueEnd < q->capacity);
		initMemory<const T>(&q->_begin[q->queueEnd], value);
		q->queueEnd++;
		if (q->queueEnd == q->capacity)
			q->queueEnd = 0;
	}
}
