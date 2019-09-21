## Blocking I/O and Threads

Currently the number of threads is constant through the whole application.
This means that when there's a blocking I/O operation,
there aren't as many threads doing actual work as there should be.

This isn't just a performance problem, it could cause an application to deadlock;
say it launches a bunch of tasks to synchronously wait on something and another task to do it.
It might use up all its threads on the waiting task and it would never be done.

To fix this, we should increase the number of threads as appropriate.
`global-ctx` should have the following counts:
* `n-blocked-threads`
* `n-threads`
* (Already existing: `n-live-threads`)

There is also `n-threads-desired` which is `n-hardware-processors + n-blocked-threads` --
because we want `n-hardware-processors` threads doing work on the CPU.

Any time there is a blocking I/O operation, we should call `begin-blocking-io` before and `end-blocking-io` after.

`begin-blocking-io` should increment `n-blocked-threads`.
And if `n-threads < n-threads-desired`, it should create a new one.

`end-blocking-io` only needs to decrement `n-blocked-threads`.

The extra thread we created should clean up itself --
at the beginning of `thread-function-recur` a thread should always check if `n-threads > n-threads-desired`,
and if so, decrement `n-threads` and exit.

### Challenges

* The changing number of threads interacting with `n-live-threads` and GC.
* Locking so we don't have all threads simultaneously seeing `n-threads > n-threads-desired`
  and all shutting down at once.
* Performance: if threads immediately exit once `n-threads > n-threads-desired`,
  we may soon need to launch a new thread again.
  Instead a thread could sleep for a second and wait on `new-thread-needed-condition`
  which should be triggered at every call to `begin-blocking-io`.
  This may or may not improve performance so this should wait until performance tests are available.
* Also consider the opposite:
  reducing the number of threads below `n-hardware-processors` if many of them have nothing to do.
