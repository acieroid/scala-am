This folder contains benchmark programs for a concurrent Scheme.
Benchmarks are present for different flavours of concurrency:
* actors,
* concurrent threads with locks,
* futures with atomic variables (atoms).

For futures/threads, the folder 'variations' contains several benchmark programs, each of which is written
multiple times using a different number of futures resp. threads. Some of benchmarks for futures
are based on/rewritten from the benchmarks from threads.