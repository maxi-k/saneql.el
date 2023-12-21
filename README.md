# saneql.el

Emacs major mode for [saneql](https://github.com/neumannt/saneql) files.

## Setup

1. Make sure that the saneql binary is on your emacs-known `PATH` (var `saneql-compile-command`) 
2. Make sure an `sqlite` / `duckdb` binary is on you path (var `saneql-[duckdb|sqlite]-default-binary`). 
   **Ideally**, use [duckdb-saneql](https://github.com/maxi-k/duckdb-saneql), compile it, put the resulting `duckdb` binary on your `PATH`.
3. Press C-c C-c (per default) in a buffer to run `saneql-compile-and-run-buffer`. 
   If `saneql-db` is not set yet, this will query for a connection type and string.
   
## Limitations
- The `sqlite` backend **only works with the TPCH schema**. 
  To get a TPC-H sqlite database for testing, you can use [TPCH-sqlite](https://github.com/lovasoa/TPCH-sqlite).
- To get the `duckdb` backend to work with arbitrary schemas, use a duckdb binary compiled with [duckdb-saneql](https://github.com/maxi-k/duckdb-saneql).

