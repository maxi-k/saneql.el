# saneql.el

Emacs major mode for [saneql](https://github.com/neumannt/saneql) files.

## Setup

1. Make sure that the saneql binary is on your emacs-known `PATH` (var `saneql-compile-command`) 
2. Make sure an sqlite / database binary (var `saneql-sql-exec-command`) is on you path
3. Press C-c C-c (per default) in a buffer to run `saneql-compile-and-run-buffer`. 
   If `saneql-db` is not set yet, this will query for a connection type and string.
4. To get a TPC-H sqlite database for testing, you can use [TPCH-sqlite](https://github.com/lovasoa/TPCH-sqlite).
