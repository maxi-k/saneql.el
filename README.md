# saneql.el

Emacs major mode for [saneql](https://github.com/neumannt/saneql) files.

## Setup

1. Make sure that the saneql binary is on your emacs-known `PATH` (var `saneql-compile-command`) 
2. Make sure an sqlite / database binary (var `saneql-sql-exec-command`) is on you path
3. Set the `saneql-db` using a [file variable](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html)
   For Example:

``` saneql
-- -*- saneql-db: ~/dev/tpch-sqlite/tpch.db -*-
-- the above line sets the input database for evaluation
let base := '1993-10-01',
let basedate(add := '+0 seconds') := foreigncall('date', date, {base, add}),
orders
.filter(o_orderdate >= basedate() && o_orderdate < basedate(add := '+3 months'))
.join(customer, c_custkey=o_custkey)
.groupby({c_custkey, c_name, c_acctbal, c_phone, n_name, c_address, c_comment}, {revenue:=sum(l_extendedprice * (1 - l_discount))})
.orderby({c_name.desc()}, limit:=20)
.project({c_custkey, c_name, revenue})
```

