#!/bin/bash

echo "Eventual 1"
/usr/lib/erlang/bin/erl -noshell -s testcases main eventual 1 -s erlang halt

echo "Eventual 3"
/usr/lib/erlang/bin/erl -noshell -s testcases main eventual 3 -s erlang halt

echo "Eventual 5"
/usr/lib/erlang/bin/erl -noshell -s testcases main eventual 5 -s erlang halt

echo "Linearizability 1"
/usr/lib/erlang/bin/erl -noshell -s testcases main linearizability 1 -s erlang halt

echo "Linearizability 3"
/usr/lib/erlang/bin/erl -noshell -s testcases main linearizability 3 -s erlang halt

echo "Linearizability 5"
/usr/lib/erlang/bin/erl -noshell -s testcases main linearizability 5 -s erlang halt
