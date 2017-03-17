#!/bin/bash

# echo "Eventual 1"
# /usr/lib/erlang/bin/erl -noshell -s testcase1 main3 eventual 1 -s erlang halt

 echo "Eventual 3"
 /usr/lib/erlang/bin/erl -noshell -s testcases main3 eventual 3 -s erlang halt

# echo "Eventual 5"
# /usr/lib/erlang/bin/erl -noshell -s testcase1 main3 eventual 5 -s erlang halt

# echo "Linearizability 1"
# /usr/lib/erlang/bin/erl -noshell -s testcase1 main3 linearizability 1 -s erlang halt

 echo "Linearizability 3"
 /usr/lib/erlang/bin/erl -noshell -s testcases main3 linearizability 3 -s erlang halt

# echo "Linearizability 5"
# /usr/lib/erlang/bin/erl -noshell -s testcase1 main3 linearizability 5 -s erlang halt
