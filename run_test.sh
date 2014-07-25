#!/bin/bash

if [ "$#" != 3 ]; then
    echo "usage $0 <block size> <processes count> <iteration count>" && exit 1
fi

B_SIZE=$1
P_COUNT=$2
I_COUNT=$3

erl +K true -pa ebin -pa deps/*/ebin -eval "{ok, P, F} = porttest:test_suite_new($P_COUNT, $B_SIZE), porttest:run_test_suite(P, F, $I_COUNT), init:stop(0)." -noshell || exit 1

echo "OK"
