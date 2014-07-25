#!/bin/bash

erl +a 8192 +A 100 +K true -pa ebin -pa deps/*/ebin -eval "loadport:run($1, $2, $3)." || exit 1

echo "OK"
