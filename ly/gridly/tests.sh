#!/bin/bash

## This test script is here only temporarily. It will be removed once
## we have a proper testing infrastructure

TESTS="\
usage-examples/example.ly \
usage-examples/multi-file/main.ily \
usage-examples/multi-file/parts/alto-I.ily \
usage-examples/multi-file/parts/basso-I.ily \
usage-examples/multi-file/parts/soprano-I.ily \
usage-examples/multi-file/parts/tenore-I.ily"

LILYPOND="lilypond -I ../.. -I .."

for TEST in $TESTS
do
    $LILYPOND $TEST
done
