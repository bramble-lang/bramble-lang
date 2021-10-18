#!/bin/sh

# Runs the entire suite of Braid integration tests:
#  1. The Braid language test suite
#  2. The Braid project import test suitee
#  3. The Braid fuzzy syntax correctness test suite

cd ./test
ret=0

./test-llvm.sh
if [[ $? -ne 0 ]]; then 
    ret=1
fi

./test-imports.sh
if [[ $? -ne 0 ]]; then 
    ret=2
fi

./syntax-test.sh 25
if [[ $? -ne 0 ]]; then 
    ret=3
fi

exit $ret