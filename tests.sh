#!/bin/sh

# Runs the entire suite of Braid tests and return the number of test suites that
# failed.  So, 0 will correspond, correctly, to success:
#  0. Run the Unit Tests
#  1. The Braid language test suite
#  2. The Braid project import test suitee
#  3. The Braid fuzzy syntax correctness test suite

ret=0
cargo test --release
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

cd ./test

./test-llvm.sh
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

./test-imports.sh
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

./syntax-test.sh 25
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

exit $ret