#!/bin/sh

# Runs the entire suite of Bramble tests and return the number of test suites that
# failed.  So, 0 will correspond, correctly, to success:
#  0. Run the Unit Tests
#  1. The Bramble language test suite
#  2. The Bramble project import test suitee
#  3. The Bramble fuzzy syntax correctness test suite

ret=0
cargo test --release
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

cd ./test

./test.sh
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

./test-imports.sh
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

./test-syntax.sh 25
if [[ $? -ne 0 ]]; then 
    ret=$(($ret + 1))
fi

exit $ret