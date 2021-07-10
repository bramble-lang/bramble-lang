#!/bin/sh

#   Runs all the Braid integration tests.
#
#   Each test consists of a Braid program, an expected output, and, sometimes, an input file.
#   This script will compile the Braid program, run it, and compare the output with the expected
#   output, if the two match then the test passes.
#
#   To add an integration test, simply write a Braid program and save it to `./test/src/{path}/{test}.br`
#   and put the expected output in `./test/src/{path}/{test}.out`.  This script will automatically
#   find and execute the test (based on the existance of the .out file).
#
#   If a test requires input, then put each input, in order, in a `.in` file with each input on
#   a separate line.

num_tests=0
num_pass=0

run_test() {
    rm -rf ./target
    mkdir -p ./target
    test=$1
    built=1

    bash ./make-import_test.sh --input ./import/$test 2> ./target/compiler.stderr 1> ./target/compiler.stdout
    echo "Running"
    ./target/output > ./target/stdout

    result=$(diff ./target/stdout ./import/${test}.out)
    if [ $? -eq 0 ]
    then
        ((num_pass=num_pass+1))
        echo "${test}: Pass"
    else
        echo "${test}: Fail"
        echo ${result}
        echo ""
        echo "Actual:"
        cat ./target/stdout
        echo "\n-------------"
        echo "Expected:"
        cat ./import/${test}.out
        echo "\n-------------"
    fi
}

cargo build --release
if [ $? -eq 0 ]
then
    start_time=$SECONDS

    mkdir -p ./target

    tests=`find ./import | grep "\.out" | grep -v "coroutine" | sort | sed 's/\.\/import\/\(.*\)\.out/\1/'`
    for test in ${tests[@]}; do
        ((num_tests=num_tests+1))
        run_test $test
    done

    stop_time=$SECONDS
    duration=$(($stop_time-$start_time))
    echo ""
    echo "${num_pass}/${num_tests} Tests Passed in ${duration}secs"

    if [ ${num_pass} -ne ${num_tests} ]; then
        exit 1
    fi
fi
