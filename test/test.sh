#!/bin/sh

#   Runs all the Bramble integration tests.
#
#   Each test consists of a Bramble program, an expected output, and, sometimes, an input file.
#   This script will compile the Bramble program, run it, and compare the output with the expected
#   output, if the two match then the test passes.
#
#   To add an integration test, simply write a Bramble program and save it to `./test/src/{path}/{test}.br`
#   and put the expected output in `./test/src/{path}/{test}.out`.  This script will automatically
#   find and execute the test (based on the existance of the .out file).
#
#   If a test requires input, then put each input, in order, in a `.in` file with each input on
#   a separate line.

num_tests=0
num_pass=0

std_dir=./target/std
build_dir=./target/build
target=debug

build_std() {
    rm -rf ${std_dir}
    mkdir -p ${std_dir}

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        ../target/${target}/bramblec --llvm -p linux -i ../bramble/std -o ${std_dir}/std.obj --manifest > ${std_dir}/stdout 2> /dev/null
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ../target/${target}/bramblec --llvm -p machos -i ../bramble/std -o ${std_dir}/std.obj --manifest > ${std_dir}/stdout 2> /dev/null
    fi
    mv ./target/std.manifest ./target/std/.
}

run_test() {
    rm -rf ${build_dir}
    mkdir -p ${build_dir}
    test=$1
    input="./src/${test}.in"
    built=1

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        ../target/${target}/bramblec --llvm -p linux --import ${std_dir}/std.manifest --json-trace -i ./src/${test} -o ${build_dir}/output.obj > ${build_dir}/stdout 2> /dev/null
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ../target/${target}/bramblec --llvm -p machos --import ${std_dir}/std.manifest --json-trace -i ./src/${test} -o ${build_dir}/output.obj > ${build_dir}/stdout 2> /dev/null
    fi

    # If there were no compilation errors then run the assembler and linker
    if [ -f "${build_dir}/output.obj" ]
    then
        if [[ "$OSTYPE" == "linux-gnu"* ]]; then
            gcc -no-pie -fno-pie -w ${std_dir}/std.obj  ${build_dir}/output.obj -g -o ${build_dir}/output -m64 2>&1 > gcc.log
            built=$?
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            gcc -w ${std_dir}/std.obj ${build_dir}/output.obj -g -o ${build_dir}/output -m64 2> ${build_dir}/stdout
            built=$?
        else
            # If we can't figure out the OS, then just try the Linux build steps
            gcc -no-pie -fno-pie -w ${std_dir}/std.obj  ${build_dir}/output.obj -g -o ${build_dir}/output -m64 2>&1 > gcc.log
            built=$?
        fi
    
        if [[ $built -eq 0 ]]; then
            if [[ -f $input ]]; then
                timeout 5s "${build_dir}/output" < $input >> ${build_dir}/stdout
            else
                timeout 5s "${build_dir}/output" >> ${build_dir}/stdout
            fi
            if [[ $? -eq 124 ]]; then
                echo "Timed out"
                echo "Timed out" >> ${build_dir}/stdout
            fi
        else 
            echo "Build failed"
        fi
    fi

    # Test the output of the compiled binary
    result=$(diff ${build_dir}/stdout ./src/${test}.out)
    if [ $? -eq 0 ]
    then
        # Test the trace output from the compiler
        result=$(diff ${build_dir}/../trace.json ./src/${test}.json)
        if [ $? -eq 0 ]
        then
            ((num_pass=num_pass+1))
            echo "${test} Pass"
        else
            echo "${test} Trace Test: Fail"
            echo "${result}"
        fi
    else
        echo "${test}: Fail"
        echo ${result}
        echo ""
        echo "Actual:"
        cat ${build_dir}/stdout
        echo "\n-------------"
        echo "Expected:"
        cat ./src/${test}.out
        echo "\n-------------"
    fi
}

run_fail_test() {
    rm -rf ./target
    mkdir -p ./target
    test=$1
    input="./src/${test}.in"

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        ../target/${target}/bramblec --llvm -p linux -i ../bramble/std -o ./target/std.obj > ./target/stdout 2> /dev/null
        ../target/${target}/bramblec --llvm -p linux -i ./src/${test} -o ./target/output.obj > ./target/stdout 2> /dev/null
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ../target/${target}/bramblec --llvm -p machos -i ../bramble/std -o ./target/std.obj > ./target/stdout 2> /dev/null
        ../target/${target}/bramblec --llvm -p machos -i ./src/${test} -o ./target/output.obj > ./target/stdout 2> /dev/null
    fi

    # If there were no compilation errors then run the assembler and linker
    if [ -f "./target/output.obj" ]
    then
        if [[ "$OSTYPE" == "linux-gnu"* ]]; then
            gcc -no-pie -fno-pie -w ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
            built=$?
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            gcc -w ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2> ./target/stdout
            built=$?
        else
            # If we can't figure out the OS, then just try the Linux build steps
            nasm -g -f elf64 ../bramble/linux/llvm/std/io.asm -l ./target/std_io_llvm.lst -o ./target/std_io_llvm.obj > assembler.log
            gcc -no-pie -fno-pie -w ./target/std_io_llvm.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
            built=$?
        fi

        if [[ $built -ne 0 ]]; then
            ((num_pass=num_pass+1))
            echo "${test}: Pass"
        else 
            echo "${test}: Fail"
        fi
    fi
}

if [[ "${target}" == "debug" ]]; then
    cargo build
else
    cargo build --${target}
fi

if [ $? -eq 0 ]
then
    start_time=$SECONDS

    mkdir -p ./target

    echo "Building STD Library"
    build_std

    echo "Running Tests"
    tests=`find ./src | grep "\.out" | grep -v "coroutine" | sort | sed 's/\.\/src\/\(.*\)\.out/\1/'`
    for test in ${tests[@]}; do
        ((num_tests=num_tests+1))
        run_test $test
    done

    echo ""
    echo "Test Failure Cases"
    tests=`find ./src | grep "\.fail" | grep -v "coroutine" | sort | sed 's/\.\/src\/\(.*\)\.fail/\1/'`
    for test in ${tests[@]}; do
        ((num_tests=num_tests+1))
        run_fail_test $test
    done

    stop_time=$SECONDS
    duration=$(($stop_time-$start_time))
    echo ""
    echo "${num_pass}/${num_tests} Tests Passed in ${duration}secs"

    if [ ${num_pass} -ne ${num_tests} ]; then
        exit 1
    fi
fi
