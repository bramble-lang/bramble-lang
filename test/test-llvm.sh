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
    input="./src/${test}.in"

    # cargo run -- -i ./src/${test}.br -o ./target/output.asm > ./target/stdout
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        ../target/debug/braid-lang --llvm -p linux -i ./src/${test}.br -o ./target/output.obj > ./target/stdout
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ../target/debug/braid-lang --llvm -p machos -i ./src/${test}.br -o ./target/output.obj > ./target/stdout
    fi

    # If there were no compilation errors then run the assembler and linker
    if [ -f "./target/output.obj" ]
    then
        if [[ "$OSTYPE" == "linux-gnu"* ]]; then
            nasm -g -f elf64 ../braid/linux/llvm/std/io.asm -l ./target/std_io_llvm.lst -o ./target/std_io_llvm.obj > assembler.log
            gcc -no-pie -fno-pie -w ./target/std_io_llvm.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
            built=1
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            nasm -g -f macho64 ../braid/macho64/llvm/std/io.asm -l ./target/std_io_llvm.lst -o ./target/std_io_llvm.obj > assembler.log
            gcc -w ./target/std_io_llvm.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
            built=1
        else
            # If we can't figure out the OS, then just try the Linux build steps
            nasm -g -f elf64 ../braid/linux/llvm/std/io.asm -l ./target/std_io_llvm.lst -o ./target/std_io_llvm.obj > assembler.log
            gcc -no-pie -fno-pie -w ./target/std_io_llvm.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
            built=1
        fi

        if [[ $built -eq 1 ]]; then
            if [[ -f $input ]]; then
                timeout 5s "./target/output" < $input >> ./target/stdout
            else
                timeout 5s "./target/output" >> ./target/stdout
            fi
            if [[ $? -eq 124 ]]; then
             echo "Timed out"
             echo "Timed out" >> ./target/stdout
            fi
        else 
            echo "Build failed"
        fi
    fi

    result=$(diff ./target/stdout ./src/${test}.out)
    if [ $? -eq 0 ]
    then
        ((num_pass=num_pass+1))
        echo "${test}: Pass"
    else
        echo "${test}: Fail"
        echo ${result}
    fi
}

cargo build
if [ $? -eq 0 ]
then
    start_time=$SECONDS

    mkdir -p ./target

    tests=`find ./src | grep "\.out" | grep -v "coroutine" | sort | sed 's/\.\/src\/\(.*\)\.out/\1/'`
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
