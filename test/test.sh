#!/bin/sh

function run_test() {
    test=$1
    rm -f ./target/*

    cargo run -- -i ./src/${test}.br -o ./target/output.asm > ./target/stdout
    if [ $? -eq 0 ]
    then
        nasm -g -f elf32 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > ./target/assembler.log
        gcc -w ./target/output.obj macro.c -g -o ./target/output -m32 2>&1 > ./target/gcc.log

        ./target/output >> ./target/stdout
    fi

    result=$(diff ./target/stdout ./src/${test}.out)
    if [ $? -eq 0 ]
    then
        echo "${test}: Pass"
    else
        echo "${test}: Fail"
        echo ${result}
    fi
}

run_test "simple"
run_test "simple_coroutine"
run_test "boolean"
run_test "comparison"
run_test "boolean_complex"
run_test "boolean_coroutine"
run_test "coroutine_two"
run_test "if_exp/if"
run_test "if_exp/if_as_conditional"
run_test "binding/recursive"
run_test "binding/undeclared"
run_test "binding/before_declaration"
run_test "binding/redeclare"
run_test "fn/recurse"
