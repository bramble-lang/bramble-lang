#!/bin/sh

function run_test() {
    test=$1
    rm ./target/*
    cargo run -- -i ./src/${test}.br -o ./target/output.asm 2>&1 > compiler.log
    nasm -g -f elf32 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > assembler.log
    gcc -w ./target/output.obj macro.c -g -o ./target/output -m32 2>&1 > gcc.log

    ./target/output > ./target/stdout

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