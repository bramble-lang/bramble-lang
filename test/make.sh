#!/bin/sh

function run() {
    test=$1
    rm -f ./target/*
    echo "Compiling"
    cargo run -- --trace-parser 7:8 -i ${test} -o ./target/output.asm
    echo ""
    echo "Assembling"

    nasm -g -f elf32 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > assembler.log
    gcc -w ./target/output.obj -g -o ./target/output -m32 2>&1 > gcc.log

    echo ""
    echo "Running"
    ./target/output
}

echo "Compile ${1}"
run $1
