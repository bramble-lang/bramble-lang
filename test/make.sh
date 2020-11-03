#!/bin/sh

function run() {
    test=$1
    rm -f ./target/*
    cargo run -- -i ${test} -o ./target/output.asm 2>&1 > compiler.log
    nasm -g -f elf32 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > assembler.log
    gcc -w ./target/output.obj macro.c -g -o ./target/output -m32 2>&1 > gcc.log

    ./target/output
}

echo "Compile ${1}"
run $1