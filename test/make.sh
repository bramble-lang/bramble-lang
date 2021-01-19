#!/bin/sh

function run() {
    # test=$1
    rm -f ./target/*
    echo "Compiling"
    cargo run -- "$@" -o ./target/output.asm
    echo ""
    echo "Assembling"

    built=0

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        nasm -g -f elf64 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > assembler.log
        gcc -w ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        nasm -g -f macho64 ./target/output.asm -l ./target/output.lst -o ./target/output.obj > assembler.log
        gcc -w ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    else
        echo "Unknown OS: ${OSTYPE}"
    fi

    echo ""

    if [[ $built -eq 1 ]]; then
        echo "Running"
        ./target/output
    else
        echo "Build Failed"
    fi
}

echo "Compile ${1}"
run "$@"
