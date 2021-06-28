#!/bin/sh

run() {
    # test=$1
    rm -rf ./target
    mkdir -p ./target

    built=0

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "Compiling"
        cargo run -- --llvm -p linux --input "../braid/std" -o ./target/std.obj --manifest
        cargo run -- --llvm -p linux "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        nasm -g -f elf64 ../braid/linux/llvm/std/input.asm -l ./target/std_input.lst -o ./target/std_input.obj > assembler.log
        gcc -no-pie -fno-pie -w ./target/std.obj ./target/std_input.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Compiling"
        cargo run -- --llvm -p machos --input "../braid/std" -o ./target/std.obj --manifest
        cargo run -- --llvm -p machos "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        nasm -g -f macho64 ../braid/macho64/llvm/std/input.asm -l ./target/std_input.lst -o ./target/std_input.obj > assembler.log
        gcc -w ./target/std.obj ./target/std_input.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
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

echo "Compile with LLVM ${@}"
run "$@"
