#!/bin/sh

run() {
    # test=$1
    rm -rf ./target
    mkdir -p ./target

    built=0

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "Compiling"
        cargo run -- -p linux --input "./import/shared" -o ./target/shared.obj --manifest
        cargo run -- -p linux --import ./target/shared.manifest --input "./import/indirect" -o ./target/indirect.obj --manifest
        cargo run -- -p linux --input "../braid/std" -o ./target/std.obj --manifest
        cargo run -- -p linux --import ./target/std.manifest,./target/shared.manifest,./target/indirect.manifest "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        nasm -g -f elf64 ../braid/linux/llvm/std/input.asm -l ./target/std_input.lst -o ./target/std_input.obj > assembler.log
        gcc -no-pie -fno-pie -w ./target/indirect.obj ./target/shared.obj ./target/std.obj ./target/std_input.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Compiling"
        cargo run -- -p machos --input "./import/shared" -o ./target/shared.obj --manifest
        cargo run -- -p machos --import ./target/shared.manifest --input "./import/indirect" -o ./target/indirect.obj --manifest
        cargo run -- -p machos --input "../braid/std" -o ./target/std.obj --manifest
        cargo run -- -p machos --import ./target/std.manifest,./target/shared.manifest,./target/indirect.manifest "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        nasm -g -f macho64 ../braid/macho64/llvm/std/input.asm -l ./target/std_input.lst -o ./target/std_input.obj > assembler.log
        gcc -w ./target/indirect.obj ./target/shared.obj ./target/std.obj ./target/std_input.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    else
        echo "Unknown OS: ${OSTYPE}"
    fi
}

echo "Compile with LLVM ${@}"
run "$@"
