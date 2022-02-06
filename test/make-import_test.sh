#!/bin/sh

run() {
    # test=$1
    rm -rf ./target
    mkdir -p ./target

    built=0

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "Compiling"
        ../target/debug/braidc -p linux --input "./import/shared" -o ./target/shared.obj --manifest
        ../target/debug/braidc -p linux --import ./target/shared.manifest --input "./import/indirect" -o ./target/indirect.obj --manifest
        ../target/debug/braidc -p linux --input "../braid/std" -o ./target/std.obj --manifest
        ../target/debug/braidc -p linux --import ./target/std.manifest,./target/shared.manifest,./target/indirect.manifest "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        gcc -no-pie -fno-pie -w ./target/indirect.obj ./target/shared.obj ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Compiling"
        ../target/debug/braidc -p machos --input "./import/shared" -o ./target/shared.obj --manifest
        ../target/debug/braidc -p machos --import ./target/shared.manifest --input "./import/indirect" -o ./target/indirect.obj --manifest
        ../target/debug/braidc -p machos --input "../braid/std" -o ./target/std.obj --manifest
        ../target/debug/braidc -p machos --import ./target/std.manifest,./target/shared.manifest,./target/indirect.manifest "$@" -o ./target/output.obj
        echo ""
        echo "Assembling"
        gcc -w ./target/indirect.obj ./target/shared.obj ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2>&1 > gcc.log
        built=1
    else
        echo "Unknown OS: ${OSTYPE}"
    fi
}

echo "Compile with LLVM ${@}"
run "$@"
