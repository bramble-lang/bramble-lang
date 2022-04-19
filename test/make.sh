#!/bin/sh

run() {
	# test=$1
	rm -rf ./target
	mkdir -p ./target
	target=--release

	if [[ $BRAMBLE_TARGET == "debug" ]]; then
		target=
	elif [[ $BRAMBLE_TARGET == "release" ]]; then
		target=--release
	fi

	echo $target

	built=0

	if [[ $OSTYPE == "linux-gnu"* ]]; then
		echo "Compiling"
		cargo run $target --bin bramblec -- --mir-beta --llvm --emit llvm-ir -p linux --input "../bramble/std" -o ./target/std.obj --manifest
		cargo run $target --bin bramblec -- --llvm -p linux --import ./target/std.manifest "$@" -o ./target/output.obj
		echo ""
		echo "Assembling"
		gcc -no-pie -fno-pie -w ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2>&1 >gcc.log
		built=1
	elif [[ $OSTYPE == "darwin"* ]]; then
		echo "Compiling"
		cargo run --$target --bin bramblec -- --llvm -p machos --input "../bramble/std" "$@" -o ./target/std.obj --manifest
		cargo run --$target --bin bramblec -- --llvm -p machos --import ./target/std.manifest "$@" -o ./target/output.obj
		echo ""
		echo "Assembling"
		gcc -w ./target/std.obj ./target/output.obj -g -o ./target/output -m64 2>&1 >gcc.log
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
