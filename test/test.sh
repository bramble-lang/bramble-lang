#!/bin/sh

function run_test() {
    test=$1
    rm -f ./target/*

    # cargo run -- -i ./src/${test}.br -o ./target/output.asm > ./target/stdout
    ../target/debug/braid-lang -p linux -i ./src/${test}.br -o ./target/output.asm > ./target/stdout
    if [ $? -eq 0 ]
    then
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

        if [[ $built -eq 1 ]]; then
            ./target/output >> ./target/stdout
        else 
            echo "Build failed"
        fi
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

cargo build
if [ $? -eq 0 ]
then
    run_test "operations/arithmetic"
    run_test "operations/boolean"
    run_test "simple"
    run_test "simple_coroutine"
    run_test "coroutine_too_many_calls"
    run_test "coroutine_infinite"
    run_test "boolean"
    run_test "comparison"
    run_test "boolean_complex"
    run_test "boolean_coroutine"
    run_test "coroutine_two"
    run_test "exp_block/simple"
    run_test "exp_block/multi_line"
    run_test "exp_block/within_exp"
    run_test "if_exp/if"
    run_test "if_exp/if_block"
    run_test "if_exp/if_as_conditional"
    run_test "binding/recursive"
    run_test "binding/undeclared"
    run_test "binding/before_declaration"
    run_test "binding/redeclare"
    run_test "binding/mutation"
    run_test "fn/recurse"
    run_test "scope/out_of_scope"
    run_test "parser/missing_type"

    run_test "string_literals/basic"
    run_test "string_literals/with_functions"
    run_test "string_literals/with_coroutines"

    run_test "structs/basic"
    run_test "structs/expr_block"
    run_test "structs/expr_block_result"
    run_test "structs/copying"
    run_test "structs/copying_simple"
    run_test "structs/big"
    run_test "structs/nested"
    run_test "structs/nested_simple"
    run_test "structs/nested_deep"
    run_test "structs/passing"
    run_test "structs/pass_struct_expr"
    run_test "structs/return_nested"
    run_test "structs/return_struct_expr"
    run_test "structs/returning"
    run_test "structs/coroutine_1"
    run_test "structs/coroutine_2"
    run_test "structs/coroutine_3"
    run_test "structs/coroutine_4"
    run_test "structs/errors/field"
    run_test "structs/errors/missing_member"
    run_test "structs/errors/type_mismatch"

    run_test "modules/simple"
    run_test "modules/coroutine"
    run_test "modules/nested"
    run_test "modules/struct"
    run_test "modules/return"
fi
