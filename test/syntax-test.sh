#! /bin/sh
# Randomly generate syntactically correct code and test that braidc validates it.
if [ -z "$1" ]
  then
    echo "No argument supplied"
    exit 1
fi

start_time=$(date +%s)
echo "Random Syntax Check: Running $1 Iterations"
cargo build

COUNTER=0
until [ $COUNTER -ge $1 ]; do
    let COUNTER+=1
    echo $COUNTER

    rm /tmp/braid_test.br
    ../target/debug/braid-gen 8 20 > /tmp/braid_test.br
    ../target/debug/braidc --stage=parser --platform=linux -i /tmp/braid_test.br --output=./
    result=$?

    if [[ result -ne 0 ]]; then
        end_time=$(date +%s)
        elapsed=$(( end_time - start_time ))
        cat /tmp/braid_test.br
        exit 1
    fi
done
end_time=$(date +%s)
elapsed=$(( end_time - start_time ))
echo "Elapsed: ${elapsed}"