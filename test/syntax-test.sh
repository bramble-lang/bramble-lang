# Randomly generate syntactically correct code and test that braidc validates it.
if [ -z "$1" ]
  then
    echo "No argument supplied"
    exit 1
fi

echo "Random Syntax Check: Running $1 Iterations"
cargo build --release
COUNTER=0
until [ $COUNTER -ge $1 ]; do
    ../target/release/braid-gen 10 10 > /tmp/braid_test.br
    ../target/release/braidc --stage=parser --platform=linux -i /tmp/braid_test.br --output=./
    result=$?

    if [[ result -ne 0 ]]; then
        cat /tmp/braid_test.br
        exit 1
    fi
    let COUNTER+=1
done