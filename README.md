# Braid Language
A language I am creating as a learning tool.

## Building the Compiler
1. You will need to install Rust and Cargo: https://www.rust-lang.org/tools/install

## Compiling a Braid File
Right now, the Braid compiler only generates x64 assembly output. There are two
steps that have to be done after to arrive at an executable:

1. Run the `nasm` assembler, which converts the assembly code into an object file.
2. Run `gcc` to do the final compilation and linking.  This will output the actual
executable.

To assist with this, I have written a `make.sh` script which will compile and input
Braid source file, then run `nasm` and `gcc` for you.

`make.sh` is located in `./test` and can be used to build a Braid file with the
following command:

```
./make.sh -i <path to source code>
```

## Testing
There are two sets of tests for Braid
1. Unit tests - These are all the unit tests which test that each component, type, and
function in the compiler code base works correctly.
2. Braid test - These are a suite of Braid code files which are used to validate that
the actual compiled Braid code works correctly.  Each test consists of a pair of files:
a Braid file (extension `.br`) and an expected output file (extension `.out`).  Some
Braid files contain intentional errors to validate that the compilers syntactic and
semantic analysis is working, those `.out` files will contain compiler error messages.

### Running Tests
#### Unit Tests
From the project directory run:

 ```
 cargo test
 ```

This will run all unit tests and output the results to teh console.

#### Braid Tests
From within the `./test` directory, run: 
```
./test.sh
```

This will run through every Braid test and test that the source code compiles and 
executes correctly.