![Rust](https://github.com/erichgess/braid-lang/workflows/Rust/badge.svg)

# Braid Language
A language I am creating as a learning tool.

## Building the Compiler
### Docker
To aid with development on other machines, there is a Docker configuration that
will spin up a container and build Braid and run the unit and Braid tests. The
container is configured to use Fedora as the build environment.

To run Braid using the Docker container, you just need to run the `start-docker.sh`
script:

```
start-docker.sh
```

This script will default to a container running Fedora, but you can use the
`-i` argument to specify a different image.  Options include `ubuntu` or `alpine`.
This script will, if necessary, build the appropriate Docker image and will then
start the container, mount the Braid compiler source code to `/work/braid-lang` and
then run an interactive shell on the container.

With the interactive shell, you will be able to compile and use the Braid compiler
under a Linux OS. And any changes that you make to the Braid source code on the host
machine will be reflected in the container, thus creating an effective dev environment
for any host that can run docker.

### Locally
1. You will need to install Rust and Cargo: https://www.rust-lang.org/tools/install

2. Braid needs the `nightly` toolchain to build, so after Rust has been installed
go to the Braid repo directory and run:
```
rustup default nightly
```
3. Build Braid by running:
```
cargo build
```

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
