![Rust](https://github.com/erichgess/braid-lang/workflows/Rust/badge.svg)
![Braid](https://github.com/erichgess/braid-lang/workflows/Braid/badge.svg)

# Braid Language
The foundation steps for a programming language.

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
1. Install LLVM: https://llvm.org/
2. NASM needs to be installed in order to compile the Input functions.
3. Install Rust and Cargo: https://www.rust-lang.org/tools/install
4. Braid needs the `nightly` toolchain to build, so after Rust has been installed
go to the Braid repo directory and run:
```
rustup default nightly
```
5. Build Braid by running:
```
cargo build
```

## Compiling a Braid File
The `./test/make-llvm.sh` script file will compile a given Braid project.

### Compiler Options
- `input`: this is the location of a file or project directory that will be compiled.
- `import`: specifies one or more Braid library projects to import for compiling the `input` project.
- `output`: The name of the output binary file.
- `emit`: Set this value to `llvm-ir` to emit the LLVM IR code as part of compilation.
- `manifest`: This will generate a manifest file, which will be used for importing the items defined in `input` project into other projects.
- `log`: Set to `debug|info|error` to turn on logging during compilation.  This is primarily used during compiler development.
- `trace-lexer`: When enabled, the compiler will emit a describing how an input source code file is converted from text to tokens.
- `trace-parser`: This will enable tracing during the parsing. Tracing allows you to see how the Parser stage is converting the input source code into an AST.
- `trace-semantic-node`: When enabled, the compiler will emit a trace of the semantic analysis of the input source code.
- `trace-canonization`: When enabled, the compiler will emit a trace showing the canonization of each node in the AST.
- `trace-type-resolver`: When enabled, the copmiler will emit a trace showing how each line of code in the input source code is assigned a type.

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
