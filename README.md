![Rust](https://github.com/erichgess/braid-lang/workflows/Rust/badge.svg)
![Braid](https://github.com/erichgess/braid-lang/workflows/Braid/badge.svg)

# Braid Language
The foundation steps for a programming language. More will be added here once the
language moves beyond the primordial stage.

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
#### Main Options
- `input`: this is the location of a file or project directory that will be 
compiled.
- `import`: specifies one or more Braid library projects to import for compiling
 the `input` project.
- `output`: The name of the output binary file.
- `manifest`: This will generate a manifest file, which will be used for 
importing the items defined in `input` project into other projects.
- `emit`: Set this value to `llvm-ir` to emit the LLVM IR code as part of 
compilation. This is useful for looking at how code you have written is being 
compiled, for investigation, debugging, or optimization.  It is also essential 
as an aid to working on the compiler itself, to verify that new language or 
compiler features are being correctly translated into LLVM IR.

#### Compiler Developer Options:
These options are primarily useful when directly working on the compiler itself. 
They allow you to gain insight into exactly what is happening in the compiler, 
how it is interpreting input source code, and what decisions it's making.
- `log`: Set to `debug|info|error` to turn on logging during compilation.  This 
is primarily used during compiler development.
- `trace-lexer`: When enabled, the compiler will emit a describing how an input 
source code file is converted from text to tokens.
- `trace-parser`: This will enable tracing during the parsing. Tracing allows 
you to see how the Parser stage is converting the input source code into an AST.
- `trace-semantic-node`: When enabled, the compiler will emit a trace of the 
semantic analysis of the input source code.
- `trace-canonization`: When enabled, the compiler will emit a trace showing the 
canonization of each node in the AST.
- `trace-type-resolver`: When enabled, the copmiler will emit a trace showing 
how each line of code in the input source code is assigned a type.

#### Compiler Tracing
Compiler traces provide a way to see exactly what the compiler is doing with a 
given input source code. This exists for debugging the compiler and validating 
in progress development. The output expects a lot of contextual knowledge and 
is not the most user friendly. One goal of Braid is to change that and generate 
an output that any user of the language can use to understand how their code is 
being converted into assembly.

All `trace-<stage>` options allow you to specify a filter so that only a subset 
of the input source file will emit tracing information, the format of the range 
option is: 
```
all - Trace the entire input file
: - Trace the entire input file
<line> - Trace only this line of the input file
:<before line> - Trace every line up to and including <before line>
<after line>: - Trace every line from <after line> on.
<from line>:<to line> - Trace every line between <from line> and <to line> 
(inclusive)
```

Currently, this does not specify a file, so it will apply the filter to each 
file within a project. It's primary purpose was for debugging new language and 
compiler features on single file test inputs. This will change soon.

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
