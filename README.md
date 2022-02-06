![Rust](https://github.com/erichgess/braid-lang/workflows/Rust/badge.svg)
![Braid](https://github.com/erichgess/braid-lang/workflows/Braid/badge.svg)

# Braid Language
The foundation steps for a programming language. More will be added here once the
language moves beyond the primordial stage.

## Building the Compiler
### Locally
1. Install LLVM: https://llvm.org/
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


## Compiling a Braid File
The `./test/make.sh` script file will compile a given Braid project.

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
- `trace`: Enables transparency tracing of all actions taken by the compiler and
connects those actions back to the specific span(s) in the input source code that
led to those actions.

## Testing
There are two sets of tests for Braid
1. Unit tests - These are all the unit tests which test that each component, type, and
function in the compiler code base works correctly.
2. Braid test - These are a suite of Braid code files which are used to validate that
the actual compiled Braid code works correctly.  Each test consists of a pair of files:
a Braid file (extension `.br`) and an expected output file (extension `.out`).  Some
Braid files contain intentional errors to validate that the compilers syntactic and
semantic analysis is working, those `.out` files will contain compiler error messages.
3. Syntax Fuzz Tests - this is a tool which randomly generates syntactically valid
(but not semantically valid) source code and has `braidc` perform syntax analysis
on the randomly generated code.  The code should pass the syntax analysis.  This
test tool is meant to find all the weird and difficult combinations of code that
could be written and that is still correct and make sure the compiler correctly
parses that code.
4. Import Tests - this is a small set of tests to verify that the compiler can
properly import external Braid projects.

### Running Tests
#### Unit Tests
From the project directory run:

 ```
 cargo test
 ```

This will run all unit tests and output the results to the console.

#### Braid Tests
From within the `./test` directory, run: 
```
./test.sh
```

This will run through every Braid test and test that the source code compiles and 
executes correctly.

#### Syntax Fuzz Test
From within the `./test` directory, run:
```
./test-syntax.sh <Repititions>
```

This will run the syntax fuzz test tool for `<Repititions>` repititions.

#### Import Test
From within `./test` run:
```
./test-imports.sh
```


## Project Layout
Directories and what they contain
- `braid` - This contains the implementations of `std` libraries.  Most of these are written in
Braid but some files are written in platform appropriate assembly.
- `design` - Design documents and notes for various features. These provide insight into how many
features were investigated and designed.  Currently, these are probably best viewed as a diary.  
There is no formal definition of what needs to be in these docs and many of them are a running log
of thoughts, ideas, and research; rather than an RFC or design doc meant to be reviewed by other
developers.
- `docker` - Docker setup files for spinning up a docker container to run the Braid compiler tests.
This is to allow for testing Braid on Linux while working on another OS.
- `src` - The compiler source code.
    - `bin` - the source code for the different Braid tool executables
    - `compiler` - the compiler source code itself
    - `diagnostics` - code related to transparency and diagnostic tooling for the
    tools and the compiler.
    - `project` - manages the concept of a Braid project consisting of one or more
    source code files and imports.
    - `.` - Code dealing with UI/UX interfacing.
- `test` - All the Braid integration tests and test scripts.  These are source code file and
projects, written in Braid, used to test that the compiler works correctly.  The tests are compiled
from Braid to the platform specific binary executable then run. The output from the compiled code is
compared against the expected output to verify if the test passed or not.
    - `scratch` - a directory for putting scratch Braid code that you are writing to
    do quick tests and experiments.  This directory is ignored by git.