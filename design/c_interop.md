# C Interop
## Problem
The Bramble programming language is very new and implementing all features from scratch
would be a giant pain, but I want to be able to perform complex tasks that would be 
common in a program (e.g. file IO) without having to take the time to implement such
a feature in Bramble.

## Solution
Use existing C functions for these tasks by building a C interop system in Bramble.

## Design
### Design Goals
Have an easy way of defining in a Bramble program that a function being called is a C function
and then, on compilation, Bramble links with that C function (and library) and directs all
calls to that function.

### ABIs
The ABI for calling C functions is platform dependent: on Linux and macOS it is System V and on
Windows it is vectorcall or MSFT Calling Convention.  See https://en.wikipedia.org/wiki/X86_calling_conventions#List_of_x86_calling_conventions. Since, I am focusing my development
on Linux, for now, I will start by implementing a solution using System V, but the design
should support switching the calling convention to MSFT.

### Bramble Requirements
In order for Bramble to be able to do interop with C, will need to add the following features:
1. Pointers and Mutable Pointers, so be able to at least pass a `&x` to a function.

#### Research
Rust C Interop: https://rust-embedded.github.io/book/interoperability/c-with-rust.html