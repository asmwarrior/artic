# Artic

Colder, and cooler. This is the redesigned version of Artic, whose goal is to provide a language with the following features:

- [X] Scala-like syntax
- [ ] Type system which supports polymorphism and overloading (with an associated inference algorithm, e.g. W<sup>o</sup>)
- [ ] AST-level optimizations, available through annotations in the code

## Building

A compiler that supports C++14 and CMake are required to build the project. Use the following commands to build the program:

    mkdir build
    cd build
    cmake-gui ..
    make -j

## Syntax

The syntax is as follows:

```scala
def x = 1
var (a, b) = (1, 2)
def fun(x, y) = {
   x + y
}
```