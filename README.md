# battleships in Fotran

Fortran 2008 Battleship game, original code from https://github.com/surftheseawing/battleships.
We observed the original code could be modernized to work on all modern Fortran compilers.

The changes we did include:

* module level `implicit none`
* use format `i0`
* use `iso_fortran_env`
* use `read(..., iostat=)` to handle unexpected input
* use escape characters instead of system calls to clear screen
* use standrd sleep() implementation we created in scivision/fortran-sleep
* break code up into source files for ease of devel

Tested on compilers including:

* GCC gfortran
* Intel oneAPI
* Flang
* NVidia HPC SDK
* AOCC AMD Optimizing Compilers

## Build

CMake or FPM (Fortran Package Manager) can quickly build Battleships:

```sh
cmake -B build
cmake --build build

build/battleships  # runs game
```

OR

```sh
fpm run
```
