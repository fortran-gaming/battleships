# battleships in Fotran

Fortran 2008 Battleship game, original code from https://github.com/surftheseawing/battleships.
We observed the original code could be modernized to work on all modern Fortran compilers.

The changes we did include:

* module level `implicit none`
* use format `i0`
* use `iso_fortran_env`
* use `read(..., iostat=)` to handle unexpected input
* use escape characters instead of system calls to clear screen

Tested on compilers including:

* GCC gfortran
* Intel oneAPI
* Flang
* NVidia HPC SDK
* AOCC AMD Optimizing Compilers
