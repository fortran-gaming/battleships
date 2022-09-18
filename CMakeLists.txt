cmake_minimum_required(VERSION 3.14)

project(BattleShips
LANGUAGES C Fortran
)

if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

add_compile_options(
"$<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall>"
"$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fimplicit-none;-Wall;-Wno-intrinsic-shadow>"
"$<$<COMPILE_LANG_AND_ID:C,IntelLLVM>:-Wall>"
"$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-warn>"
)

add_executable(battleships app/main.f90 src/battleships.f90 src/sleep.c src/misc.f90)
target_include_directories(battleships PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET battleships PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

install(TARGETS battleships)
