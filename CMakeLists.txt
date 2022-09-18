cmake_minimum_required(VERSION 3.14)

project(BattleShips LANGUAGES Fortran)

if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

add_compile_options(
"$<$<Fortran_COMPILER_ID:GNU>:-fimplicit-none;-Wall>"
"$<$<Fortran_COMPILER_ID:Intel,IntelLLVM>:-implicitnone;-warn>"
)

add_executable(battleships battleships.f90)
target_include_directories(battleships PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET battleships PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

install(TARGETS battleships)
