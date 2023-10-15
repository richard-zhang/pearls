cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(example)



add_executable(main
    main.cpp
)

set_property(TARGET main PROPERTY CXX_STANDARD 23)

target_compile_options(main PRIVATE -Wall -Wextra -Wpedantic -Werror)

target_include_directories(main PUBLIC .)

