cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

project(stdexecExample)

include(cmake/CPM.cmake)

CPMAddPackage(
  NAME stdexec
  GITHUB_REPOSITORY NVIDIA/stdexec
  GIT_TAG main # This will always pull the latest code from the `main` branch. You may also use a specific release version or tag
)

add_executable(main example.cpp)

target_link_libraries(main STDEXEC::stdexec)

add_executable(hello_world hello_world.cpp)

target_link_libraries(hello_world STDEXEC::stdexec)