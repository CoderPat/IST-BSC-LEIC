cmake_minimum_required (VERSION 2.6)
project(Project2)

include_directories(include)
set(CMAKE_CXX_FLAGS "-O3 -ansi -Wall -lm")
add_executable(proj2 src/proj2.cpp src/vertexheap.cpp)