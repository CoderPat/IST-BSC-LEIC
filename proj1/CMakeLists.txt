cmake_minimum_required (VERSION 2.6)
project(Project1)

include_directories(include)
set(CMAKE_CXX_FLAGS "-O3 -ansi -Wall -lm")
add_executable(proj1 src/proj1.cpp src/graph.cpp)