cmake_minimum_required(VERSION 3.12)
cmake_policy(SET CMP0076 NEW)
project(cpp_result LANGUAGES CXX)

add_compile_options("-O3")

add_library(cpp_result)
target_compile_features(cpp_result PUBLIC cxx_std_20)
target_include_directories(cpp_result PUBLIC include)
target_sources(cpp_result
  PUBLIC include/result.hpp
  PRIVATE source/result.cpp
)

add_executable(result_test test/test.cpp)
target_link_libraries(result_test PRIVATE cpp_result)
