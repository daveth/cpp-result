cmake_minimum_required(VERSION 3.12)
cmake_policy(SET CMP0076 NEW)
project(cpp_result LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(cpp_result)
target_compile_features(cpp_result PUBLIC cxx_std_20)
target_include_directories(cpp_result PUBLIC include)
target_sources(cpp_result
  PUBLIC  include/daveth/result.hpp
          include/daveth/result2.hpp
  PRIVATE source/result.cpp
)

add_executable(result_test test/test.cpp)
target_link_libraries(result_test PRIVATE cpp_result)
