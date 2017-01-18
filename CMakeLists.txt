
cmake_minimum_required(VERSION 3.7)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(SOURCES
    main.cpp
    codegen.cpp
    operators.cpp
    lexer.cpp
    parser.cpp
    value.cpp
    fmt/format.cc
    fmt/ostream.cc)

llvm_map_components_to_libnames(llvm_libs mcjit native)

add_executable(parser ${SOURCES})
target_link_libraries(parser ${llvm_libs})

add_custom_target(run ./parser example.txt example.ll COMMAND echo "Running..." COMMAND lli example.ll DEPENDS parser)
add_custom_target(build ./parser example.txt example.ll COMMAND clang example.ll -o example DEPENDS parser)
