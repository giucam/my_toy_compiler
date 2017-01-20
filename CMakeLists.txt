
cmake_minimum_required(VERSION 3.7)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED libClang)

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
    cparser.cpp
    types.cpp
    fmt/format.cc
    fmt/ostream.cc)

llvm_map_components_to_libnames(llvm_libs mcjit native)

add_executable(parser ${SOURCES})
target_link_libraries(parser ${LLVM_LDFLAGS} LLVM clang)

add_custom_target(build ./parser -o example example.txt print.lang  DEPENDS parser)
add_custom_target(run echo "Running..." COMMAND ./example DEPENDS build)
