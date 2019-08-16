cmake_minimum_required(VERSION 3.5.1)

include(QEverCloudGeneratorCMakePolicies)
SET_POLICIES()

project(thrift_parser)

set(HEADERS
    Generator.h
    Lexer.h
    Parser.h
    ParserHelper.h)

set(SOURCES
    Generator.cpp
    Lexer.cpp
    main.cpp
    Parser.cpp)

# Pre build step: copy thrift_lemon.y to build dir, execute lemon on it and remove the output file
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.out"
                   COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/thrift_lemon.y" "${CMAKE_CURRENT_BINARY_DIR}"
                   COMMAND ${CMAKE_COMMAND} -E echo "thrift_lemon.y was copied to build dir"
                   COMMAND ${lemon} "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.y"
                   COMMAND ${CMAKE_COMMAND} -E echo "lemon was executed on thrift_lemon.y"
                   DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/thrift_lemon.y")

add_custom_target(pre_build
                  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.out"
                  SOURCES thrift_lemon.y)

list(APPEND IDL_COMPILED_FILES "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.h")
list(APPEND IDL_COMPILED_FILES "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.cpp")

set_source_files_properties(${IDL_COMPILED_FILES} PROPERTIES GENERATED TRUE)

set_property(SOURCE "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.h" PROPERTY SKIP_AUTOMOC ON)
set_property(SOURCE "${CMAKE_CURRENT_BINARY_DIR}/thrift_lemon.cpp" PROPERTY SKIP_AUTOMOC ON)

add_executable(QEverCloudGenerator ${HEADERS} ${SOURCES} ${IDL_COMPILED_FILES})
target_link_libraries(QEverCloudGenerator ${QT_LIBRARIES})

target_compile_features(QEverCloudGenerator INTERFACE
  cxx_auto_type
  cxx_nullptr
  cxx_strong_enums)

target_compile_features(QEverCloudGenerator PRIVATE
  cxx_lambdas
  cxx_range_for)

add_dependencies(QEverCloudGenerator pre_build)

# install executable
install(TARGETS QEverCloudGenerator DESTINATION bin)
