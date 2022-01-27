cmake_minimum_required(VERSION 3.5.1)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)

include(QEverCloudGeneratorCMakePolicies)
SET_POLICIES()

project(QEverCloudGenerator)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BUILD_WITH_QT6 "Build with Qt6" OFF)

include(QEverCloudGeneratorSetupQt)
include(QEverCloudGeneratorCompilerSettings)

add_subdirectory(lemon)
get_directory_property(lemon DIRECTORY lemon DEFINITION PROJECT_NAME)

add_subdirectory(thrift_parser)
