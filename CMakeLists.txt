cmake_minimum_required(VERSION 3.16.3)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)

include(QEverCloudGeneratorCMakePolicies)
SET_POLICIES()

project(QEverCloudGenerator)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)

option(BUILD_WITH_QT6 "Build with Qt6" OFF)

include(QEverCloudGeneratorSetupQt)
include(QEverCloudGeneratorCompilerSettings)

add_subdirectory(lemon)
get_directory_property(lemon DIRECTORY lemon DEFINITION PROJECT_NAME)

add_subdirectory(thrift_parser)
