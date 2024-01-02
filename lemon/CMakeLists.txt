cmake_minimum_required(VERSION 3.16.3)

include(QEverCloudGeneratorCMakePolicies)
SET_POLICIES()

project(lemon)

add_executable(${PROJECT_NAME} main.c)

if(MSVC)
  target_compile_options(${PROJECT_NAME} PRIVATE "/wd4311")
endif()

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/lempar.c"
                   COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/lempar.c" "${CMAKE_CURRENT_BINARY_DIR}"
                   COMMAND ${CMAKE_COMMAND} -E echo "lempar.c was copied to build dir"
                   DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/lempar.c")

add_custom_target(copy_lempar
                  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/lempar.c")

add_dependencies(${PROJECT_NAME} copy_lempar)
