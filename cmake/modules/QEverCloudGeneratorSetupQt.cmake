if (BUILD_WITH_QT6)
  find_package(Qt6 6.0.0 COMPONENTS Core)
else()
  find_package(Qt5 5.12.0 COMPONENTS Core)
endif()

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR "ON")
