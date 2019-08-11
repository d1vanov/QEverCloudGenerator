find_package(Qt5Core REQUIRED)

if (Qt5Core_VERSION VERSION_LESS 5.5.0)
  message(FATAL_ERROR "Minimum supported Qt version is 5.5.0")
endif()

set(QT_INCLUDES "$${Qt5Core_INCLUDE_DIRS}")
set(QT_LIBRARIES "${Qt5Core_LIBRARIES}")
set(QT_DEFINITIONS "${Qt5Core_DEFINITIONS}")

include_directories(SYSTEM "${QT_INCLUDES} ${SYSTEM}")
add_definitions(${QT_DEFINITIONS})

add_definitions("-DQT_NO_CAST_FROM_ASCII -DQT_NO_CAST_TO_ASCII")
add_definitions("-DQT_NO_CAST_FROM_BYTEARRAY")
add_definitions("-DQT_NO_NARROWING_CONVERSIONS_IN_CONNECT")

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR "ON")
