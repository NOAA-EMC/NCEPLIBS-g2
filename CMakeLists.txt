cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  g2
  VERSION ${pVersion}
  LANGUAGES C Fortran)

include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
    "Release"
    CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)

find_package(bacio)

add_subdirectory(src)

if(ENABLE_TESTS)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(tests)
endif()
