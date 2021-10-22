# This is the main CMake file for NCEPLIBS-g2.
#
# Mark Potts, Kyle Gerheiser
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

# There was a bug in jasper for the intel compiler that was fixed in
# 2.0.25.
find_package(Jasper 2.0.25 REQUIRED)
find_package(PNG REQUIRED)
find_package(bacio REQUIRED)

add_subdirectory(src)

include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)

  # Copy the VERSION file, it's needed in a test.
  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)

  # Copy this sample GRIB2 file, it's needed in a test.
  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/gdaswave.t00z.arctic.9km.f000.grib2
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)  
endif()

# Determine whether or not to generate documentation.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  
