# This is the main CMake file for NCEPLIBS-g2.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

# Read the current version number from file VERSION.
file(STRINGS "VERSION" pVersion)

project(
  g2
  VERSION ${pVersion}
  LANGUAGES C Fortran)

include(GNUInstallDirs)

# Handle build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
    "Release"
    CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

# Set flags.
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS "-g ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format ${CMAKE_Fortran_FLAGS}")
  set(fortran_d_flags "-r8")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_C_FLAGS "-g ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall")
  set(CMAKE_Fortran_FLAGS "-g -fno-range-check -funroll-loops ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
  set(fortran_d_flags "-fdefault-real-8")
endif()

# The GNU compilers (after version 10) require the -fallow-argument-mismatch flag.
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch")
endif()

# Handle user build options.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(BUILD_4 "Build libg2_4.a" ON)
option(BUILD_D "Build libg2_d.a" ON)

find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)

# Figure whether user wants a _4, a _d, or both libraries.
if(BUILD_4 AND BUILD_D)
  set(kinds "4" "d")
elseif(BUILD_4 AND NOT BUILD_D)
  set(kinds "4")
elseif(BUILD_D AND NOT BUILD_4)
  set(kinds "d")
else()
  message(FATAL_ERROR "At least one of BUILD_4 or BUILD_D must be turned on")
endif()

# Build the code in the source directory.
add_subdirectory(src)

# Unit testing.
include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  
