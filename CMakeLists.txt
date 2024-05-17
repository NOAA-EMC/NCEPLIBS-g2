# This is the main CMake file for NCEPLIBS-g2.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

# Read the current version number from file VERSION.
file(STRINGS "VERSION" pVersion)

project(g2 VERSION ${pVersion} LANGUAGES C Fortran)
set(lib_name ${PROJECT_NAME})

include(GNUInstallDirs)

# Handle user build options.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(FTP_TEST_FILES "Fetch and test with files on FTP site." OFF)
option(FTP_LARGE_TEST_FILES "Fetch and test with very large files on FTP site." OFF)
option(FTP_EXTRA_TEST_FILES "Fetch even more large files from FTP and test them." OFF)
option(LOGGING "Turn on internal logging messages. Only useful to g2 developers." OFF)
option(BUILD_4 "Build libg2_4.a" ON)
option(BUILD_D "Build libg2_d.a" ON)
option(BUILD_WITH_W3EMC "Build with NCEPLIBS-w3emc, enabling some GRIB1 functionality" ON)

# Developers can use this option to specify a local directory which
# holds the test files. They will be copied instead of fetching the
# files via FTP.
SET(TEST_FILE_DIR "." CACHE STRING "Check this directory for test files before using FTP.")
message(STATUS "Finding test data files in directory ${TEST_FILE_DIR}.")

# Set pre-processor symbol if logging is desired.
if(LOGGING)
  add_definitions(-DLOGGING)
endif()

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
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_C_FLAGS "-g ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format ${CMAKE_Fortran_FLAGS}")
  set(fortran_d_flags "-r8")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_C_FLAGS "-g ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall")
  set(CMAKE_Fortran_FLAGS "-g -funroll-loops ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
  set(fortran_d_flags "-fdefault-real-8")
endif()

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
endif()

# There was a bug in jasper for the intel compiler that was fixed in
# 2.0.25.
find_package(Jasper 2.0.25 REQUIRED)
find_package(PNG REQUIRED)
find_package(bacio REQUIRED)
if(bacio_VERSION LESS 2.5.0)
  add_library(bacio::bacio ALIAS bacio::bacio_4)
endif()

# NCEPLIBS-w3emc may be required.
if (BUILD_WITH_W3EMC)
  find_package(w3emc 2.9.0 REQUIRED)
endif()

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

# This will cause the memcheck to fail if memory problems are found.
set(MEMORYCHECK_COMMAND_OPTIONS "--leak-check=full --error-exitcode=1 --errors-for-leak-kinds=all")

# Turn on unit testing.
include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  
