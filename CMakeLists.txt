# This is the main CMake file for NCEPLIBS-g2.
#
# Mark Potts, Kyle Gerheiser
cmake_minimum_required(VERSION 3.18)

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

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch")
endif()

# There was a bug in jasper for the intel compiler that was fixed in
# 2.0.25.
find_package(Jasper 2.0.25 REQUIRED)
find_package(PNG REQUIRED)
find_package(bacio REQUIRED)
if(bacio_VERSION LESS 2.5.0)
  add_library(bacio::bacio ALIAS bacio::bacio_4)
endif()
find_package(w3emc 2.9.0 REQUIRED)

# Handle user build options.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(FTP_TEST_FILES "Fetch and test with files on FTP site." OFF)
option(BUILD_4 "Build libg2_4.a" ON)
option(BUILD_D "Build libg2_d.a" ON)

# Build the code in the source directory.
add_subdirectory(src)

# Unit testing.
include(CTest)
if(BUILD_TESTING)
  add_subdirectory(tests)

  # Copy the VERSION file, it's needed in a test.
  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

# g2-config.cmake
configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

# g2-config-version.cmake
write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

# g2-targets.cmake and g2-targets-<build-type>.cmake
install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
