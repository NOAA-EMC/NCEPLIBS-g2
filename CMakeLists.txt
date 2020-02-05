cmake_minimum_required(VERSION 3.15)
project(g2 VERSION 3.1.0)
set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")

enable_language (Fortran)
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: PRODUCTION Debug Release."
    FORCE)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
if( NOT BUILD_RELEASE )
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
endif()
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package(Jasper REQUIRED)
find_package(PNG REQUIRED)
find_package(ZLIB REQUIRED)

if(APPLE)
  set(c_definitions "APPLE" "__64bit__")
else()
  set(c_definitions "LINUX" "__64bit__")
endif()

if(IntelComp)
  set(c_flags "-O3" "-g")
  set(c_definitions "LINUX" "__64BIT__")
  set(shared_fortran_flags "-g" "-assume" "noold_ldout_format")
  set(fortran_d_flags "-r8")
  set(fortran_4_flags)
elseif(GNUComp)
  set(c_flags "-ggdb" "-Wall")
  set(shared_fortran_flags "-ggdb" "-Wall" "-fno-range-check" "-funroll-loops")
  set(fortran_d_flags "-fdefault-real-8")
  set(fortran_4_flags)
elseif(PGIComp)
  set(c_flags "-g" "-fast")
  set(c_definitions ${c_definitions} "UNDERSCORE")
  set(shared_fortran_flags "-g" "-fast")
  set(fortran_d_flags "-i8" "-r8")
  set(fortran_4_flags)
else()
  message("unknown compiler!")
  exit()
endif()

file(GLOB fortran_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f)
file(GLOB c_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

set(kinds "4" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})


  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES POSITION_INDEPENDENT_CODE ON)
  set_target_properties(${lib_name}_f PROPERTIES
    COMPILE_OPTIONS "${shared_fortran_flags};${fortran_${kind}_flags}")

  add_library(${lib_name}_c OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES POSITION_INDEPENDENT_CODE ON)
  set_target_properties(${lib_name}_c PROPERTIES
    COMPILE_OPTIONS "${c_flags}")
  set_target_properties(${lib_name}_c PROPERTIES
    COMPILE_DEFINITIONS "${c_definitions}")

  target_include_directories(${lib_name}_f PRIVATE
    ${ZLIB_INCLUDE_DIRS} ${PNG_INCLUDE_DIRS} ${JASPER_INCLUDE_DIR})
  target_include_directories(${lib_name}_c PRIVATE
    ${ZLIB_INCLUDE_DIRS} ${PNG_INCLUDE_DIRS} ${JASPER_INCLUDE_DIR})

  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)

  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)

  install(TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib) 
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(EXPORT ${PROJECT_NAME}-config DESTINATION ${CMAKE_INSTALL_PREFIX})
