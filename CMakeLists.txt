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
  set(shared_fortran_flags "-g" "-assume noold_ldout_format")
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
set_source_files_properties(${c_src} PROPERTIES COMPILE_OPTIONS "${c_flags}")
set_source_files_properties(${c_src} PROPERTIES COMPILE_DEFINITIONS "${c_definitions}")

set(kinds "4" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})
  add_library(${lib_name} STATIC ${fortran_src} ${c_src})

  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")
  
  set_source_files_properties(${fortran_src} PROPERTIES
    COMPILE_OPTIONS "${shared_fortran_flags};${fortran_${kind}_flags}")
  
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  target_include_directories(${lib_name} PRIVATE
    ${ZLIB_INCLUDE_DIRS} ${PNG_INCLUDE_DIRS} ${JASPER_INCLUDE_DIR})
  
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
