cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
  g2
  VERSION ${pVersion}
  LANGUAGES C Fortran)

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
find_package(ZLIB REQUIRED)

add_compile_definitions(__64BIT__)
if(APPLE)
  add_compile_definitions(APPLE)
elseif(UNIX)
  add_compile_definitions(LINUX)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS "-g -O3")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS "-g -assume noold_ldout_format")
  set(fortran_d_flags "-r8")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_C_FLAGS "-g")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall")
  set(CMAKE_Fortran_FLAGS "-g -fno-range-check -funroll-loops")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
  set(fortran_d_flags "-fdefault-real-8")
endif()

add_subdirectory(src)

set(kinds "4" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})
  set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
  target_include_directories(
    ${lib_name}_f PRIVATE ${ZLIB_INCLUDE_DIRS} ${PNG_INCLUDE_DIRS}
                          ${JASPER_INCLUDE_DIR})
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY
                                                 ${module_dir})

  add_library(${lib_name}_c OBJECT ${c_src})
  target_include_directories(
    ${lib_name}_c PRIVATE ${ZLIB_INCLUDE_DIRS} ${PNG_INCLUDE_DIRS}
                          ${JASPER_INCLUDE_DIR})

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f>
                                 $<TARGET_OBJECTS:${lib_name}_c>)
  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME
                                               ${versioned_lib_name})
  set_target_properties(${lib_name} PROPERTIES INTERFACE_LINK_LIBRARIES
                                               ${lib_name})
  target_include_directories(
    ${lib_name}
    PUBLIC $<BUILD_INTERFACE:${module_dir}>
           $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)

  install(
    TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(EXPORT ${PROJECT_NAME}-config
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/cmake/${PROJECT_NAME})
