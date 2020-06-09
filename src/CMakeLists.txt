
include("list_of_files.cmake")

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

set(kinds "4" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
    "${fortran_${kind}_flags}")
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY
    ${module_dir})

  add_library(${lib_name}_c OBJECT ${c_src})
  target_include_directories(${lib_name}_c
    PRIVATE
    ${ZLIB_INCLUDE_DIRS}
    ${JASPER_INCLUDE_DIR})

    add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
    target_link_libraries(${lib_name} PRIVATE PNG::PNG)
    target_include_directories(${lib_name} INTERFACE
      $<BUILD_INTERFACE:${module_dir}>
      $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)
    list(APPEND LIB_TARGETS ${lib_name})
 
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
  
endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

# bacio-config.cmake
configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

# bacio-config-version.cmake
write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

# bacio-targets.cmake and bacio-targets-<build-type>.cmake
install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
