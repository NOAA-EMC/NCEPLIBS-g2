
# This is the CMake file for the src directory of the NCEPLIBS-g2 project.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett

# These are the fortran source files.
set(fortran_src compack.F90 drstemplates.F90 g2bytes.F90 g2grids.F90
g2get.F90 g2getgb2.F90 g2index.F90 g2gf.F90 g2unpack.F90 g2create.F90
${CMAKE_CURRENT_BINARY_DIR}/gribmod.F90 gridtemplates.F90 intmath.F90
g2jpc.F90 pack_gp.f params_ecmwf.F90 params.F90 pdstemplates.F90
g2png.F90 realloc.F90 reduce.f g2sim.F90 skgb.F90 g2spec.F90 g2logging.F90)

# This function calls NCEPLIBS-w3emc.
if (BUILD_WITH_W3EMC)
  set(fortran_src ${fortran_src} gdt2gds.F90)
endif()

if (USE_AEC)
  set(fortran_src ${fortran_src} g2aec.F90)
endif()

# These are the C source files.
set(c_src mova2i.c)

# Create this fortran file, which has the contents of the VERSION file
# substituted in.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/gribmod.F90.in" "${CMAKE_CURRENT_BINARY_DIR}/gribmod.F90" @ONLY)

foreach(kind ${kinds})

  # Figure out the library name and the include directory.
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})

  # Build the fortran code.
  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
    "${fortran_${kind}_flags}")
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY
    ${module_dir})
 if(BUILD_SHARED_LIBS)
    set_property(TARGET ${lib_name}_f PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()
  
  # Set preprocessor define so code can know what kind of library is
  # being built.
  target_compile_definitions(${lib_name}_f PUBLIC KIND=${kind})

  if (USE_AEC)
    target_compile_definitions(${lib_name}_f PRIVATE USE_AEC)
  endif()
  # Build the C code.
  add_library(${lib_name}_c OBJECT ${c_src})
  target_include_directories(${lib_name}_c PRIVATE ${JASPER_INCLUDE_DIR})
  target_link_libraries(${lib_name}_c PRIVATE PNG::PNG g2c::g2c)

 if(BUILD_SHARED_LIBS)
    set_property(TARGET ${lib_name}_c PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  # Assemble the library.
  add_library(${lib_name} $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
  
  # Link to bacio, and jasper always.
  target_link_libraries(${lib_name} PUBLIC bacio::bacio ${PNG_LIBRARIES} ${JASPER_LIBRARIES} g2c::g2c)
  # Link to w3emc if necessary.
  if (BUILD_WITH_W3EMC)
    target_link_libraries(${lib_name} PUBLIC w3emc::w3emc_${kind})    
  endif()

  # For shared, set SOVERSION; increment it if API changes (other than
  # adding new functions).
  if(BUILD_SHARED_LIBS)
    set_target_properties(${lib_name} PROPERTIES SOVERSION 0)
  endif()

  # Include directories.
  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)
  list(APPEND LIB_TARGETS ${lib_name})

  # Build with Jasper.
  message(STATUS "Found JASPER:")
  message(STATUS "      JASPER include directories ${JASPER_INCLUDE_DIR}")
  message(STATUS "      JASPER libraries ${JASPER_LIBRARIES} version ${JASPER_VERSION}")
  if(JASPER_VERSION_STRING VERSION_GREATER_EQUAL "3")
    message(STATUS "      JASPER is version 3.0 or greater")
  endif()
  target_include_directories(${lib_name} PRIVATE ${JASPER_INCLUDE_DIR})

  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})

endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

# g2-config.cmake
configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
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
