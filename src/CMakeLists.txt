
# This is the CMake file for the src directory of the NCEPLIBS-g2 project.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett

# These are the fortran source files.
set(fortran_src addfield.f addgrid.F90 addlocal.F90 cmplxpack.f compack.f
comunpack.f drstemplates.F90 g2_gbytesc.F90 g2grids.F90 gb_info.f gdt2gds.f
getdim.f getfield.F90 getg2i.f getg2ir.f getgb2.f getgb2l.f getgb2p.f
getgb2r.f getgb2rp.f getgb2s.f getidx.f getlocal.f getpoly.f
gettemplates.f gf_free.f gf_getfld.F90 gf_unpack1.f gf_unpack2.f
gf_unpack3.F90 gf_unpack4.f gf_unpack5.f gf_unpack6.f gf_unpack7.f
gribcreate.F90 gribend.F90 gribinfo.F90
${CMAKE_CURRENT_BINARY_DIR}/gribmod.F90 gridtemplates.f intmath.f
ixgb2.f jpcpack.F90 jpcunpack.F90 misspack.f mkieee.f pack_gp.f
params_ecmwf.f params.f pdstemplates.F90 pngpack.F90 pngunpack.F90
putgb2.F90 rdieee.f realloc.f reduce.f simpack.f simunpack.f skgb.f
specpack.f specunpack.f)

# These are the C source files.
set(c_src dec_jpeg2000.c dec_png.c enc_jpeg2000.c enc_png.c mova2i.c)

# Create this fortran file, which has the contents of the VERSION file
# substituted in.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/gribmod.F90.in" "${CMAKE_CURRENT_BINARY_DIR}/gribmod.F90" @ONLY)

set(kinds "4" "d")
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

  # Set preprocessor define so code can know what kind of library is
  # being built.
  target_compile_definitions(${lib_name}_f PUBLIC KIND=${kind})

  # Build the C code.
  add_library(${lib_name}_c OBJECT ${c_src})
  target_include_directories(${lib_name}_c PRIVATE ${JASPER_INCLUDE_DIR})
  target_link_libraries(${lib_name}_c PRIVATE PNG::PNG)

  # Assemble the library.
  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
  
  # Link to bacio, jasper, and w3emc.
  target_link_libraries(${lib_name} PUBLIC bacio::bacio ${PNG_LIBRARIES} ${JASPER_LIBRARIES} w3emc::w3emc_${kind})
  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)
  list(APPEND LIB_TARGETS ${lib_name})

  # Build with Jasper.
  if(JASPER_FOUND)
    message(STATUS "Found JASPER:")
    message(STATUS "      JASPER include directories ${JASPER_INCLUDE_DIR}")
    message(STATUS "      JASPER libraries ${JASPER_LIBRARIES} version ${JASPER_VERSION}")
    target_sources(${lib_name} PRIVATE dec_jpeg2000.c enc_jpeg2000.c)
    target_compile_definitions(${lib_name} PRIVATE USE_JPEG2000)
    target_include_directories(${lib_name} PRIVATE "${JASPER_INCLUDE_DIR}")
    set(G2_JPEG2000_ENABLED ON)
    if(JASPER_VERSION_STRING VERSION_GREATER_EQUAL "3")
      message(STATUS "      JASPER is version 3.0 or greater")
      add_definitions(-DJASPER3)
    endif()
    add_definitions(-DJPEG)
  endif()

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
