# This is the CMake file for the src directory of the NCEPLIBS-g2 project.
#
# Mark Potts, Kyle Gerheiser, Ed Hartnett

# These are the fortran source files.
set(fortran_src addfield.F90 addgrid.F90 addlocal.F90 cmplxpack.F90
compack.F90 comunpack.F90 drstemplates.F90 g2_gbytesc.F90 g2grids.F90
gb_info.F90 gdt2gds.F90 getdim.F90 getfield.F90 getg2i.F90 getg2ir.F90
getgb2.F90 getgb2l.F90 getgb2p.F90 getgb2r.F90 getgb2rp.F90 getgb2s.F90
getidx.F90 getlocal.F90 getpoly.F90 gettemplates.F90 gf_free.F90 gf_getfld.F90
gf_unpack1.F90 gf_unpack2.F90 gf_unpack3.F90 gf_unpack4.F90 gf_unpack5.F90
gf_unpack6.F90 gf_unpack7.F90 gribcreate.F90 gribend.F90 gribinfo.F90
${CMAKE_BINARY_DIR}/src/gribmod.F90 gridtemplates.F90 intmath.F90 ixgb2.F90
jpcpack.F90 jpcunpack.F90 misspack.F90 mkieee.F90 pack_gp.F90 params_ecmwf.F90
params.F90 pdstemplates.F90 pngpack.F90 pngunpack.F90 putgb2.F90 rdieee.F90
realloc.F90 reduce.F90 simpack.F90 simunpack.F90 skgb.F90 specpack.F90
specunpack.F90)

# These are the C source files.
set(c_src dec_jpeg2000.c dec_png.c enc_jpeg2000.c enc_png.c mova2i.c)

# Create this fortran file, which has the contents of the VERSION file
# substituted in.
CONFIGURE_FILE("${CMAKE_SOURCE_DIR}/src/gribmod.F90.in" "${CMAKE_BINARY_DIR}/src/gribmod.F90"
  @ONLY)

# Set compiler definitions.
add_compile_definitions(__64BIT__)
if(APPLE)
  add_compile_definitions(APPLE)
elseif(UNIX)
  add_compile_definitions(LINUX)
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
  set(CMAKE_Fortran_FLAGS "-g -fno-range-check -funroll-loops -fsanitize=address ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall -fsanitize=address")
  set(fortran_d_flags "-fdefault-real-8")
endif()

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()

# Build a _4 and _d version of the library, for 4-byte real, and
# 8-byte real.
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
  target_include_directories(${lib_name}_c PRIVATE ${JASPER_INCLUDE_DIR} ${PNG_INCLUDE_DIRS})
  target_link_libraries(${lib_name}_c PRIVATE ${PNG_LIBRARIES})

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
  if(bacio_VERSION GREATER_EQUAL 2.5.0)
    target_link_libraries(${lib_name} PUBLIC bacio::bacio -fsanitize=address ${PNG_LIBRARIES} ${JASPER_LIBRARIES})
  else()
    target_link_libraries(${lib_name} PUBLIC bacio::bacio_4 -fsanitize=address ${PNG_LIBRARIES} ${JASPER_LIBRARIES})
  endif()
  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)
  list(APPEND LIB_TARGETS ${lib_name})

  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})

endforeach()

# Set up the install.
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
