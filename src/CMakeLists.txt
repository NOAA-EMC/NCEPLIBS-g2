# This is the CMake file for the src directory of the NCEPLIBS-g2 project.
#
# Mark Potts, Kyle Gerheiser

# These are the fortran source files.
set(fortran_src addfield.f addgrid.f addlocal.f cmplxpack.f compack.f
comunpack.f drstemplates.f g2_gbytesc.f g2grids.f gb_info.f gdt2gds.f
getdim.f getfield.f getg2i.f getg2ir.f getgb2.f getgb2l.f getgb2p.f
getgb2r.f getgb2rp.f getgb2s.f getidx.f getlocal.f getpoly.f
gettemplates.f gf_free.f gf_getfld.f gf_unpack1.f gf_unpack2.f
gf_unpack3.f gf_unpack4.f gf_unpack5.f gf_unpack6.f gf_unpack7.f
gribcreate.f gribend.f gribinfo.f ${CMAKE_BINARY_DIR}/src/gribmod.f
gridtemplates.f intmath.f ixgb2.f jpcpack.f jpcunpack.f misspack.f
mkieee.f pack_gp.f params_ecmwf.f params.f pdstemplates.f pngpack.f
pngunpack.f putgb2.f rdieee.f realloc.f reduce.f simpack.f simunpack.f
skgb.f specpack.f specunpack.f)

# These are the C source files.
set(c_src dec_jpeg2000.c dec_png.c enc_jpeg2000.c enc_png.c mova2i.c)

# Create this fortran file, which has the contents of the VERSION file
# substituted in.
CONFIGURE_FILE("${CMAKE_SOURCE_DIR}/src/gribmod.f.in" "${CMAKE_BINARY_DIR}/src/gribmod.f"
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
  set(CMAKE_Fortran_FLAGS "-g -fno-range-check -funroll-loops ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
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
  target_include_directories(${lib_name}_c PRIVATE ${JASPER_INCLUDE_DIR})
  target_link_libraries(${lib_name}_c PRIVATE PNG::PNG)

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
  target_link_libraries(${lib_name} PUBLIC bacio::bacio_4 PNG::PNG ${JASPER_LIBRARIES})
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
