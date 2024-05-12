# This is the CMake file for the test directory in the NCEPLIBS-g2
# project. 
#
# Ed Hartnett

# This is needed for macos to work on GitHub CI.
link_directories("/usr/local/lib")

# Some test files are large and are kept on the NOAA EMC FTP
# site. This function is used to download such test data. It takes two
# arguments, the URL and the file to be downloaded.
function(PULL_DATA THE_URL THE_FILE)
  # If the TEST_FILE_DIR was specified, look for our test data files
  # there before FTPing them. Developers can keep all test files on
  # their machines, and save the time of downloading them every time.
  if(NOT ${TEST_FILE_DIR} STREQUAL ".")
    if (EXISTS ${TEST_FILE_DIR}/${THE_FILE})
      message(STATUS "Copying file ${TEST_FILE_DIR}/${THE_FILE} to test data directory.")
      FILE(COPY ${TEST_FILE_DIR}/${THE_FILE}
        DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/data)
    endif()
  endif()
  if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}")
    message(STATUS "Downloading file ${THE_URL}/${THE_FILE} to test data directory.")    
    file(DOWNLOAD
      ${THE_URL}/${THE_FILE}
      ${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}
      SHOW_PROGRESS
      STATUS status
      INACTIVITY_TIMEOUT 30
      )
    list(GET status 0 status_num)
    if(NOT status_num EQUAL 0 OR NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/data/${THE_FILE}")
      message(FATAL_ERROR "Could not download ${THE_FILE}")
    endif()
  endif()
endfunction()

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data name)
  message(STATUS "Copying test file ${name}")
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${name}"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# This function builds, links, and runs a test program.
function(create_test name kind)
  if(${ARGC} GREATER 2)
    set(extra_files ${ARGN})
  endif()
  add_executable(${name}_${kind} ${name}.F90 g2_test_util.F90 ${extra_files})
  target_link_libraries(${name}_${kind} PRIVATE g2_${kind} PNG::PNG)
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

# This function runs a test script.
function(gu_test name)
  # Copy the test scripts.
  file(COPY "${CMAKE_SOURCE_DIR}/tests/${name}.sh"
    DESTINATION ${CMAKE_BINARY_DIR}/tests
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
  # Add the shell script as a test.
  add_test(NAME ${name}.sh COMMAND bash ${name}.sh)
endfunction()

# Copy the VERSION file, it's needed in a test.
FILE(COPY ${CMAKE_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Does the user want to get extra test files from the FTP site, and
# run extra tests on them?
if(FTP_TEST_FILES)
  # This is the FTP site.
  set(G2_FTP_URL "https://ftp.emc.ncep.noaa.gov/static_files/public/NCEPLIBS-g2")

  # These are the test data files.
  set(FTP_FILES WW3_Regional_US_West_Coast_20220718_0000.grib2
    WW3_Regional_US_East_Coast_20220717_0600.grib2
    gep19.t00z.pgrb2a.0p50_bcf144 geavg.t00z.pgrb2a.0p50_mecomf144
    gec00.t00z.pgrb2a.0p50.f144 gegfs.t00z.pgrb2a.0p50.f144
    gegfs.t00z.pgrb2a.0p50_mef144 gep19.t00z.pgrb2a.0p50.f144
    gdas.t12z.pgrb2.1p00.anl.grib2 blend.t19z.core.f001.co.grib2
    aqm.t12z.max_8hr_o3.227.grib2 seaice.t00z.grb.grib2)

  # User may also ask for large test file.
  if(FTP_LARGE_TEST_FILES)
    set(FTP_FILES ${FTP_FILES} fv3lam.t00z.prslev.f000.grib2 rrfs.t12z.prslevfaa.f010.na3km.grib2 grib2.awips.rrfs.010)
  endif()

  # User may also ask for extra large test files.
  if(FTP_EXTRA_TEST_FILES)
    set(FTP_FILES ${FTP_FILES} GFSPRS.GrbF06 rrfs.t18z.prslev.f000.grib2)
  endif()
  message(STATUS "Getting these files from FTP: ${FTP_FILES}.")      
  
  # Get each of the test data files.
  foreach(THE_FILE IN LISTS FTP_FILES) 
    PULL_DATA(${G2_FTP_URL} ${THE_FILE})
  endforeach()

  # Add tests that use the data downloaded from FTP.
  # Build a _4 and _d version of each test and link them to the _4 and
  # _d builds of the library, for 4-byte real, and 8-byte real.
  foreach(kind ${kinds})
    create_test(test_skgb ${kind})
    create_test(test_ixgb2 ${kind})  
    create_test(test_getg2ir ${kind})  
    create_test(test_getidx ${kind})  
    create_test(test_getgb2rp ${kind})  
    create_test(test_getgb2rp_2 ${kind})  
    create_test(test_getgb2s ${kind})  
    create_test(test_getgb2p ${kind})  
    create_test(test_getgb2r ${kind})  
    create_test(test_getgb2ir ${kind})
    create_test(test_file_blend ${kind})
    create_test(test_aqm ${kind})
    create_test(test_create_index_gdas ${kind})
    create_test(test_create_index_seaice ${kind})
    # This test depends on gdt2gds(), which is not present if the GRIB1
    # library is not also included in the build.
    if (BUILD_WITH_W3EMC)
      create_test(test_getgb2_mem ${kind})
    endif()
    # This test depends on a very large file downloaded from FTP.
    if(FTP_LARGE_TEST_FILES)    
      create_test(test_fv3 ${kind})
      create_test(test_create_index_fv3 ${kind})
      create_test(test_files_fv3 ${kind})
      create_test(test_getgb2p_2 ${kind})
    endif()
    if(FTP_EXTRA_TEST_FILES)    
      create_test(test_create_index_gfsprs ${kind})
    endif()
  endforeach()  
endif()

# Copy test data files that are in the repo to the build directory.
copy_test_data(testdata_g2grids)
copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grb2index)
copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grb2index2)
copy_test_data(g1.grib2)
copy_test_data(gdaswave.t00z.wcoast.0p16.f000.grib2)
copy_test_data(ref_test_jpcpack_4_output.txt)

# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
#  create_test(test_g2cf ${kind})
  create_test(test_misc ${kind})
  create_test(test_g2 ${kind})
  create_test(test_g1 ${kind})
  create_test(test_g2_encode ${kind})
  create_test(test_g2_decode ${kind})
  create_test(test_gridtemplates ${kind})
  create_test(test_drstemplates ${kind})
  create_test(test_params ${kind})
  create_test(test_params_ecmwf ${kind})
  create_test(test_pdstemplates ${kind})
  create_test(test_pdstemplates_2 ${kind})
  create_test(test_getgb2 ${kind})
  create_test(test_getdim ${kind})
  create_test(test_getpoly ${kind})
  create_test(test_intmath ${kind})
  create_test(test_g2grids ${kind})
  create_test(test_cmplxpack ${kind})
  create_test(test_mkieee ${kind})
  create_test(test_getlocal ${kind})
  create_test(test_index_gdas ${kind} index_rec.F90)
  create_test(test_create_index ${kind})
  create_test(test_getgb2p_gdas ${kind})
  set_tests_properties(test_getgb2p_gdas_${kind} PROPERTIES LABELS noMemcheck)  
  create_test(test_realloc ${kind})
  create_test(test_simpack ${kind})
  create_test(test_gbytec ${kind})
  create_test(test_gbytec2 ${kind})
  create_test(test_gribcreate ${kind})
  create_test(test_getfield ${kind})
  create_test(test_pngpack ${kind})
  create_test(test_jpcpack ${kind})
  create_test(test_specpack ${kind})
  create_test(test_files ${kind})
  create_test(test_gb_info ${kind})
  create_test(test_gettemplates ${kind})
  create_test(test_addfield ${kind})
  create_test(test_gf_unpack2 ${kind})

  # This test depends on gdt2gds(), which is not present if the GRIB1
  # library is not also included in the build.
  if (BUILD_WITH_W3EMC)
    create_test(test_gdt2gds ${kind})
  endif()

  if (${kind} EQUAL "4")
    # Add this script to ensure that jasper is not issuing warnings.
    gu_test(run_jasper_warning_test)  
    set_tests_properties(run_jasper_warning_test.sh PROPERTIES LABELS noMemcheck)
  endif()
endforeach()

