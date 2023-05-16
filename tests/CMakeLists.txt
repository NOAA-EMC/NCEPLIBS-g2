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
  if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}")
    file(DOWNLOAD
      ${THE_URL}/${THE_FILE}
      ${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}
      SHOW_PROGRESS
      STATUS status
      INACTIVITY_TIMEOUT 30
      )
    list(GET status 0 status_num)
    if(NOT status_num EQUAL 0 OR NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${THE_FILE}")
      message(FATAL_ERROR "Could not download ${THE_FILE}")
    endif()
  endif()
endfunction()

# Some very small test files may be committed to the repo. This
# function copies such a data file to the build directory.
function(copy_test_data name)
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${name}"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90 util.F90)
  target_link_libraries(${name}_${kind} PRIVATE g2_${kind} PNG::PNG)
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

# Copy the VERSION file, it's needed in a test.
FILE(COPY ${CMAKE_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Does the user want to get extra test files from the FTP site, and
# run extra tests on them?
if(FTP_TEST_FILES)
  # Get the files from the FTP site.
  set(G2_FTP_URL "https://ftp.emc.ncep.noaa.gov/static_files/public/NCEPLIBS-g2")
  set(WW3_WEST_FILE "WW3_Regional_US_West_Coast_20220718_0000.grib2")
  set(WW3_EAST_FILE "WW3_Regional_US_East_Coast_20220717_0600.grib2")
  # Not using the East Coast file yet.
  # foreach(THE_FILE IN LISTS WW3_WEST_FILE WW3_EAST_FILE) 
  foreach(THE_FILE IN LISTS WW3_WEST_FILE) 
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
#    create_test(test_getgb2rp ${kind})  
    create_test(test_getgb2s ${kind})  
    create_test(test_getgb2p ${kind})  
    create_test(test_getgb2r ${kind})  
    create_test(test_getgb2ir ${kind})
  endforeach()  
endif()

# Copy test data files that are in the repo to the build directory.
copy_test_data(testdata_g2grids)
copy_test_data(ref_gdaswave.t00z.wcoast.0p16.f000.grb2index)
copy_test_data(gdaswave.t00z.wcoast.0p16.f000.grib2)

# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
#  create_test(test_g2cf ${kind})
  create_test(test_misc ${kind})
  create_test(test_g2 ${kind})
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
  create_test(test_getg2i ${kind})
  create_test(test_realloc ${kind})
  create_test(test_gdt2gds ${kind})
  create_test(test_simpack ${kind})
  create_test(test_gbytec ${kind})
  create_test(test_gribcreate ${kind})
  create_test(test_getfield ${kind})
  create_test(test_pngpack ${kind})
  create_test(test_jpcpack ${kind})
  create_test(test_files ${kind})
endforeach()

