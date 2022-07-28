# This is the CMake file for the test directory in the NCEPLIBS-g2
# project.
#
# Ed Hartnett

# This is needed for macos to work on GitHub CI.
link_directories("/usr/local/lib")

# Common functions used for testing
set(lib_src creategrib.f90)
add_library(g2_test_lib STATIC ${lib_src})
target_link_libraries(g2_test_lib PRIVATE bacio::bacio)

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
function(create_test name g2_kind)
  add_executable(${name} ${name}.f90)
  target_link_libraries(${name} PRIVATE g2_test_lib ${g2_kind} PNG::PNG)
  add_test(${name} ${name})
endfunction()

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
  create_test(test_ixgb2 g2_d)  
  create_test(test_getg2ir g2_d)  
  create_test(test_getidx g2_d)  
  create_test(test_getgb2rp g2_d)  
  create_test(test_getgb2s g2_d)  
  create_test(test_getgb2p g2_d)  
endif()

# Copy test data files that are in the repo to the build directory.
copy_test_data(testdata_g2grids)
copy_test_data(gdaswave.t00z.wcoast.0p16.f000.grib2.idx)

# These tests link to the g2_d library.
create_test(test_g2 g2_d)
create_test(test_g2_encode g2_d)
create_test(test_g2_decode g2_d)
create_test(test_gridtemplates g2_d)
create_test(test_drstemplates g2_d)
create_test(test_params g2_d)
create_test(test_params_ecmwf g2_d)
create_test(test_pngpack g2_d)
create_test(test_pdstemplates g2_d)
create_test(test_getgb2 g2_d)
create_test(test_getdim g2_d)
create_test(test_getpoly g2_d)
create_test(test_intmath g2_d)
create_test(test_g2grids g2_d)
create_test(test_cmplxpack g2_d)
create_test(test_mkieee g2_d)
create_test(test_getlocal g2_d)
create_test(test_putgb2 g2_d)
create_test(test_getg2i g2_d)
create_test(test_realloc g2_d)

# These tests link to the g2_4 library.
create_test(test_simpack g2_4)
create_test(test_gbytec g2_4)
create_test(test_gribcreate g2_4)
create_test(test_getfield g2_4)
