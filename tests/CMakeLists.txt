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

# Copy test data file to the build directory.
function(copy_test_data name)
  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${name}"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
endfunction()

# Create a test from a test source file.
function(create_test name g2_kind)
  add_executable(${name} ${name}.f90)
  target_link_libraries(${name} PRIVATE g2_test_lib ${g2_kind} PNG::PNG)
  add_test(${name} ${name})
endfunction()

# Copy test data files.
copy_test_data(testdata_g2grids)

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

# These tests link to the g2_4 library.
create_test(test_simpack g2_4)
create_test(test_gbytec g2_4)
create_test(test_gribcreate g2_4)
create_test(test_getfield g2_4)
