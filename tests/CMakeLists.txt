# This is the CMake file for the test directory in the NCEPLIBS-g2
# project.
#
# Ed Hartnett

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)
  target_link_libraries(${name}_${kind} PRIVATE g2_${kind} PNG::PNG)
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

# Copy the VERSION file, it's needed in a test.
FILE(COPY ${CMAKE_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Build a _4 and _d version of each test and link them to the _4 and
# _d builds of the library, for 4-byte real, and 8-byte real.
foreach(kind ${kinds})
#  create_test(test_g2_encode ${kind})
  create_test(test_g2_decode ${kind})
  create_test(test_gridtemplates ${kind})
  create_test(test_drstemplates ${kind})
  create_test(test_params ${kind})
  create_test(test_params_ecmwf ${kind})
  create_test(test_pdstemplates ${kind})
  create_test(test_pdstemplates_2 ${kind})
#  create_test(test_getgb2 ${kind})
  create_test(test_getdim ${kind})
  create_test(test_getpoly ${kind})
  create_test(test_intmath ${kind})
#  create_test(test_g2grids ${kind})
  create_test(test_mkieee ${kind})
#  create_test(test_getlocal ${kind})
#  create_test(test_getg2i ${kind})
  create_test(test_realloc ${kind})
#  create_test(test_gdt2gds ${kind})
  create_test(test_simpack ${kind})
  create_test(test_gbytec ${kind})
#  create_test(test_gribcreate ${kind})
  create_test(test_getfield ${kind})
  create_test(test_pngpack ${kind})
#  create_test(test_jpcpack ${kind})
endforeach()

# add_executable(test_g2 test_g2.f90)
# target_link_libraries(test_g2 g2_d)
# add_test(test_g2 test_g2)

# add_executable(test_params test_params.F90)
# target_link_libraries(test_params g2_d)
# add_test(test_params test_params)
