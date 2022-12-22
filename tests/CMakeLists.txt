# This is the CMake file for the test directory in the NCEPLIBS-g2
# project.
#
# Ed Hartnett

add_executable(test_g2 test_g2.f90)
target_link_libraries(test_g2 g2_d)
add_test(test_g2 test_g2)
add_executable(test_params test_params.F90)
target_link_libraries(test_params g2_d)
add_test(test_params test_params)
