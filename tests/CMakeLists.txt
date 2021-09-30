
add_executable(test_g2 test_g2.f90)
target_link_libraries(test_g2 g2_d)
add_test(test_g2 test_g2)

add_executable(test_gridtemplates test_gridtemplates.f90)
target_link_libraries(test_gridtemplates g2_d)
add_test(test_gridtemplates test_gridtemplates)
