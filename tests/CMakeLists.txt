
add_executable(test_g2 test_g2.f90)
target_link_libraries(test_g2 g2_d)
add_test(test_g2 test_g2)
