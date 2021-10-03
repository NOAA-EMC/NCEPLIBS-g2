
add_executable(test_g2 test_g2.f90)
target_link_libraries(test_g2 g2_d)
add_test(test_g2 test_g2)

add_executable(test_gridtemplates test_gridtemplates.f90)
target_link_libraries(test_gridtemplates g2_d)
add_test(test_gridtemplates test_gridtemplates)

add_executable(test_drstemplates test_drstemplates.f90)
target_link_libraries(test_drstemplates g2_d)
add_test(test_drstemplates test_drstemplates)

add_executable(test_params test_params.f90)
target_link_libraries(test_params g2_d)
add_test(test_params test_params)

add_executable(test_params_ecmwf test_params_ecmwf.f90)
target_link_libraries(test_params_ecmwf g2_d)
add_test(test_params_ecmwf test_params_ecmwf)

add_executable(test_simpack test_simpack.f90)
target_link_libraries(test_simpack g2_d)
add_test(test_simpack test_simpack)

