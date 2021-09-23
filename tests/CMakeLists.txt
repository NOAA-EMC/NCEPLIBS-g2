# This is the cmake build file for the tests directory of the
# NCEPLIBS-g2_UTILS project.
#

function(g2_test name)
  add_executable(${name} ${name}.f)
  add_dependencies(${name} g2)
  target_link_libraries(${name} PRIVATE g2)
  add_test(NAME ${name} COMMAND ${name})
endfunction()

g2_test(tst_decode)
