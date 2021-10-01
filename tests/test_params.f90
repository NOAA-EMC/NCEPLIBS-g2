! This program tests the params module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 10/1/21
program test_params
  use params
  implicit none

  print *, 'Testing the params module.'

  print *, 'Testing param_g1_to_g2...'
  
  print *, 'Testing param_get_abbrev...'
  
  print *, 'Testing param_g2_to_g1...'
  
  print *, 'SUCCESS!'
  
end program test_gridtemplates
