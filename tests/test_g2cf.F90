! This is a test program for NCEPLIBS-g2.
!
! This program tests the g2cf functions.
!
! Ed Hartnett 11/21/22
program test_g2cf
  use g2cf
  implicit none
  integer :: ierr

  print *, 'Testing g2cf API...'
  ierr = g2cf_set_log_level(10)

  print *, 'SUCCESS!'
end program test_g2cf
