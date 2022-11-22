! This is a test program for NCEPLIBS-g2.
!
! This program tests the g2cf functions.
!
! Ed Hartnett 11/21/22
program test_g2cf
  use g2cf
  implicit none
  character (len = *), parameter :: fileName = "gdaswave.t00z.wcoast.0p16.f000.grib2"
  integer :: g2cid
  integer :: ierr

  print *, 'Testing g2cf API...'
  ierr = g2cf_set_log_level(10)

  ierr = g2cf_open(fileName, 0, g2cid)
  ierr = g2cf_close(g2cid)

  print *, 'SUCCESS!'
end program test_g2cf
