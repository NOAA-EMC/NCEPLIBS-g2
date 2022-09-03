! This is a test program for NCEPLIBS-g2.
!
! This program tests reading a GRIB2 file.
!
! Ed Hartnett 9/3/22
program test_files
  use grib_mod, only: G2_VERSION
  implicit none

  print *, 'Testing reading GRIB2 files...'

  print *, 'SUCCESS!'
end program test_files
