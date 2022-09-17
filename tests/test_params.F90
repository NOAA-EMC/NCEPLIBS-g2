! This program tests the params module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 10/1/21
program test_params
  use params
  implicit none

  integer :: g2disc, g2cat, g2num, g1val, g1ver
  character(len=8) :: abbrev
  integer :: LU = 10;
  integer :: ios

  print *, 'Testing the params module.'

  print *, 'Testing param_g1_to_g2...'
  call param_g1_to_g2(1, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 2
  call param_g1_to_g2(47, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 3
  
  print *, 'Testing param_get_abbrev...'
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 4
  abbrev = param_get_abbrev(0, 3, 8)
  if (abbrev .ne. 'PRESA') stop 4
  
  print *, 'Testing param_g2_to_g1...'
  call param_g2_to_g1(0, 3, 1, g1val, g1ver)
  if (g1val .ne. 2 .or. g1ver .ne. 2) stop 6
  call param_g2_to_g1(0, 2, 0, g1val, g1ver)
  if (g1val .ne. 31 .or. g1ver .ne. 2) stop 7

  print *, 'Writing a CSV file with all parameters...'
  open(LU, FILE='noaa_grib2_params.csv', IOSTAT = ios)
  if (ios .ne. 0) stop 10

  write(LU, *, IOSTAT = ios) 'GRIB1_version, GRIB1_value, GRIB2_discipline, GRIB2_category, GRIB2_parameter'
  if (ios .ne. 0) stop 11

  close(LU)
  
  print *, 'SUCCESS!'
  
end program test_params
