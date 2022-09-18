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
  integer :: g1_table_version, g1_val, g2_discipline, g2_category, g2_param_num
  character(len = 8) :: g2_abbrev
  integer :: ios, i

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

  print *, 'Testing param_all with out of range indexes...'
  call param_all(0, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 255 .or. g1_val .ne. 255 .or. g2_discipline .ne. 255 .or. &
       g2_category .ne. 255 .or. g2_param_num .ne. 255 .or. g2_abbrev .ne. 'UNKNOWN ') stop 10
  call param_all(2001, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 255 .or. g1_val .ne. 255 .or. g2_discipline .ne. 255 .or. &
       g2_category .ne. 255 .or. g2_param_num .ne. 255 .or. g2_abbrev .ne. 'UNKNOWN ') stop 10
  
  print *, 'Testing param_all with some real indexes...'
  call param_all(1, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 2 .or. g1_val .ne. 1 .or. g2_discipline .ne. 0 .or. &
       g2_category .ne. 3 .or. g2_param_num .ne. 0 .or. g2_abbrev .ne. 'PRES') stop 10
  call param_all(1019, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 130 .or. g1_val .ne. 160 .or. g2_discipline .ne. 2 .or. &
       g2_category .ne. 3 .or. g2_param_num .ne. 5 .or. g2_abbrev .ne. 'SOILL') stop 10

  print *, 'Writing a CSV file with all parameters...'
  open(LU, FILE='noaa_grib2_params.csv', IOSTAT = ios)
  if (ios .ne. 0) stop 50

  write(LU, *, IOSTAT = ios) 'GRIB1_version, GRIB1_value, GRIB2_discipline, GRIB2_category, GRIB2_parameter'
  if (ios .ne. 0) stop 70

  ! Send a CSV list of params to a file.
  do i = 1, 2000
     call param_all(i, g1_table_version, g1_val, g2_discipline, g2_category, &
          g2_param_num, g2_abbrev)
     if (g1_table_version .eq. 0 .and. g1_val .eq. 0 .and. g2_discipline .eq. 0 .and.  g2_category .eq. 0 .and. &
          g2_param_num .eq. 0) cycle
     write(LU, *, IOSTAT = ios) g1_table_version, ',', g1_val, ',', g2_discipline, ',', g2_category, ',', &
          g2_param_num, ', ', g2_abbrev
  end do

  close(LU)
  
  print *, 'SUCCESS!'
  
end program test_params
