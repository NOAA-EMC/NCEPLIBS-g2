! This program tests the params_ecmwf module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 9/30/21
program test_params_ecmwf
  use params_ecmwf
  implicit none

  integer :: g2disc, g2cat, g2num, g1val, g1ver

  print *, 'Testing the params_ecmwf module.'

  print *, 'Testing param_ecmwf_g1_to_g2...'
  call param_ecmwf_g1_to_g2(1, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 2
  call param_ecmwf_g1_to_g2(52, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 3
  call param_ecmwf_g1_to_g2(-52, -1, g2disc, g2cat, g2num)
  if (g1val .le. 0 .or. g1ver .le. 0) stop 5
  call param_ecmwf_g1_to_g2(53, 129, g2disc, g2cat, g2num)
  if (g1val .ge. 53 .or. g1ver .ge. 129) stop 6  

  print *, 'Testing param_ecmwf_g2_to_g1...'
  call param_ecmwf_g2_to_g1(0, 3, 1, g1val, g1ver)
  if (g1val .ne. 151 .or. g1ver .ne. 128) stop 4
  ! There are two matches here. param_ecmwf_g2_to_g1() returns the first.
  call param_ecmwf_g2_to_g1(0, 0, 5, g1val, g1ver)
  if (g1val .ne. 202 .or. g1ver .ne. 128) stop 5
  
  print *, 'SUCCESS!'
  
end program test_params_ecmwf
