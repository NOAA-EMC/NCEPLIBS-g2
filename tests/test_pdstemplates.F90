! This is a test for the NCEPLIBS-g2 project.
!
! This program tests pdstemplates.F90.
!
! Brian Curtis 11/09/2021, Ed Hartnett
program test_pdstemplates
  use pdstemplates
  implicit none

  integer :: idx
  integer :: nummap
  integer :: iret, i
  logical :: needext
  integer :: pdtlen
  integer, dimension(MAXLEN) :: map, list, exp_map0, exp_map3, exp_map4, exp_map8, exp_map9, exp_map10, &
  exp_map11, exp_map12, exp_map13, exp_map14, exp_map30, exp_map31, exp_map32, exp_map33, exp_map34, &
  exp_map35,exp_map42, exp_map43, exp_map46, exp_map47, exp_map51, exp_map53, exp_map54, exp_map57, & 
  exp_map61, exp_map91, exp_extmap3, exp_extmap4, exp_extmap8, exp_extmap9, exp_extmap10, exp_extmap11, &
  exp_extmap12, exp_extmap13, exp_extmap14, exp_extmap30, exp_extmap31, exp_extmap32, exp_extmap33, &
  exp_extmap34, exp_extmap35, exp_extmap42, exp_extmap43, exp_extmap46, exp_extmap47, exp_extmap51, & 
  exp_extmap53, exp_extmap54, exp_extmap57, exp_extmap61, exp_extmap91

  print *, 'Testing pdstemplates, expect and ignore error messages...'

  print *, 'Testing getpdsindex() ...'
  ! Fortran is base 1, so index 0 should = 1
  idx = getpdsindex(0)
  if (idx .ne. 1) stop 3
  ! Index -1 will still equal -1 because it doesn't exist
  idx = getpdsindex(-1)
  if (idx .ne. -1) stop 4

  print *, 'testing getpdstemplate() ...'
  pdtlen = getpdtlen(0)
  if (pdtlen .ne. 15) stop 5
  exp_map0(1:pdtlen) = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4/)
  call getpdstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0) stop 6
  if (pdtlen .ne. nummap) stop 7
  do i = 1, nummap
      if (map(i) .ne. exp_map0(i)) stop 8
  end do
  if (needext) stop 9

  print *, 'testing getpdtlen() with template -1 (nonexistent)...'
  pdtlen = getpdtlen(-1)
  if (pdtlen .ne. 0) stop 10

  print *, 'testing getpdstemplate() with template -1 (nonexistent)...'  
  call getpdstemplate(-1, nummap, map, needext, iret)
  if (iret .eq. 0) stop 11
  if (nummap .ne. 0) stop 12
  if (needext) stop 13

  print *, 'testing extpdstemplate()...'
  list = 0
  map = 0
  ! Setting expected maps
  pdtlen = getpdtlen(3)
  if (pdtlen .ne. 31) stop 40
  exp_map3(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, -4, 4, 4,  &
      1, -1, 4, -1, 4/)
  pdtlen = getpdtlen(4)
  if (pdtlen .ne. 30) stop 41
  exp_map4(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, 4, 4,  &
      1, -1, 4, -1, 4/)
  pdtlen = getpdtlen(8)
  if (pdtlen .ne. 29) stop 42
  exp_map8(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(9)
  if (pdtlen .ne. 36) stop 43
  exp_map9(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, -4, 2, 1, 1, 1,  &
      1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(10)
  if (pdtlen .ne. 30) stop 44
  exp_map10(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 2, 1, 1, 1, 1, 1, 1, 4,  &
      1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(11)
  if (pdtlen .ne. 32) stop 45
  exp_map11(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,  &
      4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(12)
  if (pdtlen .ne. 31) stop 46
  exp_map12(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1,  &
      2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(13)
  if (pdtlen .ne. 45) stop 47
  exp_map13(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, -4, 4, 4,  &
      1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(14)
  if (pdtlen .ne. 44) stop 48
  exp_map14(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, 4, 4,  &
      1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(30)
  if (pdtlen .ne. 5) stop 49
  exp_map30(1:pdtlen) = (/1, 1, 1, 1, 1/)
  pdtlen = getpdtlen(31)
  if (pdtlen .ne. 5) stop 50
  exp_map31(1:pdtlen) = (/1, 1, 1, 1, 1/)
  pdtlen = getpdtlen(32)
  if (pdtlen .ne. 10) stop 51
  exp_map32(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1/)
  pdtlen = getpdtlen(33)
  if (pdtlen .ne. 18) stop 52
  exp_map33(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1/)
  pdtlen = getpdtlen(34)
  if (pdtlen .ne. 32) stop 53
  exp_map34(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1, 2, 1, 1, 1,  &
      1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(42)
  if (pdtlen .ne. 30) stop 54
  exp_map42(1:pdtlen) = (/1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4,  &
      1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(43)
  if (pdtlen .ne. 33) stop 55
  exp_map43(1:pdtlen) = (/1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4,  &
      1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(46)
  if (pdtlen .ne. 35) stop 56
  exp_map46(1:pdtlen) = (/1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1,  &
      1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(47)
  if (pdtlen .ne. 38) stop 57
  exp_map47(1:pdtlen) = (/1, 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1,  &
      1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(51)
  if (pdtlen .ne. 16) stop 58
  exp_map51(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1/)
  pdtlen = getpdtlen(53)
  if (pdtlen .ne. 19) stop 59
  exp_map53(1:pdtlen) = (/1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4/)
  pdtlen = getpdtlen(54)
  if (pdtlen .ne. 22) stop 60
  exp_map54(1:pdtlen) = (/1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1/)
  pdtlen = getpdtlen(91)
  if (pdtlen .ne. 36) stop 61
  exp_map91(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, -4,  &
      2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(57)
  if (pdtlen .ne. 7) stop 62
  exp_map57(1:pdtlen) = (/1, 1, 2, 2, 2, 2, 1/)
  pdtlen = getpdtlen(61)
  if (pdtlen .ne. 38) stop 63
  exp_map61(1:pdtlen) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, &
      1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4/)
  pdtlen = getpdtlen(35)
  if (pdtlen .ne. 6) stop 64
  exp_map35(1:pdtlen) = (/1,1,1,1,1,1/)

  ! Setting expexted extended maps
  exp_extmap3(1:32) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, -4, 4, 4, 1, -1, 4, -1, &
      4, 1/)
  exp_extmap4(1:31) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, 4, 4, 1, -1, 4, -1, 4, &
      1/)
  exp_extmap8(1:35) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, &
      1, 1, 4, 1, 4/)
  exp_extmap9(1:42) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, &
      1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap10(1:36) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, &
      1, 1, 1, 4, 1, 4/)
  exp_extmap11(1:38) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, &
      1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap12(1:37) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, &
      4, 1, 1, 1, 4, 1, 4/)
  exp_extmap13(1:52) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, -4, 4, 4, 1, -1, 4, -1, &
      4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4, 1/)
  exp_extmap14(1:51) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, 1, -4, 4, 4, 1, -1, 4, -1, 4, &
      2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4, 1/)
  exp_extmap30(1:10) = (/1, 1, 1, 1, 1, 2, 2, 1, 1, 4/)
  exp_extmap31(1:10) = (/1, 1, 1, 1, 1, 2, 2, 2, 1, 4/)
  exp_extmap32(1:15) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4/)
  exp_extmap33(1:19) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1, 1/)
  exp_extmap34(1:39) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, &
      1, 1, 1, 4, 1, 4, 1/)
  exp_extmap42(1:36) = (/1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, &
      1, 1, 1, 4, 1, 4/)
  exp_extmap43(1:39) = (/1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, &
      1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap46(1:41) = (/1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, &
      1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap47(1:44) = (/1, 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, &
      1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap51(1:22) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, -4/)
  exp_extmap53(1:20) = (/1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1/)
  exp_extmap54(1:23) = (/1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1/)
  exp_extmap91(1:43) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, &
      1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4, 1/)
  exp_extmap57(1:22) = (/1, 1, 2, 2, 2, 2, 1, 1, -4, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4/)
  exp_extmap61(1:44) = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, &
      1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 4/)
  exp_extmap35(1:11) = (/1,1,1,1,1,1,2,2,2,1,4/)

  print *, 'Testing extpdstemplate with index = -1' 
  ! -- returns without doing anything
  call extpdstemplate(-1, list, nummap, map)
  print *, 'Testing extpdstemplate on template without map extension (needext = false)'
  ! -- returns without doing anything
  call extpdstemplate(0, list, nummap, map)
  print *, 'Testing the extendable tempaltes'
  ! ---- still need to test output i.e. nummap and map ----
  ! Template number 3
  ! Getting correct template
  call getpdstemplate(3, nummap, list, needext, iret)
  ! Checking return values of getpdstemplate against expected
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 14
  do i = 1, nummap
    if (list(i) .ne. exp_map3(i)) stop 62
  end do
  ! Extending the template
  call extpdstemplate(3, list, nummap, map)
  ! Checking the returned map and length against expected
  if (nummap .ne. 32) stop 100
  do i = 1, nummap
    if (map(i) .ne. exp_extmap3(i)) stop 101
  end do
  ! Template number 4
  call getpdstemplate(4, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 15
  do i = 1, nummap
    if (list(i) .ne. exp_map4(i)) stop 63
  end do
  call extpdstemplate(4, list, nummap, map)
  if (nummap .ne. 31) stop 102
  do i = 1, nummap
    if (map(i) .ne. exp_extmap4(i)) stop 103
  end do
  ! Template number 8
  call getpdstemplate(8, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 29 .or. .not. needext) stop 16
  do i = 1, nummap
    if (list(i) .ne. exp_map8(i)) stop 64
  end do
  list(22) = 2
  call extpdstemplate(8, list, nummap, map)
  if (nummap .ne. 35) stop 104
  do i = 1, nummap
    if (map(i) .ne. exp_extmap8(i)) stop 105
  end do
  ! Template number 9
  call getpdstemplate(9, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 17
  do i = 1, nummap
    if (list(i) .ne. exp_map9(i)) stop 65
  end do
  list(29) = 2
  call extpdstemplate(9, list, nummap, map)
  if (nummap .ne. 42) stop 106
  do i = 1, nummap
    if (map(i) .ne. exp_extmap9(i)) stop 107
  end do
  ! Template number 10
  call getpdstemplate(10, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 18
  do i = 1, nummap
    if (list(i) .ne. exp_map10(i)) stop 66
  end do
  list(23) = 2
  call extpdstemplate(10, list, nummap, map)
  if (nummap .ne. 36) stop 108
  do i = 1, nummap
    if (map(i) .ne. exp_extmap10(i)) stop 109
  end do
  ! Template number 11
  call getpdstemplate(11, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 19
  do i = 1, nummap
    if (list(i) .ne. exp_map11(i)) stop 67
  end do
  list(25) = 2
  call extpdstemplate(11, list, nummap, map)
  if (nummap .ne. 38) stop 110
  do i = 1, nummap
    if (map(i) .ne. exp_extmap11(i)) stop 111
  end do 
  ! Template number 12
  call getpdstemplate(12, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 20
  do i = 1, nummap
    if (list(i) .ne. exp_map12(i)) stop 68
  end do
  list(24) = 2
  call extpdstemplate(12, list, nummap, map)
  if (nummap .ne. 37) stop 112
  do i = 1, nummap
    if (map(i) .ne. exp_extmap12(i)) stop 113
  end do
  ! Template number 13
  call getpdstemplate(13, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 45 .or. .not. needext) stop 21
  do i = 1, nummap
    if (list(i) .ne. exp_map13(i)) stop 69
  end do
  list(38) = 2
  call extpdstemplate(13, list, nummap, map)
  if (nummap .ne. 52) stop 114
  do i = 1, nummap
    if (map(i) .ne. exp_extmap13(i)) stop 115
  end do
  ! Template number 14
  call getpdstemplate(14, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 44 .or. .not. needext) stop 22
  do i = 1, nummap
    if (list(i) .ne. exp_map14(i)) stop 70
  end do
  list(37) = 2
  call extpdstemplate(14, list, nummap, map)
  if (nummap .ne. 51) stop 116
  do i = 1, nummap
    if (map(i) .ne. exp_extmap14(i)) stop 117
  end do 
  ! Template number 30
  call getpdstemplate(30, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 23
  do i = 1, nummap
    if (list(i) .ne. exp_map30(i)) stop 71
  end do
  call extpdstemplate(30, list, nummap, map)
  if (nummap .ne. 10) stop 118
  do i = 1, nummap
    if (map(i) .ne. exp_extmap30(i)) stop 119
  end do
  ! Template number 31
  call getpdstemplate(31, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 24
  do i = 1, nummap
    if (list(i) .ne. exp_map31(i)) stop 71
  end do
  call extpdstemplate(31, list, nummap, map)
  if (nummap .ne. 10) stop 120
  do i = 1, nummap
    if (map(i) .ne. exp_extmap31(i)) stop 121
  end do
  ! Template number 32
  call getpdstemplate(32, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 10 .or. .not. needext) stop 25
  do i = 1, nummap
    if (list(i) .ne. exp_map32(i)) stop 72
  end do
  call extpdstemplate(32, list, nummap, map)
  if (nummap .ne. 15) stop 122
  do i = 1, nummap
    if (map(i) .ne. exp_extmap32(i)) stop 123
  end do
  ! Template number 33
  call getpdstemplate(33, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 18 .or. .not. needext) stop 26
  do i = 1, nummap
    if (list(i) .ne. exp_map33(i)) stop 73
  end do
  call extpdstemplate(33, list, nummap, map)
  if (nummap .ne. 19) stop 124
  do i = 1, nummap
    if (map(i) .ne. exp_extmap33(i)) stop 125
  end do
  ! Template number 34
  call getpdstemplate(34, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 27
  do i = 1, nummap
    if (list(i) .ne. exp_map34(i)) stop 74
  end do
  list(25) = 2
  call extpdstemplate(34, list, nummap, map)
  if (nummap .ne. 39) stop 126
  do i = 1, nummap
    if (map(i) .ne. exp_extmap34(i)) stop 127
  end do
  ! Template number 42
  call getpdstemplate(42, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 28
  do i = 1, nummap
    if (list(i) .ne. exp_map42(i)) stop 75
  end do
  list(23) = 2
  call extpdstemplate(42, list, nummap, map)
  if (nummap .ne. 36) stop 128
  do i = 1, nummap
    if (map(i) .ne. exp_extmap42(i)) stop 129
  end do
  ! Template number 43
  call getpdstemplate(43, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 33 .or. .not. needext) stop 29
  do i = 1, nummap
    if (list(i) .ne. exp_map43(i)) stop 76
  end do
  list(26) = 2
  call extpdstemplate(43, list, nummap, map)
  if (nummap .ne. 39) stop 130
  do i = 1, nummap
    if (map(i) .ne. exp_extmap43(i)) stop 131
  end do
  ! Template number 46
  call getpdstemplate(46, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 35 .or. .not. needext) stop 30
  do i = 1, nummap
    if (list(i) .ne. exp_map46(i)) stop 77
  end do
  list(28) = 2
  call extpdstemplate(46, list, nummap, map)
  if (nummap .ne. 41) stop 132
  do i = 1, nummap
    if (map(i) .ne. exp_extmap46(i)) stop 133
  end do
  ! Template number 47
  call getpdstemplate(47, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 38 .or. .not. needext) stop 31
  do i = 1, nummap
    if (list(i) .ne. exp_map47(i)) stop 78
  end do
  list(31) = 2
  call extpdstemplate(47, list, nummap, map)
  if (nummap .ne. 44) stop 134
  do i = 1, nummap
    if (map(i) .ne. exp_extmap47(i)) stop 135
  end do
  ! Template number 51
  call getpdstemplate(51, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. .not. needext) stop 32
  do i = 1, nummap
    if (list(i) .ne. exp_map51(i)) stop 79
  end do
  call extpdstemplate(51, list, nummap, map)
  if (nummap .ne. 22) stop 136
  do i = 1, nummap
    if (map(i) .ne. exp_extmap51(i)) stop 137
  end do
  ! Template number 53
  call getpdstemplate(53, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 19 .or. .not. needext) stop 33
  do i = 1, nummap
    if (list(i) .ne. exp_map53(i)) stop 80
  end do
  call extpdstemplate(53, list, nummap, map)
  if (nummap .ne. 20) stop 138
  do i = 1, nummap
    if (map(i) .ne. exp_extmap53(i)) stop 139
  end do
  ! Template number 54
  call getpdstemplate(54, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 22 .or. .not. needext) stop 34
  do i = 1, nummap
    if (list(i) .ne. exp_map54(i)) stop 81
  end do
  call extpdstemplate(54, list, nummap, map)
  if (nummap .ne. 23) stop 140
  do i = 1, nummap
    if (map(i) .ne. exp_extmap54(i)) stop 141
  end do
  ! Template number 91
  call getpdstemplate(91, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 35
  do i = 1, nummap
    if (list(i) .ne. exp_map91(i)) stop 82
  end do
  list(29) = 2
  call extpdstemplate(91, list, nummap, map)
  if (nummap .ne. 43) stop 142
  do i = 1, nummap
    if (map(i) .ne. exp_extmap91(i)) stop 143
  end do
  ! Template number 57
  call getpdstemplate(57, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 7 .or. .not. needext) stop 36
  do i = 1, nummap
    if (list(i) .ne. exp_map57(i)) stop 83
  end do
  call extpdstemplate(57, list, nummap, map)
  if (nummap .ne. 22) stop 144
  do i = 1, nummap
    if (map(i) .ne. exp_extmap57(i)) stop 145
  end do
  ! Template number 61
  call getpdstemplate(61, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 38 .or. .not. needext) stop 37
  do i = 1, nummap
    if (list(i) .ne. exp_map61(i)) stop 84
  end do
  list(31) = 2
  call extpdstemplate(61, list, nummap, map)
  if (nummap .ne. 44) stop 146
  do i = 1, nummap
    if (map(i) .ne. exp_extmap61(i)) stop 147
  end do
  ! Template number 35
  call getpdstemplate(35, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 6 .or. .not. needext) stop 38
  do i = 1, nummap
    if (list(i) .ne. exp_map35(i)) stop 85
  end do
  call extpdstemplate(35, list, nummap, map)
  if (nummap .ne. 11) stop 148
  do i = 1, nummap
    if (map(i) .ne. exp_extmap35(i)) stop 149
  end do  

  print *, 'SUCCESS'
end program test_pdstemplates
