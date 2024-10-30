! This is a test for the NCEPLIBS-g2 project.
!
! This program tests pdstemplates.F90.
!
! Brian Curtis 11/09/2021, Ed Hartnett
program test_pdstemplates
  use pdstemplates
  implicit none

  integer :: nummap
  integer, dimension(MAXLEN) :: map
  integer, dimension(15) :: expected_map_0 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(18) :: expected_map_1 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1 /)
  integer, dimension(17) :: expected_map_2 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1 /)
  integer, dimension(31) :: expected_map_3 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, &
       1, 1, 1, -4, -4, 4, 4, 1, -1, 4, -1, 4 /)
  integer, dimension(30) :: expected_map_4 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, &
       1, 1, 1, -4, 4, 4, 1, -1, 4, -1, 4 /)
  integer, dimension(22) :: expected_map_5 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, &
       -4, -1, -4 /)
  integer, dimension(16) :: expected_map_6 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1 /)
  integer, dimension(15) :: expected_map_7 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(29) :: expected_map_8 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, &
       1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(36) :: expected_map_9 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, &
       -1, -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(30) :: expected_map_10 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 2, 1, 1, 1, 1, &
       1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(32) :: expected_map_11 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, &
       1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(31) :: expected_map_12 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 2, 1, 1, 1, 1, &
       1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(45) :: expected_map_13 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, &
       1, -4, -4, 4, 4, 1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(44) :: expected_map_14 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, &
       1, -4, 4, 4, 1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(19) :: expected_map_20 = (/ 1, 1, 1, 1, 1, -4, 4, 2, 4, 2, 1, 1, 1, 1, 1, 2, 1, 3, 2 /)
  integer, dimension(5) :: expected_map_30 = (/ 1, 1, 1, 1, 1 /)
  integer, dimension(3) :: expected_map_254 = (/ 1, 1, 4 /)
  integer, dimension(9) :: expected_map_1000 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4 /)
  integer, dimension(16) :: expected_map_1001 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(15) :: expected_map_1002 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 1, 1, 4, 4, 2 /)
  integer, dimension(15) :: expected_map_1100 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(22) :: expected_map_1101 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(5) :: expected_map_31 = (/ 1, 1, 1, 1, 1 /)
  integer, dimension(18) :: expected_map_15 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1 /)
  integer, dimension(16) :: expected_map_40 = (/ 1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(19) :: expected_map_41 = (/ 1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1 /)
  integer, dimension(30) :: expected_map_42 = (/ 1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 2, 1, 1, 1, 1, 1, &
       1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(33) :: expected_map_43 = (/ 1, 1, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, &
       1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(21) :: expected_map_44 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -2, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(24) :: expected_map_45 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, &
       1, 1, 1 /)
  integer, dimension(35) :: expected_map_46 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, &
       2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(38) :: expected_map_47 = (/ 1, 1, 1, 2, 1, -1, -4, -1, -4, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, &
       1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(21) :: expected_map_50 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 4, 4, 4, 4 /)
  integer, dimension(16) :: expected_map_51 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1 /)
  integer, dimension(36) :: expected_map_91 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, &
       -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(10) :: expected_map_32 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1 /)
  integer, dimension(26) :: expected_map_48 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, &
       -1, -4, 1, -1, -4 /)
  integer, dimension(29) :: expected_map_49 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, &
       -1, -4, 1, -1, -4, 1, 1, 1 /)
!  integer, dimension(21) :: expected_map_40 = (/ 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, -1, -4, 1, -1, -4, 1, 1, 4, 4, 4, 4 /)
  integer, dimension(15) :: expected_map_52 = (/ 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4 /)
  integer, dimension(18) :: expected_map_33 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1 /)
  integer, dimension(32) :: expected_map_34 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, &
       1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(19) :: expected_map_53 = (/ 1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(22) :: expected_map_54 = (/ 1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1 /)
  integer, dimension(7)  :: expected_map_57 = (/ 1, 1, 2, 2, 2, 2, 1 /)
  integer, dimension(24) :: expected_map_60 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1 /)
  integer, dimension(38) :: expected_map_61 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, & 
       1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(6)  :: expected_map_35 = (/ 1, 1, 1, 1, 1, 1 /)

  logical :: needext
  integer :: m
  integer :: iret

  print *, 'Testing pdstemplates complete contents...'

  print *, 'testing getpdstemplate()...'  
  call getpdstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 15 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_0(m)) stop 100
  end do

  call getpdstemplate(1, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 18 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1(m)) stop 100
  end do

  call getpdstemplate(2, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 17 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_2(m)) stop 100
  end do

  call getpdstemplate(3, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_3(m)) stop 100
  end do

  call getpdstemplate(4, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_4(m)) stop 100
  end do

  call getpdstemplate(5, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 22 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_5(m)) stop 100
  end do

  call getpdstemplate(6, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_6(m)) stop 100
  end do

  call getpdstemplate(7, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 15 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_7(m)) stop 100
  end do

  call getpdstemplate(8, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 29 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_8(m)) stop 100
  end do

  call getpdstemplate(9, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_9(m)) stop 100
  end do

  call getpdstemplate(10, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_10(m)) stop 100
  end do

  call getpdstemplate(11, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_11(m)) stop 100
  end do

  call getpdstemplate(12, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_12(m)) stop 100
  end do

  call getpdstemplate(13, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 45 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_13(m)) stop 100
  end do

  call getpdstemplate(14, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 44 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_14(m)) stop 100
  end do

  call getpdstemplate(20, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 19 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_20(m)) stop 100
  end do

  call getpdstemplate(30, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_30(m)) stop 100
  end do

  call getpdstemplate(254, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 3 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_254(m)) stop 100
  end do

  call getpdstemplate(1000, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 9 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1000(m)) stop 100
  end do

  call getpdstemplate(1001, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1001(m)) stop 100
  end do

  call getpdstemplate(1002, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 15 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1002(m)) stop 100
  end do

  call getpdstemplate(1100, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 15 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1100(m)) stop 100
  end do

  call getpdstemplate(1101, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 22 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_1101(m)) stop 100
  end do

  call getpdstemplate(31, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_31(m)) stop 100
  end do

  call getpdstemplate(15, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 18 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_15(m)) stop 100
  end do

  call getpdstemplate(40, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_40(m)) stop 100
  end do

  call getpdstemplate(41, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 19 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_41(m)) stop 100
  end do

  call getpdstemplate(42, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_42(m)) stop 100
  end do

  call getpdstemplate(43, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 33 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_43(m)) stop 100
  end do

  call getpdstemplate(44, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 21 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_44(m)) stop 100
  end do

  call getpdstemplate(45, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 24 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_45(m)) stop 100
  end do

  call getpdstemplate(46, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 35 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_46(m)) stop 100
  end do

  call getpdstemplate(47, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 38 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_47(m)) stop 100
  end do

  call getpdstemplate(51, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_51(m)) stop 100
  end do

  call getpdstemplate(91, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_91(m)) stop 100
  end do

  call getpdstemplate(32, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 10 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_32(m)) stop 100
  end do

  call getpdstemplate(48, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 26 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_48(m)) stop 100
  end do

  call getpdstemplate(49, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 29 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_49(m)) stop 100
  end do

  call getpdstemplate(50, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 21 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_50(m)) stop 100
  end do

  call getpdstemplate(52, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 15 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_52(m)) stop 100
  end do

  call getpdstemplate(33, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 18 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_33(m)) stop 100
  end do

  call getpdstemplate(34, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_34(m)) stop 100
  end do

  call getpdstemplate(53, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 19 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_53(m)) stop 100
  end do

  call getpdstemplate(54, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 22 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_54(m)) stop 100
  end do

  call getpdstemplate(57, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 7 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_57(m)) stop 100
  end do

  call getpdstemplate(60, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 24 .or. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_60(m)) stop 100
  end do
  
  call getpdstemplate(61, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 38 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_61(m)) stop 100
  end do

  call getpdstemplate(35, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 6 .or. .not. needext) stop 99
  do m = 1, nummap
     if (map(m) .ne. expected_map_35(m)) stop 100
  end do

  print *, 'SUCCESS'
end program test_pdstemplates
