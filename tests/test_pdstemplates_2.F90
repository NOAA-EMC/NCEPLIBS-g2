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
  integer, dimension(31) :: expected_map_12 = (/ 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, -1, -4, 1, -1, -4, 1, 1, 2, 1, 1, 1, 1, &
       1, 1, -4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(45) :: expected_map_13 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, &
       1, -4, -4, 4, 4, 1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(44) :: expected_map_14 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, 1, 1, 1, &
       1, -4, 4, 4, 1, -1, 4, -1, 4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(19) :: expected_map_20 = (/ 1, 1, 1, 1, 1, -4, 4, 2, -4, 2, 1, 1, 1, 1, 1, 2, 1, 3, 2 /)
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
  integer, dimension(16) :: expected_map_51 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1 /)
  integer, dimension(36) :: expected_map_91 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1, -1, -4, -1, &
       -4, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(10) :: expected_map_32 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1 /)
  integer, dimension(26) :: expected_map_48 = (/ 1, 1, 2, 1, -1, -4, -1, -4, 1, -1, -4, -1, -4, 1, 1, 1, 2, 1, 1, -4, 1, &
       -1, -4, 1, -1, -4 /)
!  integer, dimension(21) :: expected_map_40 = (/ 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, -1, -4, 1, -1, -4, 1, 1, 4, 4, 4, 4 /)
  integer, dimension(15) :: expected_map_52 = (/ 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, -1, -4 /)
  integer, dimension(18) :: expected_map_33 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1 /)
  integer, dimension(32) :: expected_map_34 = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, 2, 2, 2, -1, -4, 1, 1, 1, 2, 1, 1, 1, 1, 1, &
       1, 4, 1, 1, 1, 4, 1, 4 /)
  integer, dimension(19) :: expected_map_53 = (/ 1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4 /)
  integer, dimension(22) :: expected_map_54 = (/ 1, 1, 1, 1, 4, 2, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4, 1, 1, 1 /)
  logical :: needext
  integer :: m
  integer :: iret

  print *, 'Testing pdstemplates complete contents...'

  print *, 'testing getpdstemplate()...'  
  call getpdstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0) stop 11
  if (nummap .ne. 15) stop 12
  if (needext) stop 13
  do m = 1, nummap
     if (map(m) .ne. expected_map_0(m)) then
        print *, map(m), ' expected ', expected_map_0(m), ' m ', m
        stop 15
     end if
  end do

  print *, 'SUCCESS'
end program test_pdstemplates
