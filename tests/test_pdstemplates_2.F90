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
  integer, dimension(15) :: expected_map = (/1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4/)
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
     if (map(m) .ne. expected_map(m)) then
        print *, map(m), ' expected ', expected_map(m), ' m ', m
        stop 15
     end if
  end do

  print *, 'SUCCESS'
end program test_pdstemplates
