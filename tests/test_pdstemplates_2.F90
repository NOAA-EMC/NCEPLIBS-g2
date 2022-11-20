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
  integer, dimension(15) :: map_comp, list
  integer, dimension(MAXLEN) :: map
  !integer, dimension(MAXLEN) :: map1
  logical :: needext
  integer :: pdtlen

  print *, 'Testing pdstemplates complete contents...'

  print *, 'testing getpdstemplate()...'  
  call getpdstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0) stop 11
  if (nummap .ne. 15) stop 12
  if (needext) stop 13

  print *, 'SUCCESS'
end program test_pdstemplates
