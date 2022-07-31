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
  map_comp = (/ 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, -1, -4, 1, -1, -4/)
  call getpdstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0) stop 6
  if (pdtlen .ne. nummap) stop 7
  do i = 1, nummap
      if (map(i) .ne. map_comp(i)) stop 8
  end do
  if (needext) stop 9

  print *, 'testing getpdtlen() with template -1 (nonexistent)...'
  pdtlen = getpdtlen(-1)
  if (pdtlen .ne. 0) stop 10

  print *, 'testing getpdstemplate() with template -1 (nonexistent)...'  
  call getpdstemplate(-1, nummap, map, needext, iret)
  if (iret .eq. 0) stop 11
  if (pdtlen .ne. nummap) stop 12
  if (needext) stop 13

  print *, 'testing extpdstemplate()...'
  call extpdstemplate(0, list, nummap, map)
  if (nummap .ne. 0) stop 14

  ! print *, 'testing extpdstemplate() some more...'
  ! pdtlen = getpdtlen(3)
  ! if (pdtlen .ne. 31) stop 5
  ! call getpdstemplate(3, nummap, map, needext, iret)
  ! if (iret .ne. 0) stop 6
  ! if (nummap .ne. 31) stop 20
  ! call extpdstemplate(3, map, nummap, map1)
  ! print *, 'nummap = ', nummap
  ! if (nummap .ne. 32) stop 20
  
  print *, 'SUCCESS'
end program test_pdstemplates