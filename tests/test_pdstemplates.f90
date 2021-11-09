! Test pdstemplates
! Brian Curtis 11/09/2021
program test_pdstemplates
  use pdstemplates
  implicit none

  integer :: idx
  integer :: nummap
  integer :: iret, i
  integer, dimension(15) :: map, map_comp, list
  logical :: needext
  integer :: pdtlen

  print *, 'Testing pdstemplates'
  print *, 'Testing getpdsindex() ...'
  ! Fortran is base 1, so index 0 should = 1
  idx = getpdsindex(0)
  if (idx .ne. 1) stop 3

  ! Index -1 will still equal -1 because it doesn't exist
  idx = getpdsindex(-1)
  if (idx .ne. -1) stop 4
  print *, 'OK!'

  print *, 'testing getpdstemplate() ...'
  print *, 'testing with template 0...'
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
  print *, 'OK!'

  print *, 'testing with template -1 (nonexistent)...'
  pdtlen = getpdtlen(-1)
  print *, 'pdtlen(index(-1)):', pdtlen
  if (pdtlen .ne. 0) stop 10
  call getpdstemplate(-1, nummap, map, needext, iret)
  if (iret .eq. 0) stop 11
  if (pdtlen .ne. nummap) stop 12
  if (needext) stop 13
  print *, 'OK!'

  print *, 'testing extpdstemplate(0) ...'
  call extpdstemplate(0, list, nummap, map)
  if (nummap .ne. 0) stop 14

  print *, 'SUCCESS'
end program test_pdstemplates
