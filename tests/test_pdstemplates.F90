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
  integer, dimension(15) :: map_comp
  integer, dimension(MAXLEN) :: map, list
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
  map_comp = (/ 1, 1, 1, 1, 1, 2, 1, 1, -4, 1, -1, -4, 1, -1, -4/)
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
  if (nummap .ne. 0) stop 12
  if (needext) stop 13

  print *, 'testing extpdstemplate()...'
  list = 0
  map = 0
  print *, 'Index = -1' ! -- returns without doing anything'
  call extpdstemplate(-1, list, nummap, map)
  print *, 'Template without map extension (needext = false)'
  ! -- returns without doing anything
  call extpdstemplate(0, list, nummap, map)
  print *, 'Testing the extendable tempaltes'
  ! ---- still need to test output i.e. nummap and map ----
  ! Template number 3
  call getpdstemplate(3, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 14
  call extpdstemplate(3, list, nummap, map)
  ! Template number 4
  call getpdstemplate(4, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 15
  call extpdstemplate(4, list, nummap, map)
  ! Template number 8
  call getpdstemplate(8, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 29 .or. .not. needext) stop 16
  call extpdstemplate(8, list, nummap, map)
  if (list(22) .le. 1) then
    list(22) = 2
  end if
  call extpdstemplate(8, list, nummap, map)
  ! Template number 9
  call getpdstemplate(9, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 17
  call extpdstemplate(9, list, nummap, map)
  if (list(29) .le. 1) then
    list(29) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 10
  call getpdstemplate(10, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 18
  call extpdstemplate(10, list, nummap, map)
  if (list(23) .le. 1) then
    list(23) = 2
  end if
  call extpdstemplate(10, list, nummap, map)
  ! Template number 11
  call getpdstemplate(11, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 19
  call extpdstemplate(11, list, nummap, map)
  if (list(25) .le. 1) then
    list(25) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 12
  call getpdstemplate(12, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 31 .or. .not. needext) stop 20
  call extpdstemplate(12, list, nummap, map)
  if (list(24) .le. 1) then
    list(24) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 13
  call getpdstemplate(13, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 45 .or. .not. needext) stop 21
  call extpdstemplate(13, list, nummap, map)
  if (list(38) .le. 1) then
    list(38) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 14
  call getpdstemplate(14, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 44 .or. .not. needext) stop 22
  call extpdstemplate(14, list, nummap, map)
  if (list(37) .le. 1) then
    list(37) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 30
  call getpdstemplate(30, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 23
  call extpdstemplate(30, list, nummap, map)
  ! Template number 31
  call getpdstemplate(31, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. .not. needext) stop 24
  call extpdstemplate(31, list, nummap, map)
  ! Template number 32
  call getpdstemplate(32, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 10 .or. .not. needext) stop 25
  call extpdstemplate(32, list, nummap, map)
  ! Template number 33
  call getpdstemplate(33, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 18 .or. .not. needext) stop 26
  call extpdstemplate(33, list, nummap, map)
  ! Template number 34
  call getpdstemplate(34, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 32 .or. .not. needext) stop 27
  call extpdstemplate(34, list, nummap, map)
  if (list(25) .le. 1) then
    list(25) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 42
  call getpdstemplate(42, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 30 .or. .not. needext) stop 28
  call extpdstemplate(42, list, nummap, map)
  if (list(23) .le. 1) then
    list(23) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 43
  call getpdstemplate(43, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 33 .or. .not. needext) stop 29
  call extpdstemplate(43, list, nummap, map)
  if (list(26) .le. 1) then
    list(26) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 46
  call getpdstemplate(46, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 35 .or. .not. needext) stop 30
  call extpdstemplate(46, list, nummap, map)
  if (list(28) .le. 1) then
    list(28) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 47
  call getpdstemplate(47, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 38 .or. .not. needext) stop 31
  call extpdstemplate(47, list, nummap, map)
  if (list(31) .le. 1) then
    list(31) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  ! Template number 51
  call getpdstemplate(51, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 16 .or. .not. needext) stop 32
  call extpdstemplate(51, list, nummap, map)
  ! Template number 53
  call getpdstemplate(53, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 19 .or. .not. needext) stop 33
  call extpdstemplate(53, list, nummap, map)
  ! Template number 54
  call getpdstemplate(54, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 22 .or. .not. needext) stop 34
  call extpdstemplate(54, list, nummap, map)
  ! Template number 91
  call getpdstemplate(91, nummap, list, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 36 .or. .not. needext) stop 35
  call extpdstemplate(91, list, nummap, map)
  if (list(29) .le. 1) then
    list(29) = 2
  end if
  call extpdstemplate(9, list, nummap, map)
  

  
  
  print *, 'SUCCESS'
end program test_pdstemplates
