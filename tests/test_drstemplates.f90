! This program tests the drstemplates module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 10/1/21
program test_drstemplates
  use drstemplates
  implicit none

  integer :: index, nummap, iret
  logical :: needext
  integer, parameter :: max_map = 5
  integer :: map(max_map)
  integer :: list(1)
  integer :: map1(max_map)

  print *, 'Testing the drstemplates module.'

  print *, 'Testing getdrsindex...'
  index = getdrsindex(0)
  if (index .ne. 1) stop 2
  index = getdrsindex(41)
  if (index .ne. 9) stop 3
  
  print *, 'Testing getdrstemplate...'
  call getdrstemplate(0, nummap, map, needext, iret)
  if (iret .ne. 0 .or. nummap .ne. 5 .or. needext) stop 4
  if (map(1) .ne. 4 .or. map(2) .ne. -2 .or. map(3) .ne. -2 .or. &
       map(4) .ne. 1 .or. map(5) .ne. 1) stop 5
  

  ! Currenrly no drstemplates have needext=.true.
  print *, 'Testing extdrstemplate...'
  call extdrstemplate(0, list, nummap, map1)
  
  print *, 'SUCCESS!'
  
end program test_drstemplates
