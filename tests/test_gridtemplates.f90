! This program tests the gridtemplates module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 9/30/21
program test_gridtemplates
  use gridtemplates
  implicit none

  integer :: index, t
  integer, dimension(MAXTEMP):: template_num = (/ 0, 1, 2, 3, 10, 20, &
       30, 40, 41, 42, 43, 50, 51, 52, 53, 90, 100, 110, 120, 1000, &
       1100, 1200, 31, 204, 32768, 32769, 4, 5, 12, 101, 140/)
  integer :: nummap
  integer, parameter :: lat_lon_mapgridlen = 19
  integer :: map(lat_lon_mapgridlen)
  logical :: needext
  integer :: gdtlen
  integer :: iret

  print *, 'Testing the gridtemplates module.'

  print *, 'Testing getgridindex...'
  do t = 1, MAXTEMP
     index = getgridindex(template_num(t))
     if (index .ne. t) stop 2
  enddo
  
  print *, 'Testing getgridtemplate...'
  call getgridtemplate(0, nummap, map, needext, iret)
  if (nummap .ne. lat_lon_mapgridlen .or. needext) stop 3

  print *, 'Testing getgdtlen...'
  gdtlen = getgdtlen(0)
  if (gdtlen .ne. lat_lon_mapgridlen) stop 4
  
  print *, 'SUCCESS!'
  
end program test_gridtemplates
