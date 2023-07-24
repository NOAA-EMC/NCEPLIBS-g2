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
  integer :: map_120(11) = (/4, 4, -4, 4, 4, 4, 1, 2, -2, 2, -2/)
  integer :: map_1000(22) = (/ 1, 1, 4, 1, 4, 1, 4, 4, 4, 4, -4, 4, 1, 4, &
       4, 1, 2, 1, 1, 2, 4, 4/)
  integer :: map_1200(18) = (/ 4, 1, -4, 1, 1, -4, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 4, 4 /)
  integer :: nummap
  integer, parameter :: max_mapgridlen = 28
  integer :: map(max_mapgridlen)
  integer :: list_4(max_mapgridlen) 
  logical :: needext
  integer :: gdtlen
  integer :: i, iret

  print *, 'Testing the gridtemplates module.'

  print *, 'Testing getgridindex...'
  do t = 1, MAXTEMP
     index = getgridindex(template_num(t))
     if (index .ne. t) stop 2
  enddo
  
  print *, 'Testing getgridtemplate...'
  call getgridtemplate(0, nummap, map, needext, iret)
  if (nummap .ne. 19 .or. needext) stop 3

  print *, 'Testing getgridtemplate 4, which needs extension...'
  ! An extra 6 values are added to the map.
  call getgridtemplate(4, nummap, map, needext, iret)
  if (nummap .ne. 13 .or. .not. needext) stop 4
  list_4(8) = 2
  list_4(9) = 2
  call extgridtemplate(4, list_4, nummap, map)
  if (nummap .ne. 17) stop 5
  if (map(14) .ne. 4 .or. map(15) .ne. 4 .or. map(16) .ne. -4 .or. map(17) .ne. -4) stop 6
  
  print *, 'Testing getgridtemplate 5, which needs extension...'
  call getgridtemplate(5, nummap, map, needext, iret)
  if (nummap .ne. 16 .or. .not. needext) stop 40
  list_4(8) = 2
  list_4(9) = 2
  call extgridtemplate(5, list_4, nummap, map)
  if (nummap .ne. 20) stop 50
  if (map(17) .ne. 4 .or. map(18) .ne. 4 .or. map(19) .ne. -4 .or. map(20) .ne. -4) stop 60

  print *, 'Testing getgridtemplate 120, which needs extension...'
  call getgridtemplate(120, nummap, map, needext, iret)
  if (nummap .ne. 7 .or. .not. needext) stop 120
  list_4(2) = 2
  call extgridtemplate(120, list_4, nummap, map)
  !print *, 'nummap ', nummap, 'map: ', map
  if (nummap .ne. 11) stop 50
  do i = 1, 11
     if (map(i) .ne. map_120(i)) stop 120
  end do

  print *, 'Testing getgridtemplate 1000, which needs extension...'
  call getgridtemplate(1000, nummap, map, needext, iret)
  if (nummap .ne. 20 .or. .not. needext) stop 1000
  list_4(20) = 2
  call extgridtemplate(1000, list_4, nummap, map)
  !print *, 'nummap ', nummap, 'map: ', map
  if (nummap .ne. 22) stop 50
  do i = 1, 22
     if (map(i) .ne. map_1000(i)) stop 120
  end do

  print *, 'Testing getgridtemplate 1200, which needs extension...'
  call getgridtemplate(1200, nummap, map, needext, iret)
  if (nummap .ne. 16 .or. .not. needext) stop 1000
  list_4(16) = 2
  call extgridtemplate(1200, list_4, nummap, map)
  !print *, 'nummap ', nummap, 'map: ', map
  if (nummap .ne. 18) stop 50
  do i = 1, 18
     if (map(i) .ne. map_1200(i)) stop 120
  end do

  print *, 'Testing getgdtlen...'
  gdtlen = getgdtlen(0)
  if (gdtlen .ne. 19) stop 400

  print *, 'SUCCESS!'
  
end program test_gridtemplates
