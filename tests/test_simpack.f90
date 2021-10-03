! This program tests the simple packing subroutine of the NCEPLIBS-g2
! project. Link this to the _4 build of the library.
!
! Ed Hartnett 10/3/21
program test_simpack
  implicit none

  integer, parameter :: ndpts = 4
  real(4) :: fld(ndpts)
  integer :: idrstmpl(5)
  character, dimension(10) :: cpack
  character, dimension(4) :: expected_cpack
  integer :: lcpack
  integer :: i

  print *, 'Testing simpack.'
  expected_cpack = (/ char(0), char(93), char(108), char(64) /)
!  expected_cpack(1) = char(0)

  print *, 'Testing simple call to simpack...'
  fld = (/ 42.3, 43.2, 44.1, 45.0/)
  idrstmpl = (/ 42, 2, 2, 0, 0/)
  call simpack(fld, ndpts, idrstmpl, cpack, lcpack)
!  print *, 'lcpack: ', lcpack
  do i = 1, lcpack
!     print *, ichar(cpack(i))
     if (cpack(i) .ne. expected_cpack(i)) stop 3
  end do
  if (lcpack .ne. 4) stop 2
  
  
  print *, 'SUCCESS!'


end program test_simpack
