! This program tests the simple packing subroutine of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 10/3/21
program test_simpack
  implicit none

  integer, parameter :: ndpts = 4
  real :: fld(ndpts)
  integer :: idrstmpl(5)
  character, dimension(10) :: cpack
  integer :: lcpack

  print *, 'Testing simpack.'

  print *, 'Testing simple call to simpack...'
  fld = (/ 42.3, 43.2, 44.1, 45.0/)
  idrstmpl = (/ 42, 2, 2, 0, 0/)
  call simpack(fld, ndpts, idrstmpl, cpack, lcpack)
  print '(Z4)', cpack(1)
  print *, lcpack
  
  print *, 'SUCCESS!'
  
end program test_simpack
