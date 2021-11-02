! This program tests the pngpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-04
program test_pngpack

  implicit none

  integer, parameter :: width=2, height=2, ndpts=4
  real(kind=8) :: fld(ndpts), fld2(ndpts)
  integer :: idrstmpl(7)
  character(len=1) :: cpack(100)
  integer :: lcpack
  integer :: i

  ! Create the fld variable with data to pack
  fld = (/1.0, 2.0, 3.0, 4.0/)

  ! No idea what this needs to be, documentation confusing
  idrstmpl(1)=0
  idrstmpl(2)=1
  idrstmpl(3)=1
  idrstmpl(4)=8
  idrstmpl(5)=0
  idrstmpl(6)=0
  idrstmpl(7)=1

  print *, 'Testing pngpack'
  call pngpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'Call to pngpack completed'
  print *, 'lcpack is: ', lcpack
  print *, 'Testing pngunpack'
  call pngunpack(cpack, lcpack, idrstmpl, ndpts, fld2)
  print *, 'Call to pngunpack completed'
  do i = 1, ndpts
    print *, fld(i), fld2(i)
  end do

  print *, 'SUCCESS!'

end program test_pngpack
