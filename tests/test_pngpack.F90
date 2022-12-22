! This program tests the pngpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-04
program test_pngpack

  implicit none

  integer, parameter :: width=2, height=2, ndpts=4
  real, parameter :: delta = 0.0000001
  real :: fld(ndpts), fld2(ndpts)
  integer :: idrstmpl(7)
  character(len=1) :: cpack(100)
  integer :: lcpack = 100
  integer :: i

  print *, 'Testing pngpack()/pngunpack()...'

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

  ! Testing pngpack
  call pngpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'lcpack: ', lcpack

  ! Testing pngunpack
  call pngunpack(cpack, lcpack, idrstmpl, ndpts, fld2)

  ! Compare each value to see match, reals do not compare well
  do i = 1, ndpts
     if (abs(fld(i) - fld2(i)) .ge. delta) then
        print *, fld(i), fld2(i), 'do not match'
        stop 4
     end if
  end do

  print *, 'SUCCESS!'

end program test_pngpack
