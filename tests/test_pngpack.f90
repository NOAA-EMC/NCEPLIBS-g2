! This program tests the pngpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-04
program test_pngpack

  implicit none

  integer :: width=5
  integer :: height=5
  integer :: len=25, ndpts=25
  real, dimension(25):: fld
  real, dimension(25) :: fld2
  integer :: idrstmpl(7)
  character(len=1), dimension(25) :: cpack
  integer :: lcpack
  integer :: i

  ! Create the fld variable with data to pack
  fld = (/0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4/)

  ! No idea what this needs to be, documentation confusing
  idrstmpl = 0000000
  print *, 'Testing pngpack/unpack'
  call pngpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'Call to pngpack successful'
  call pngunpack(cpack, len, idrstmpl, ndpts, fld2)
  print *, 'fld2 is:', fld2
  do i = 1, ndpts
   if (abs(fld2(i) - fld(i)) .gt. .1) stop 10
  end do
  !if (fld2 .ne. fld) stop 3

  print *, 'SUCCESS!'

end program test_pngpack
