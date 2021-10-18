! This program tests the pngpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-04
program test_pngpack

  implicit none

  integer :: width=3
  integer :: height=3
  integer :: ndpts=9
  integer(kind=8), dimension(9) :: fld
  integer(kind=8), dimension(9) :: fld2
  integer :: idrstmpl(7)
  character(len=1), dimension(9) :: cpack
  integer :: lcpack
  integer :: i, j

  ! Create the fld variable with data to pack
  fld = (/0, 0, 0, 1, 1, 1, 2, 2, 2/)

  do i = 1, 9
    print *, 'fld is', fld(i)
  end do
  ! No idea what this needs to be, documentation confusing
  idrstmpl(1)=41
  idrstmpl(2)=1
  idrstmpl(3)=1
  idrstmpl(4)=8
  idrstmpl(5)=1
  idrstmpl(6)=0
  idrstmpl(7)=0

  print *, 'Testing pngpack/unpack'
  call pngpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'Call to pngpack successful'
  print *, 'cpack is: ', cpack
  print *, 'lcpack is: ', lcpack
  call pngunpack(cpack, lcpack, idrstmpl, ndpts, fld2)
  do i = 1, 9
    print *, 'fld2 is', fld2(i), 'fld is', fld(i)
    if (fld2(i) .ne. fld(i)) then
        print *, 'fld2 and fld mismatch at point', i
        stop 10
    endif
  end do

  print *, 'SUCCESS!'

end program test_pngpack
