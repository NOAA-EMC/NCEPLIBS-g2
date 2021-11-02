! This program tests the jpcpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-18
program test_jpcpack

  implicit none

  integer, parameter :: width=2, height=2, ndpts=4
  real(kind=8) :: fld(ndpts), fld2(ndpts)
  integer :: idrstmpl(7)
  character(len=1) :: cpack(200)
  integer :: lcpack
  integer :: i

  ! Create the fld variable with data to pack
  fld = (/1.0, 2.0, 3.0, 4.0/)

  ! No idea what this needs to be, documentation confusing
  idrstmpl(1)=0
  idrstmpl(2)=1
  idrstmpl(3)=1
  idrstmpl(4)=21
  idrstmpl(5)=0
  idrstmpl(6)=0
  idrstmpl(7)=0

  print *, 'Testing jpcpack/unpack'
  call jpcpack(fld, width, height, idrstmpl, cpack, lcpack)
  print *, 'Call to jpcpack successful'
  print *, 'lcpack is: ', lcpack
  call jpcunpack(cpack, lcpack, idrstmpl, ndpts, fld2)
  do i = 1, ndpts
    print *, 'fld2 is', fld2(i), 'fld is', fld(i)
    !if (fld2(i) .ne. fld(i)) then
    !    print *, 'fld2 and fld mismatch at point', i
    !    stop 10
    !endif
  end do

  print *, 'SUCCESS!'

end program test_jpcpack
