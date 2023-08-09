! This program tests the specpack subroutine of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 7/31/23
program test_specpack
  implicit none
#ifdef KIND_4
  integer, parameter :: width = 2, height = 2, ndpts = 4
  real, parameter :: delta = 0.2
  real :: fld(ndpts*2)
  real :: fld2(ndpts*2)
  integer :: idrstmpl(10)
  character*1 :: cpack(200)
  integer :: lcpack = 200
  integer :: ii
  integer :: jj, kk, mm
  

  ! Create the fld variable with data to pack.
  fld = (/1.1, 2.2, 3.3, 4.4, 1.1, 2.2, 3.3, 4.4/)
  fld2 = (/0, 0, 0, 0, 0, 0, 0, 0/)

  idrstmpl(1) = 0
  idrstmpl(2) = 1
  idrstmpl(3) = 1
  idrstmpl(4) = 32
  idrstmpl(5) = 1
  idrstmpl(6) = 1
  idrstmpl(7) = 1
  idrstmpl(8) = 0
  idrstmpl(9) = ndpts
  idrstmpl(10) = 1

  ! Pack the data.
  jj = 1
  kk = 1
  mm = 1
  call specpack(fld, ndpts, jj, kk, mm, idrstmpl, cpack, lcpack)
  print *, 'lcpack: ', lcpack

  ! Unpack the data.
  call specunpack(cpack, lcpack, idrstmpl, ndpts, jj, kk, mm, fld2)

  ! Compare each value to see match, remember, comparing reals
!  print *, 'fld  ', fld
!  print *, 'fld2 ', fld2
  do ii = 1, ndpts
     if (abs(fld(ii) - fld2(ii)) .gt. delta) then
        print *, fld(ii), fld2(ii), 'do not match'
        stop 4
     end if
  end do

  print *, 'SUCCESS!'
#endif
end program test_specpack
