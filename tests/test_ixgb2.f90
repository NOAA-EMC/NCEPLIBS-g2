! This is a test program for NCEPLIBS-g2.
!
! This program tests ixgb2.F90
!
! Ed Hartnett 7/22/22
program test_ixgb2
  use bacio_module
  implicit none

  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lskip, lgrib, numfld, mlen, iret

  interface
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer lugb, lskip, lgrib, numfld, mlen, iret
       character(len = 1),pointer,dimension(:) :: cbuf
     end subroutine ixgb2
  end interface

  ! This will not work, because it's not a GRIB2 file.
  call baopenr(lugi, "testdata_g2grids", iret)
  if (iret .ne. 0) stop 3
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 3) stop 4
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  
  ! Now open a real GRIB2 file.
  call baopenr(lugi, "WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  ! Skip the first 202 bytes of the test file.
  lskip = 202
  lgrib = 1000
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 0) stop 101
  if (numfld .ne. 1 .or. mlen .ne. 200) stop 102
  deallocate(cbuf)
  
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 200

  print *, 'SUCCESS!...'

end program test_ixgb2
