! This is a test program for NCEPLIBS-g2.
!
! This program tests ix2gb2.F90
!
! Ed Hartnett 5/9/24
program test_ix2gb2
  use bacio_module
  implicit none

  character(*) :: TEST_FILE_WW3_WEST
  parameter (TEST_FILE_WW3_WEST = 'data/WW3_Regional_US_West_Coast_20220718_0000.grib2')
  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  character :: expected_cbuf(200)
  integer :: lskip, lgrib, numfld, mlen, iret, i

  interface
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer lugb, lskip, lgrib, numfld, mlen, iret
       character(len = 1),pointer,dimension(:) :: cbuf
     end subroutine ixgb2
  end interface

  call baopenr(lugi, TEST_FILE_WW3_WEST, iret)
  if (iret .ne. 0) then
     print *, 'baopenr failed with iret value: ', iret
     stop 3
  end if

  ! This will return an error because lskip does not point to a valid
  ! GRIB message.
  lskip = 0
  lgrib = 11183
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 3) stop 11

  ! Free allocated memory
  deallocate(cbuf)

  ! These numbers come from test_skgb.F90, which finds the
  ! offsets/lengths of all GRIB messages in this test file.
  lskip = 202
  lgrib = 11183
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (numfld .ne. 1 .or. mlen .ne. 200 .or. iret .ne. 0) stop 20
  !print *,cbuf(1:mlen)
   
  ! Free allocated memory
  deallocate(cbuf)

  call baclose(lugi, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 5
  end if
  print *, 'Success!...'

end program test_ix2gb2
