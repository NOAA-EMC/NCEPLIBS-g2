! This is a test program for NCEPLIBS-g2.
!
! This program tests ixgb2.F90
!
! Ed Hartnett 7/22/22
program test_ixgb2
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

  ! THis is the index record for the first GRIB message in the test file.
  expected_cbuf(:) = (/ &
       char(0), char(0), char(0), char(200), char(0), char(0), char(0), char(202), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(0), char(37), char(0), char(0), char(0), &
       char(109), char(0), char(0), char(0), char(143), char(0), char(0), char(0), char(166), &
       char(0), char(0), char(18), char(113), char(0), char(0), char(0), char(0), char(0), &
       char(0), char(43), char(175), char(2), char(10), char(0), char(1), char(0), char(0), &
       char(0), char(21), char(1), char(0), char(7), char(0), char(0), char(2), char(1), &
       char(1), char(7), char(230), char(7), char(18), char(0), char(0), char(0), char(0), &
       char(1), char(0), char(0), char(0), char(72), char(3), char(0), char(0), char(0), &
       char(142), char(39), char(0), char(0), char(0), char(0), char(6), char(0), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(241), char(0), &
       char(0), char(0), char(151), char(0), char(0), char(0), char(0), char(0), char(0), &
       char(0), char(0), char(2), char(250), char(240), char(128), char(12), char(132), &
       char(88), char(128), char(48), char(1), char(125), char(120), char(64), char(14), &
       char(230), char(178), char(128), char(0), char(2), char(139), char(11), char(0), &
       char(2), char(139), char(11), char(0), char(0), char(0), char(0), char(34), char(4), &
       char(0), char(0), char(0), char(0), char(0), char(5), char(2), char(0), char(11), &
       char(0), char(0), char(0), char(1), char(0), char(0), char(0), char(0), char(1), &
       char(0), char(0), char(0), char(0), char(1), char(255), char(0), char(0), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(23), char(5), char(0), char(0), &
       char(33), char(36), char(0), char(40), char(65), char(136), char(0), char(0), char(0), &
       char(0), char(0), char(2), char(9), char(0), char(0), char(255), char(0), char(0), &
       char(17), char(203), char(6), char(0) /)       

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
  do i = 1, mlen
!     print *,'char(',ichar(cbuf(i)),'), '
     if (cbuf(i) .ne. expected_cbuf(i)) stop 30
  end do
   
  ! Free allocated memory
  deallocate(cbuf)

  call baclose(lugi, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 5
  end if
  print *, 'Success!...'

end program test_ixgb2
