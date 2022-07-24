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
  integer :: i

  interface
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer lugb, lskip, lgrib, numfld, mlen, iret
       character(len = 1),pointer,dimension(:) :: cbuf
     end subroutine ixgb2
  end interface

  ! This is the index we expect to be created from the test file.
  character :: expected_cbuf(200) = (/ char(0),  char(0),  char(0),  char(200),  char(0),  char(0),  char(0),  &
       char(202),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(37),  char(0),  &
       char(0),  char(0),  char(109),  char(0),  char(0),  char(0),  char(143),  char(0),  char(0),  char(0),  &
       char(166),  char(0),  char(0),  char(18),  char(113),  char(0),  char(0),  char(0),  char(0),  char(0),  &
       char(0),  char(0),  char(96),  char(2),  char(10),  char(0),  char(1),  char(0),  char(0),  char(0),  &
       char(21),  char(1),  char(0),  char(7),  char(0),  char(0),  char(2),  char(1),  char(1),  char(7),  &
       char(230),  char(7),  char(18),  char(0),  char(0),  char(0),  char(0),  char(1),  char(0),  char(0),  &
       char(0),  char(72),  char(3),  char(0),  char(0),  char(0),  char(142),  char(39),  char(0),  char(0),  &
       char(0),  char(0),  char(6),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  &
       char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  char(0),  &
       char(0),  char(241),  char(0),  char(0),  char(0),  char(151),  char(0),  char(0),  char(0),  char(0),  &
       char(0),  char(0),  char(0),  char(0),  char(2),  char(250),  char(240),  char(128),  char(12),  char(132),  &
       char(88),  char(128),  char(48),  char(1),  char(125),  char(120),  char(64),  char(14),  char(230),  char(178),  &
       char(128),  char(0),  char(2),  char(139),  char(11),  char(0),  char(2),  char(139),  char(11),  char(0),  &
       char(0),  char(0),  char(0),  char(34),  char(4),  char(0),  char(0),  char(0),  char(0),  char(0),  char(5),  &
       char(2),  char(0),  char(11),  char(0),  char(0),  char(0),  char(1),  char(0),  char(0),  char(0),  char(0),  &
       char(1),  char(0),  char(0),  char(0),  char(0),  char(1),  char(255),  char(0),  char(0),  char(0),  char(0),  &
       char(0),  char(0),  char(0),  char(0),  char(23),  char(5),  char(0),  char(0),  char(33),  char(36),  char(0),  &
       char(40),  char(65),  char(136),  char(0),  char(0),  char(0),  char(0),  char(0),  char(2),  char(9),  char(0),  &
       char(0),  char(255),  char(0),  char(0),  char(17),  char(203),  char(6),  char(0) /)

  ! This will not work, because we try to read more bytes than the file holds.
  print *, 'Trying to read too many bytes...'
  lgrib = 1000
  call baopenr(lugi, "testdata_g2grids", iret)
  if (iret .ne. 0) stop 3
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 2) stop 4
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  deallocate(cbuf)

  ! This will not work, because it's not a GRIB2 file.
  ! print *, 'Trying to index a non-GRIB2 file...'
  ! lgrib = 96
  ! call baopenr(lugi, "testdata_g2grids", iret)
  ! if (iret .ne. 0) stop 10
  ! call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  ! if (iret .ne. 3) stop 11
  ! call baclose(lugi, iret)
  ! if (iret .ne. 0) stop 12
  ! deallocate(cbuf)

  ! Now open a real GRIB2 file.
!   print *, 'Indexing a real GRIB2 file...'
!   call baopenr(lugi, "WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
!   if (iret .ne. 0) stop 100

!   ! Skip the first 202 bytes of the test file.
!   lskip = 202
!   call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
!   if (iret .ne. 0) stop 101
!   if (numfld .ne. 1 .or. mlen .ne. 200) stop 102
!   do i = 1, mlen
! !     print *, 'char(', ichar(cbuf(i)), '), '
!      if (cbuf(i) .ne. expected_cbuf(i)) stop 103
!   end do
!   deallocate(cbuf)
  
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_ixgb2
