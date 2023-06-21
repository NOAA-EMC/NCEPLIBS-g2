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
       char(0),  char(0),  char(95),  char(2),  char(10),  char(0),  char(1),  char(0),  char(0),  char(0),  &
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

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number

  ! This will not work, because we try to read more bytes than the file holds.
  print *, 'Trying to read too many bytes...'
  lgrib = 1000
  lskip = 0
  call baopenr(lugi, "testdata_g2grids", iret)
  if (iret .ne. 0) stop 3
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 2) stop 4
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  deallocate(cbuf)

  ! This will not work, because it's not a GRIB2 file.
  print *, 'Trying to index a non-GRIB2 file...'
  lgrib = 95
  lskip = 0
  call baopenr(lugi, "testdata_g2grids", iret)
  if (iret .ne. 0) stop 10
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  print *, 'iret = ', iret
  if (iret .ne. 3) stop 11
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 12
  deallocate(cbuf)

  ! Now open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file...'
  call baopenr(lugi, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  ! Skip the first 202 bytes of the test file.
  lskip = 202
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)
  if (iret .ne. 0) stop 101
  if (numfld .ne. 1 .or. mlen .ne. 200) stop 102

  ! Check every value.
  do i = 1, mlen
     !print *, 'char(', ichar(cbuf(i)), '), '
     if (cbuf(i) .ne. expected_cbuf(i)) stop 103
  end do

  ! Break out the index record into component values.
  call g2_gbytec(cbuf, index_rec_len, 0, 8*4)
  if (index_rec_len .ne. 200) stop 105
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 202) stop 106
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0) stop 107
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37) stop 108
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109) stop 109
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143) stop 110
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166) stop 111
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721) stop 112
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 95) stop 113
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 113
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 10) stop 113
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 113
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number
  
  deallocate(cbuf)
  
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_ixgb2
