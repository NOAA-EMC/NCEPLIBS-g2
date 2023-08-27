! This is a test program for NCEPLIBS-g2.
!
! This program tests getidx().
!
! Ed Hartnett 7/26/22
program test_getidx
  use bacio_module
  implicit none

  ! THese are the test files we will use.
  character(*) :: TEST_FILE_WW3_WEST
  parameter (TEST_FILE_WW3_WEST = 'data/WW3_Regional_US_West_Coast_20220718_0000.grib2')
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')

  ! These are for the fist test file, WW3_WEST.
  integer :: lugi
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lugb = 3
  integer :: nlen, nnum, iret
  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number

  integer :: lu_gdas = 4, lu_gdas_index = 5

  interface
     subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(out) :: nlen, nnum, iret
       character(len = 1), pointer, dimension(:) :: cindex
     end subroutine getidx
  end interface

  print *, 'Testing the getidx() subroutine - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file: ', TEST_FILE_WW3_WEST
  call baopenr(lugb, TEST_FILE_WW3_WEST, iret)
  if (iret .ne. 0) stop 100

  ! This will not work because the first argument must be between 0 and 9999.
!  call getidx(0, lugi, cbuf, nlen, nnum, iret)
!  if (iret .ne. 90) stop 99
  call getidx(10000, lugi, cbuf, nlen, nnum, iret)
  if (iret .ne. 90) stop 99

  ! Get the index info, telling getidx() to generate it directly from
  ! the GRIB2 file.
  lugi = 0
  call getidx(lugb, lugi, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  if (nlen .ne. 137600 .or. nnum .ne. 688) stop 102
  print *, 'nlen, nnum: ', nlen, nnum
  
  ! Break out the index record into component values and check them for correctness.
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
  if (total_bytes .ne. 11183) stop 113
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 113
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 10) stop 113
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 113
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  ! Clean up.
  deallocate(cbuf)
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199
  
  ! Open a real GRIB2 file.
  print *, 'Open a real GRIB2 file: ', TEST_FILE_GDAS
  call baopenr(lu_gdas, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 100

  ! Open a real GRIB2 index file.
  print *, 'Open a real GRIB2 index file: ', TEST_FILE_GDAS_INDEX
  call baopenr(lu_gdas_index, TEST_FILE_GDAS_INDEX, iret)
  if (iret .ne. 0) stop 100

  ! Get the index info, telling getidx() to use the index file.
  call getidx(lu_gdas, -1 * lu_gdas_index, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 102
  
  ! Get the index info, telling getidx() to use the index file.
  call getidx(lu_gdas, -1 * lu_gdas_index, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 102
  
  ! Break out the index record into component values and check them for correctness.
  call g2_gbytec(cbuf, index_rec_len, 0, 8*4)
  if (index_rec_len .ne. 200) stop 205
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 0) stop 206
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0) stop 207
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37) stop 208
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109) stop 209
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143) stop 210
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166) stop 211
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721) stop 212
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 15254) stop 213
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 215
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 0) stop 217
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 220
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  ! Do the same read again.
  call getidx(lu_gdas, lu_gdas_index, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 102
  
  ! Break out the index record into component values and check them for correctness.
  call g2_gbytec(cbuf, index_rec_len, 0, 8*4)
  if (index_rec_len .ne. 200) stop 205
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 0) stop 206
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0) stop 207
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37) stop 208
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109) stop 209
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143) stop 210
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166) stop 211
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721) stop 212
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 15254) stop 213
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 215
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 0) stop 217
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 220
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  ! Get the index info, telling getidx() to use the index file.
  call getidx(lu_gdas, lu_gdas, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 102
  
  ! Break out the index record into component values and check them for correctness.
  call g2_gbytec(cbuf, index_rec_len, 0, 8*4)
  if (index_rec_len .ne. 200) stop 205
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 0) stop 206
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0) stop 207
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37) stop 208
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109) stop 209
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143) stop 210
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166) stop 211
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721) stop 212
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 15254) stop 213
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 215
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 0) stop 217
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 220
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  ! Clean up.
  deallocate(cbuf)
  call baclose(lu_gdas, iret)
  if (iret .ne. 0) stop 199
  call baclose(lu_gdas_index, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getidx
