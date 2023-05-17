! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir() with the test GDAS file.
!
! Ed Hartnett 5/17/23
program test_getg2ir_gdas
  use bacio_module
  implicit none

  integer :: lugb = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nlen, nnum, nmess, iret

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number

  ! These are the test files we will use.
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')

  interface
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb, msk1, msk2, mnum
       character(len = 1),pointer,dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
  end interface

  print *, 'Testing getg2ir() with ', TEST_FILE_GDAS, &
       ' - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  call baopenr(lugb, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 100

  msk1 = 1000
  msk2 = 1000
  mnum = 0
  call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  if (iret .ne. 0) stop 101
  if (nlen .ne. 3800 .or. nnum .ne. 19 .or. nmess .ne. 19) stop 102
  !  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
  
  ! Break out the index record into component values.
  call g2_gbytec(cbuf, index_rec_len, 0, 8 * 4)
  if (index_rec_len .ne. 200) stop 105
  call g2_gbytec(cbuf, b2s_message, 8 * 4, 8 * 4)
  if (b2s_message .ne. 0) stop 106
  call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 4)
  if (b2s_lus .ne. 0) stop 107
  call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 4)
  if (b2s_gds .ne. 37) stop 108
  call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 4)
  if (b2s_pds .ne. 109) stop 109
  call g2_gbytec(cbuf, b2s_drs, 8 * 20, 8 * 4)
  if (b2s_drs .ne. 143) stop 110
  call g2_gbytec(cbuf, b2s_bms, 8 * 24, 8 * 4)
  if (b2s_bms .ne. 166) stop 111
  call g2_gbytec(cbuf, b2s_data, 8 * 28, 8 * 4)
  if (b2s_data .ne. 4721) stop 112
  call g2_gbytec(cbuf, total_bytes, 8 * 32, 8 * 8)
  if (total_bytes .ne. 15254) stop 113
  call g2_gbytec(cbuf, grib_version, 8 * 40, 8 * 1)
  if (grib_version .ne. 2) stop 113
  call g2_gbytec(cbuf, discipline, 8 * 41, 8 * 1)
  if (discipline .ne. 0) stop 113
  call g2_gbytec(cbuf, field_number, 8 * 42, 8 * 2)
  if (field_number .ne. 1) stop 113
  ! print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  ! print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  ! print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  deallocate(cbuf)
  
  ! Get a index for a different message.
  mnum = 2
  call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
  if (iret .ne. 0) stop 101
  if (nlen .ne. 3400 .or. nnum .ne. 17 .or. nmess .ne. 19) stop 102
  
  ! Break out the index record into component values.
  call g2_gbytec(cbuf, index_rec_len, 0, 8 * 4)
  call g2_gbytec(cbuf, b2s_message, 8 * 4, 8 * 4)
  call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 4)
  call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 4)
  call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 4)
  call g2_gbytec(cbuf, b2s_drs, 8 * 20, 8 * 4)
  call g2_gbytec(cbuf, b2s_bms, 8 * 24, 8 * 4)
  call g2_gbytec(cbuf, b2s_data, 8 * 28, 8 * 4)
  call g2_gbytec(cbuf, total_bytes, 8 * 32, 8 * 8)
  call g2_gbytec(cbuf, grib_version, 8 * 40, 8 * 1)
  call g2_gbytec(cbuf, discipline, 8 * 41, 8 * 1)
  call g2_gbytec(cbuf, field_number, 8 * 42, 8 * 2)
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number
  if (index_rec_len .ne. 200) stop 105
  if (b2s_message .ne. 37897) stop 106
  if (b2s_lus .ne. 0) stop 107
  if (b2s_gds .ne. 37) stop 108
  if (b2s_pds .ne. 109) stop 109
  if (b2s_drs .ne. 143) stop 110
  if (b2s_bms .ne. 166) stop 111
  if (b2s_data .ne. 4721) stop 112
  if (total_bytes .ne. 15897) stop 113
  if (grib_version .ne. 2) stop 113
  if (discipline .ne. 0) stop 113
  if (field_number .ne. 1) stop 113
  
  deallocate(cbuf)
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getg2ir_gdas
