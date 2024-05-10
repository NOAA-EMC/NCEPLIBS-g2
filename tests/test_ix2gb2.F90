! This is a test program for NCEPLIBS-g2.
!
! This program tests ix2gb2(). ix2gb2() creates the index record for
! one GRIB2 message, and returns it in parameter cbuf.
!
! Ed Hartnett 5/9/24
program test_ix2gb2
  use bacio_module
  implicit none

  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: numfld, mlen, iret
  integer (kind = 8) :: lskip8, lgrib8
  integer :: idxver = 1

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number

  interface
     subroutine ix2gb2(lugb, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
       integer :: lugb
       integer (kind = 8) :: lskip8
       integer :: idxver
       integer (kind = 8) :: lgrib8
       character(len = 1), pointer, dimension(:) :: cbuf
       integer :: numfld, mlen, iret
     end subroutine ix2gb2
     subroutine read_index(cbuf, idxver, index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, &
     b2s_bms, b2s_data, total_bytes, grib_version, discipline, field_number, iret)
       character(len=1), pointer, dimension(:), intent(in) :: cbuf(:)
       integer, intent(in) :: idxver
       integer, intent(out) :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
       integer, intent(out) :: total_bytes, grib_version, discipline, field_number, iret
     end subroutine read_index
  end interface

  call baopenr(lugi, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) then
     print *, 'baopenr failed with iret value: ', iret
     stop 3
  end if

  ! Create an index record for the first message in the gdas test
  ! file.
  lskip8 = 0
  lgrib8 = 5000
  call ix2gb2(lugi, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
  if (numfld .ne. 1 .or. iret .ne. 0) stop 20
  if (mlen .ne. 200) stop 20
  
  ! Break out the index record into component values.
  call read_index(cbuf, idxver, index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, &
       b2s_bms, b2s_data, total_bytes, grib_version, discipline, field_number, iret)
  if (iret .ne. 0) stop 21
  
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  if (index_rec_len .ne. 200) stop 105
  if (b2s_message .ne. 0) stop 106
  if (b2s_lus .ne. 0) stop 107
  if (b2s_gds .ne. 37) stop 108
  if (b2s_pds .ne. 109) stop 109
  if (b2s_drs .ne. 143) stop 110
  if (b2s_bms .ne. 166) stop 111
  if (b2s_data .ne. 4721) stop 112
  if (total_bytes .ne. 5000) stop 113
  if (grib_version .ne. 2) stop 114
  if (discipline .ne. 0) stop 115
  if (field_number .ne. 1) stop 116

  ! Free allocated memory
  deallocate(cbuf)

  call baclose(lugi, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 5
  end if
  print *, 'Success!...'

end program test_ix2gb2

subroutine read_index(cbuf, idxver, index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, &
     b2s_bms, b2s_data, total_bytes, grib_version, discipline, field_number, iret)
  implicit none

  character(len=1), pointer, dimension(:), intent(in) :: cbuf(:)
  integer, intent(in) :: idxver
  integer, intent(out) :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer, intent(out) :: total_bytes, grib_version, discipline, field_number, iret

  integer :: inc

  if (idxver .eq. 1) then
     inc = 0
     call g2_gbytec(cbuf, index_rec_len, 0, 8 * 4)
     print *, 'index_rec_len', index_rec_len
     call g2_gbytec(cbuf, b2s_message, 8 * 4, 8 * 4)
     call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 4)
     call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 4)
  else
     inc = 16
     call g2_gbytec(cbuf, index_rec_len, 0, 8 * 8)
     print *, 'index_rec_len', index_rec_len
     call g2_gbytec(cbuf, b2s_message, 8 * 8, 8 * 8)
     call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 8)
     call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 8)
     ! call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 8)
  endif
  call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 4)
  call g2_gbytec(cbuf, b2s_drs, inc + 8 * 20, 8 * 4)
  call g2_gbytec(cbuf, b2s_bms, inc + 8 * 24, 8 * 4)
  call g2_gbytec(cbuf, b2s_data, inc + 8 * 28, 8 * 4)
  call g2_gbytec(cbuf, total_bytes, inc + 8 * 32, 8 * 8)
  call g2_gbytec(cbuf, grib_version, inc + 8 * 40, 8 * 1)
  call g2_gbytec(cbuf, discipline, inc + 8 * 41, 8 * 1)
  call g2_gbytec(cbuf, field_number, inc + 8 * 42, 8 * 2)
  
  iret = 0
end subroutine read_index

