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
  integer (kind = 8) :: b2s_lus8, b2s_gds8, b2s_pds8
  integer :: total_bytes, grib_version, discipline, field_number, i, idxver
  integer (kind = 8) :: b2s_message8

  integer :: lu_gdas = 4, lu_gdas_index = 5, mypos
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)

  interface
     subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(out) :: nlen, nnum, iret
       character(len = 1), pointer, dimension(:) :: cindex
     end subroutine getidx
     subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(inout) :: idxver
       character(len = 1), pointer, dimension(:) :: cindex
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getidx2
  end interface

  print *, 'Testing the getidx() subroutine - expect and ignore error messages during test...'

  do i = 1, 2
     ! Open a real GRIB2 file.
     print *, 'Indexing a real GRIB2 file: ', TEST_FILE_WW3_WEST
     call baopenr(lugb, TEST_FILE_WW3_WEST, iret)
     if (iret .ne. 0) stop 10

     ! This will not work because the first argument must be between 0 and 9999.
     !  call getidx(0, lugi, cbuf, nlen, nnum, iret)
     !  if (iret .ne. 90) stop 11
     ! call getidx(10000, lugi, cbuf, nlen, nnum, iret)
     ! if (iret .ne. 90) stop 12

     ! Get the index info, telling getidx() to generate it directly from
     ! the GRIB2 file.
     lugi = 0
     idxver = i
     call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, iret)
     if (iret .ne. 0) stop 20
     print *, 'nlen, nnum: ', nlen, nnum
     if (nnum .ne. 688) stop 21
     if (i .eq. 1) then
        if (nlen .ne. 137600) stop 22
     else
        if (nlen .ne. 148608) then
           print *, nlen
           stop 23
        endif
     endif

     ! Break out the index record into component values and check them for correctness.
     mypos = 0
     call g2_gbytec(cbuf, index_rec_len, mypos, INT4_BITS)
     mypos = mypos + INT4_BITS
     if (i .eq. 1) then
        if (index_rec_len .ne. 200) stop 30
     else
        if (index_rec_len .ne. 216) then
           print *, index_rec_len
           stop 30
        endif
     endif
     if (i .eq. 1) then
        call g2_gbytec(cbuf, b2s_message, mypos, INT4_BITS) ! msg length
        if (b2s_message .ne. 202) stop 31
        mypos = mypos + INT4_BITS
        b2s_message8 = b2s_message
        call g2_gbytec(cbuf, b2s_lus, mypos, INT4_BITS) ! skip to local
        if (b2s_lus .ne. 0) stop 33
        mypos = mypos + INT4_BITS
        call g2_gbytec(cbuf, b2s_gds, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        call g2_gbytec(cbuf, b2s_pds, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
     else
        call g2_gbytec8(cbuf, b2s_message8, mypos, INT8_BITS) ! msg length
        if (b2s_message8 .ne. 202) stop 32
        mypos = mypos + INT8_BITS
        call g2_gbytec8(cbuf, b2s_lus8, mypos, INT8_BITS) ! skip to local
        if (b2s_lus8 .ne. 0) stop 33
        mypos = mypos + INT8_BITS
        b2s_lus = int(b2s_lus8, kind(4))
        call g2_gbytec8(cbuf, b2s_gds8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        b2s_gds = int(b2s_gds8, kind(4))
        call g2_gbytec8(cbuf, b2s_pds8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        b2s_pds = int(b2s_pds8, kind(4))
     endif
     if (b2s_gds .ne. 37) stop 34
     if (b2s_pds .ne. 109) stop 35
     call g2_gbytec(cbuf, b2s_drs, mypos, INT4_BITS)
     if (b2s_drs .ne. 143) stop 36
     mypos = mypos + INT4_BITS
     call g2_gbytec(cbuf, b2s_bms, mypos, INT4_BITS)
     if (b2s_bms .ne. 166) stop 37
     mypos = mypos + INT4_BITS
     call g2_gbytec(cbuf, b2s_data, mypos, INT4_BITS)
     if (b2s_data .ne. 4721) stop 38
     mypos = mypos + INT4_BITS
     print *, 'total_bytes mypos ', mypos
     call g2_gbytec(cbuf, total_bytes, mypos, INT8_BITS)
     if (total_bytes .ne. 11183) stop 39
     mypos = mypos + INT8_BITS
     print *, 'test_getidx expecting grib_version after byte, bit ', mypos, mypos/8
     print '(z2.2)', cbuf(mypos/8 + 0)
     print '(z2.2)', cbuf(mypos/8 + 1)
     print '(z2.2)', cbuf(mypos/8 + 2)
     print '(z2.2)', cbuf(mypos/8 + 3)
     call g2_gbytec(cbuf, grib_version, mypos, INT1_BITS)
     print *, grib_version, mypos
     if (grib_version .ne. 2) stop 40
     mypos = mypos + INT1_BITS
     call g2_gbytec(cbuf, discipline, mypos, INT1_BITS)
     if (discipline .ne. 10) stop 41
     mypos = mypos + INT1_BITS
     call g2_gbytec(cbuf, field_number, mypos, INT2_BITS)
     if (field_number .ne. 1) stop 42
     print *, 'index_rec_len = ', index_rec_len, ' b2s_message8 = ', b2s_message8
     print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
     print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

     ! Clean up. Call gf_finalize or else index will be found in
     !library buffer. Don't deallocate cbuf becuase gf_finalize() does
     !that.
     call gf_finalize(iret)
     if (iret .ne. 0) stop 50
     call baclose(lugb, iret)
     if (iret .ne. 0) stop 51
  end do
  
  ! Open a real GRIB2 file.
  print *, 'Open a real GRIB2 file: ', TEST_FILE_GDAS
  call baopenr(lu_gdas, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 60

  ! Open a real GRIB2 index file.
  print *, 'Open a real GRIB2 index file: ', TEST_FILE_GDAS_INDEX
  call baopenr(lu_gdas_index, TEST_FILE_GDAS_INDEX, iret)
  if (iret .ne. 0) stop 70

  ! Get the index info, telling getidx() to use the index file.
  call getidx(lu_gdas, -1 * lu_gdas_index, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 80
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 81
  
  ! Get the index info, telling getidx() to use the index file.
  call getidx(lu_gdas, -1 * lu_gdas_index, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 90
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 91
  
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
  if (iret .ne. 0) stop 300
  print *, 'nlen, nnum: ', nlen, nnum
  if (nlen .ne.  3800 .or. nnum .ne. 19) stop 301
  
  ! Break out the index record into component values and check them for correctness.
  call g2_gbytec(cbuf, index_rec_len, 0, 8*4)
  if (index_rec_len .ne. 200) stop 305
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 0) stop 306
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0) stop 307
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37) stop 308
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109) stop 309
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143) stop 310
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166) stop 311
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721) stop 312
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 15254) stop 313
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2) stop 315
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 0) stop 317
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1) stop 320
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
  if (index_rec_len .ne. 200)stop 405
  call g2_gbytec(cbuf, b2s_message, 8*4, 8*4)
  if (b2s_message .ne. 0)stop 406
  call g2_gbytec(cbuf, b2s_lus, 8*8, 8*4)
  if (b2s_lus .ne. 0)stop 407
  call g2_gbytec(cbuf, b2s_gds, 8*12, 8*4)
  if (b2s_gds .ne. 37)stop 408
  call g2_gbytec(cbuf, b2s_pds, 8*16, 8*4)
  if (b2s_pds .ne. 109)stop 409
  call g2_gbytec(cbuf, b2s_drs, 8*20, 8*4)
  if (b2s_drs .ne. 143)stop 410
  call g2_gbytec(cbuf, b2s_bms, 8*24, 8*4)
  if (b2s_bms .ne. 166)stop 411
  call g2_gbytec(cbuf, b2s_data, 8*28, 8*4)
  if (b2s_data .ne. 4721)stop 412
  call g2_gbytec(cbuf, total_bytes, 8*32, 8*8)
  if (total_bytes .ne. 15254)stop 413
  call g2_gbytec(cbuf, grib_version, 8*40, 8*1)
  if (grib_version .ne. 2)stop 415
  call g2_gbytec(cbuf, discipline, 8*41, 8*1)
  if (discipline .ne. 0)stop 417
  call g2_gbytec(cbuf, field_number, 8*42, 8*2)
  if (field_number .ne. 1)stop 420
  print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
  print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

  ! Clean up.
  deallocate(cbuf)
  call baclose(lu_gdas, iret)
  if (iret .ne. 0) stop 500
  call baclose(lu_gdas_index, iret)
  if (iret .ne. 0) stop 501

  print *, 'SUCCESS!...'

end program test_getidx
