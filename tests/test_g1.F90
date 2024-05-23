! This is a test program for NCEPLIBS-g2.
!
! This program tests getidx() with a file containing only one GRIB2
! message, the first message from file
! gdaswave.t00z.wcoast.0p16.f000.grib2.
!
! Ed Hartnett 2/12/24
program test_g1
  use bacio_module
  implicit none

  character(*) :: TEST_FILE_G1
  parameter (TEST_FILE_G1 = 'g1.grib2')
  integer :: lugi
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lugb = 3
  integer :: nlen, nnum, iret
  integer :: index_rec_len, b2s_message, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data, b2s_lus
  integer (kind = 8) :: b2s_lus8, b2s_gds8, b2s_pds8, b2s_drs8, b2s_bms8
  integer :: total_bytes, grib_version, discipline, field_number, i, idxver
  integer (kind = 8) :: b2s_message8

  integer :: mypos
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

  print *, 'Testing the getidx() subroutine with simple file...'

  do i = 1, 2
     ! Open a real GRIB2 file.
     print *, 'Indexing a real GRIB2 file: ', TEST_FILE_G1
     call baopenr(lugb, TEST_FILE_G1, iret)
     if (iret .ne. 0) stop 10

     ! Get the index info, telling getidx() to generate it directly from
     ! the GRIB2 file.
     lugi = 0
     idxver = i
     call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, iret)
     if (iret .ne. 0) stop 20
     print *, 'nlen, nnum: ', nlen, nnum
     if (nnum .ne. 1) stop 21
     if (i .eq. 1) then
        if (nlen .ne. 200) stop 22
     else
        print *, nlen
        if (nlen .ne. 224) stop 23
     endif
     ! do j = 1, nlen
     !    print '(i3, x, z2.2)', j, cbuf(j)
     ! end do

     ! Break out the index record into component values and check them for correctness.
     mypos = 0
     call g2_gbytec(cbuf, index_rec_len, mypos, INT4_BITS)
     mypos = mypos + INT4_BITS
     if (i .eq. 1) then
        if (index_rec_len .ne. 200) stop 29
     else
        if (index_rec_len .ne. 224) then
           print *, index_rec_len
           stop 30
        endif
     endif
     if (i .eq. 1) then
        call g2_gbytec(cbuf, b2s_message, mypos, INT4_BITS)
        if (b2s_message .ne. 0) stop 31
        mypos = mypos + INT4_BITS
        b2s_message8 = b2s_message
        call g2_gbytec(cbuf, b2s_lus, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        b2s_lus8 = b2s_lus
        call g2_gbytec(cbuf, b2s_gds, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        b2s_gds8 = b2s_gds
        call g2_gbytec(cbuf, b2s_pds, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        b2s_pds8 = b2s_pds
        call g2_gbytec(cbuf, b2s_drs, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        b2s_drs8 = b2s_drs
        call g2_gbytec(cbuf, b2s_bms, mypos, INT4_BITS)
        mypos = mypos + INT4_BITS
        b2s_bms8 = b2s_bms
     else
        call g2_gbytec8(cbuf, b2s_message8, mypos, INT8_BITS)
        if (b2s_message8 .ne. 0) stop 32
        mypos = mypos + INT8_BITS
        call g2_gbytec8(cbuf, b2s_lus8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        call g2_gbytec8(cbuf, b2s_gds8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        call g2_gbytec8(cbuf, b2s_pds8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        call g2_gbytec8(cbuf, b2s_drs8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
        call g2_gbytec81(cbuf, b2s_bms8, mypos, INT8_BITS)
        mypos = mypos + INT8_BITS
     endif
     if (b2s_lus8 .ne. 0) stop 33
     if (b2s_gds8 .ne. 37) stop 34
     if (b2s_pds8 .ne. 109) stop 35
     if (b2s_drs .ne. 143) stop 36
     if (b2s_bms .ne. 166) stop 37
     call g2_gbytec(cbuf, b2s_data, mypos, INT4_BITS)
     if (b2s_data .ne. 4721) stop 38
     mypos = mypos + INT4_BITS
     print *, 'total_bytes mypos ', mypos
     call g2_gbytec(cbuf, total_bytes, mypos, INT8_BITS)
     print *, 'total_bytes ', total_bytes
     
     if (total_bytes .ne. 15254) stop 39
     mypos = mypos + INT8_BITS
     call g2_gbytec(cbuf, grib_version, mypos, INT1_BITS)
     print *, grib_version, mypos
     if (grib_version .ne. 2) stop 40
     mypos = mypos + INT1_BITS
     call g2_gbytec(cbuf, discipline, mypos, INT1_BITS)
     if (discipline .ne. 0) stop 41
     mypos = mypos + INT1_BITS
     call g2_gbytec(cbuf, field_number, mypos, INT2_BITS)
     if (field_number .ne. 1) stop 42
     print *, 'index_rec_len = ', index_rec_len, ' b2s_message8 = ', b2s_message8
     print *, 'b2s_lus8, b2s_gds8, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus8, b2s_gds8, b2s_pds, b2s_drs, b2s_bms, b2s_data
     print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

     ! Clean up. Call gf_finalize or else index will be found in
     !library buffer. Don't deallocate cbuf becuase gf_finalize() does
     !that.
     call gf_finalize(iret)
     if (iret .ne. 0) stop 50
     call baclose(lugb, iret)
     if (iret .ne. 0) stop 51
  end do
  
  print *, 'SUCCESS!...'

end program test_g1
