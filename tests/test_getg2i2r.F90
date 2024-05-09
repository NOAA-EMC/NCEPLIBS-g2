! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir2.F90
!
! Ed Hartnett 5/9/24
program test_getg2ir2
  use bacio_module
  implicit none

  integer :: lugb = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer (kind = 8) :: msk1, msk2
  integer :: mnum
  integer :: nlen, nnum, nmess, iret
  integer :: idxver

  interface
     subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: msk1, msk2
       integer, intent(in) :: mnum, idxver
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2i2r
  end interface

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number, inc

  print *, 'Testing the getg2ir2() subroutine - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file...'
  call baopenr(lugb, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  do idxver = 1, 1
     print *, '   testing with idxver', idxver
     msk1 = 1000
     msk2 = 1000
     mnum = 0
     call getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, nlen, nnum, nmess, iret)
     if (iret .ne. 0) stop 101
     print *, 'nlen, nnum, nmess: ', nlen, nnum, nmess
     if (nlen .ne. 137600 .or. nnum .ne. 688 .or. nmess .ne. 688) stop 102

     ! Break out the index record into component values.
     if (idxver .eq. 1) then
        inc = 0
        call g2_gbytec(cbuf, index_rec_len, 0, 8 * 4)
        if (index_rec_len .ne. 200) stop 105
        print *, 'index_rec_len', index_rec_len
        call g2_gbytec(cbuf, b2s_message, 8 * 4, 8 * 4)
        if (b2s_message .ne. 202) stop 106
        call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 4)
        if (b2s_lus .ne. 0) stop 107
        call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 4)
        if (b2s_gds .ne. 37) stop 108
     else
        inc = 16
        call g2_gbytec(cbuf, index_rec_len, 0, 8 * 8)
        if (index_rec_len .ne. 200) stop 105
        print *, 'index_rec_len', index_rec_len
        call g2_gbytec(cbuf, b2s_message, 8 * 8, 8 * 8)
        if (b2s_message .ne. 202) stop 106
        call g2_gbytec(cbuf, b2s_lus, 8 * 8, 8 * 8)
        if (b2s_lus .ne. 0) stop 107
        call g2_gbytec(cbuf, b2s_gds, 8 * 12, 8 * 8)
        if (b2s_gds .ne. 37) stop 108
        ! call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 8)
        ! if (b2s_pds .ne. 109) stop 109
     endif
     call g2_gbytec(cbuf, b2s_pds, 8 * 16, 8 * 4)
     if (b2s_pds .ne. 109) stop 109
     call g2_gbytec(cbuf, b2s_drs, inc + 8 * 20, 8 * 4)
     if (b2s_drs .ne. 143) stop 110
     call g2_gbytec(cbuf, b2s_bms, inc + 8 * 24, 8 * 4)
     if (b2s_bms .ne. 166) stop 111
     call g2_gbytec(cbuf, b2s_data, inc + 8 * 28, 8 * 4)
     if (b2s_data .ne. 4721) stop 112
     call g2_gbytec(cbuf, total_bytes, inc + 8 * 32, 8 * 8)
     if (total_bytes .ne. 11183) stop 113
     call g2_gbytec(cbuf, grib_version, inc + 8 * 40, 8 * 1)
     if (grib_version .ne. 2) stop 113
     call g2_gbytec(cbuf, discipline, inc + 8 * 41, 8 * 1)
     if (discipline .ne. 10) stop 113
     call g2_gbytec(cbuf, field_number, inc + 8 * 42, 8 * 2)
     if (field_number .ne. 1) stop 113
     print *, 'index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
     print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
     print *, 'total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number

     deallocate(cbuf)

     call baclose(lugb, iret)
     if (iret .ne. 0) stop 199
     print *, '   OK!'
  end do
  print *, 'SUCCESS!...'

end program test_getg2ir2
