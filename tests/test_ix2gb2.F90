! This is a test program for NCEPLIBS-g2.
!
! This program tests ix2gb2(). ix2gb2() creates the index record for
! one GRIB2 message, and returns it in parameter cbuf.
!
! Ed Hartnett 5/9/24
program test_ix2gb2
  use g2logging
  use bacio_module
  implicit none

  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: numfld, mlen, iret
  integer (kind = 8) :: lskip8, lgrib8
  integer :: idxver = 1

  integer :: index_rec_len
  integer (kind = 8) :: b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer (kind = 8) :: total_bytes
  integer :: grib_version, discipline, field_number
  integer :: SEC1_LEN
  parameter (SEC1_LEN = 21)
  integer :: GDS_LEN
  parameter (GDS_LEN = 72)
  integer :: PDS_LEN
  parameter (PDS_LEN = 34)
  integer :: DRS_LEN
  parameter (DRS_LEN = 23)
  integer :: BMS_LEN
  parameter (BMS_LEN = 6)
  integer :: lengds, lenpds, lendrs
  character :: sec1(SEC1_LEN), gds(GDS_LEN), pds(PDS_LEN), drs(DRS_LEN), bms(BMS_LEN)
  character :: expected_sec1(SEC1_LEN) = (/  char(0), char(0), char(0), char(21), char(1), char(0), &
       char(7), char(0), char(0), char(2), char(1), &
       char(1), char(7), char(229), char(11), char(30), char(0), char(0), char(0), char(0), char(1)/)
  character :: expected_gds(GDS_LEN) = (/ char(0), char(0), char(0), char(72), char(3), char(0), char(0), &
       char(0), char(142), char(39), char(0), char(0), char(0), char(0), char(6), char(0), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
       char(0), char(0), char(0), char(0), char(0), char(241), char(0), char(0), char(0), char(151), &
       char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(2), char(250), &
       char(240), char(128), char(12), char(132), char(88), char(128), char(48), char(1), char(125), &
       char(120), char(64), char(14), char(230), char(178), char(128), char(0), char(2), char(139), &
       char(11), char(0), char(2), char(139), char(11), char(0) /)
  character :: expected_pds(PDS_LEN) = (/ char(0), char(0), char(0), char(34), char(4), char(0), char(0), &
       char(0), char(0), char(2), char(1), char(2), char(0), char(11), char(0), char(0), char(0), char(1), &
       char(0), char(0), char(0), char(0), char(1), char(0), char(0), char(0), char(0), char(1), char(255), &
       char(0), char(0), char(0), char(0), char(0) /)
  character :: expected_drs(DRS_LEN) = (/ char(0), char(0), char(0), char(23), char(5), char(0), char(0), &
       char(43), char(33), char(0), char(40), char(65), char(32), char(0), char(0), char(0), char(0), char(0), &
       char(2), char(11), char(0), char(0), char(255) /)
  character :: expected_bms(BMS_LEN) = (/ char(0), char(0), char(17), char(203), char(6), char(0) /)
  integer :: i

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
          b2s_bms, b2s_data, total_bytes, grib_version, discipline, field_number, sec1, lengds, gds, lenpds, pds, &
          lendrs, drs, bms, iret)
       character(len=1), pointer, dimension(:), intent(in) :: cbuf(:)
       integer, intent(in) :: idxver
       integer, intent(out) :: index_rec_len
       integer (kind = 8), intent(out) :: b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
       integer (kind = 8), intent(out) :: total_bytes
       integer, intent(out) :: grib_version, discipline, field_number
       character, intent(out) :: sec1(21)
       integer, intent(inout) :: lengds
       character, intent(out) :: gds(:)
       integer, intent(inout) :: lenpds
       character, intent(out) :: pds(:)
       integer, intent(inout) :: lendrs
       character, intent(out) :: drs(:)
       character, intent(out) :: bms(:)
       integer, intent(out) :: iret
     end subroutine read_index
  end interface

  print *, 'Testing ix2gb2().'
  do idxver = 1, 2
     print *, '  testing with idxver', idxver
     call baopenr(lugi, TEST_FILE_GDAS, iret)
     if (iret .ne. 0) then
        print *, 'baopenr failed with iret value: ', iret
        stop 3
     end if

     ! Create an index record for the first message in the gdas test
     ! file.
     g2_log_level = 5
     lskip8 = 0
     lgrib8 = 5000
     call ix2gb2(lugi, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
     if (numfld .ne. 1 .or. iret .ne. 0) stop 10
     if (idxver .eq. 1) then
        if (mlen .ne. 200) stop 11
     else
        if (mlen .ne. 224) then
           print *, mlen
           stop 12
        endif
     endif

     ! Break out the index record into component values.
     g2_log_level = 3
     call read_index(cbuf, idxver, index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, &
          b2s_bms, b2s_data, total_bytes, grib_version, discipline, field_number, sec1, lengds, gds, &
          lenpds, pds, lendrs, drs, bms, iret)
     if (iret .ne. 0) stop 21

     print *, '    index_rec_len = ', index_rec_len, ' b2s_message = ', b2s_message
     print *, '    b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
     print *, '    total_bytes, grib_version, discipline, field_number: ', total_bytes, grib_version, discipline, field_number
     print *, '    lengds, lenpds, lendrs', lengds, lenpds, lendrs

     if (idxver .eq. 1) then
        if (index_rec_len .ne. 200) stop 104
     else
        if (index_rec_len .ne. 224) then
           print *, index_rec_len
           stop 105
        endif
     endif
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
     do i = 1, SEC1_LEN
        !print *, i, ichar(sec1(i))
        if (sec1(i) .ne. expected_sec1(i)) stop 200
     enddo
     if (lengds .ne. GDS_LEN) stop 201
     do i = 1, GDS_LEN
        !print *, i, ichar(gds(i))
        if (gds(i) .ne. expected_gds(i)) stop 201
     enddo
     if (lenpds .ne. PDS_LEN) stop 201
     do i = 1, PDS_LEN
        !print *, i, ichar(pds(i))
        if (pds(i) .ne. expected_pds(i)) stop 210
     enddo
     if (lendrs .ne. DRS_LEN) stop 201
     do i = 1, DRS_LEN
        !print *, i, ichar(drs(i))
        if (drs(i) .ne. expected_drs(i)) stop 210
     enddo
     do i = 1, BMS_LEN
        !print *, i, ichar(bms(i))
        if (bms(i) .ne. expected_bms(i)) stop 210
     enddo
     
     ! Free allocated memory
     deallocate(cbuf)

     call baclose(lugi, iret)
     if (iret .ne. 0) then
        print *, 'baclose failed with iret value: ', iret
        stop 5
     end if
     print *, '  ok!'
  end do ! next idxver
  
  print *, 'Success!...'

end program test_ix2gb2

