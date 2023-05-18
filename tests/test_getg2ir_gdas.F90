! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir() with the test GDAS file.
!
! Ed Hartnett 5/17/23
program test_getg2ir_gdas
  use bacio_module
#if KIND == 4 
  use index_rec_4
#else
  use index_rec_d
#endif
  implicit none

  integer :: lugb = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nlen, nnum, nmess, iret

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number
  type (index_rec_data) :: idx, expected_idx1, expected_idx2

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

  print *, 'Testing getg2ir() with ', TEST_FILE_GDAS

  ! Initialize expected results.
  expected_idx1%index_rec_len = 200
  expected_idx1%b2s_message = 0
  expected_idx1%b2s_lus = 0
  expected_idx1%b2s_gds = 37
  expected_idx1%b2s_pds = 109
  expected_idx1%b2s_drs = 143
  expected_idx1%b2s_bms = 166
  expected_idx1%b2s_data = 4721
  expected_idx1%total_bytes = 15254
  expected_idx1%grib_version = 2
  expected_idx1%discipline = 0
  expected_idx1%field_number = 1

  expected_idx2%index_rec_len = 200
  expected_idx2%b2s_message = 37897
  expected_idx2%b2s_lus = 0
  expected_idx2%b2s_gds = 37
  expected_idx2%b2s_pds = 109
  expected_idx2%b2s_drs = 143
  expected_idx2%b2s_bms = 166
  expected_idx2%b2s_data = 4721
  expected_idx2%total_bytes = 15897
  expected_idx2%grib_version = 2
  expected_idx2%discipline = 0
  expected_idx2%field_number = 1

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

  call parse_cbuf(cbuf, idx)
  call print_index(idx)
  if (cmp_idx(idx, expected_idx1) .ne. 0) stop 300

  ! Free memory.
  deallocate(cbuf)
  
  ! Get a index for a different message.
  mnum = 2
  call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
!  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
  if (iret .ne. 0) stop 101
  if (nlen .ne. 3400 .or. nnum .ne. 17 .or. nmess .ne. 19) stop 102
  
  call parse_cbuf(cbuf, idx)
  call print_index(idx)
  if (cmp_idx(idx, expected_idx2) .ne. 0) stop 300

  deallocate(cbuf)
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getg2ir_gdas


