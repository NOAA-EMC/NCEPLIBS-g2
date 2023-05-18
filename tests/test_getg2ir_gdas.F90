! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir() with the test GDAS file.
!
! Ed Hartnett 5/17/23
program test_getg2ir_gdas
  use bacio_module
  use index_rec
  implicit none

  integer :: lugb = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nlen, nnum, nmess, iret
  integer :: nlen_expected

  integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: total_bytes, grib_version, discipline, field_number
  type (index_rec_data) :: idx, expected_idx(3)

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
  call init_index(200, 0, 0, 37, 109, 143, 166, 4721, 15254, 2, 0, 1, expected_idx(1))
  call init_index(200, 15254, 0, 37, 109, 143, 166, 4721, 22643, 2, 0, 1, expected_idx(2))
  call init_index(200, 37897, 0, 37, 109, 143, 166, 4721, 15897, 2, 0, 1, expected_idx(3))

  ! Open a real GRIB2 file.
  call baopenr(lugb, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 100

  msk1 = 1000
  msk2 = 1000
  nlen_expected = 3800
  do mnum = 0, 2
     call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
     !  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
     if (iret .ne. 0) stop 101
     if (nlen .ne. nlen_expected .or. nnum .ne. 19 - mnum .or. nmess .ne. 19) stop 102
     nlen_expected = nlen_expected - 200

     ! Parse the index record into special type.
     call parse_cbuf(cbuf, idx)
     !call print_index(idx)

     ! Is this what we expected?
     if (cmp_idx(idx, expected_idx(mnum + 1)) .ne. 0) stop 300
     
     ! Free memory.
     deallocate(cbuf)
  end do
  
!   ! Get a index for a different message.
!   mnum = 1
!   call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
! !  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
!   if (iret .ne. 0) stop 101
!   if (nlen .ne. 3600 .or. nnum .ne. 18 .or. nmess .ne. 19) stop 102
  
!   call parse_cbuf(cbuf, idx)
!   !call print_index(idx)
!   if (cmp_idx(idx, expected_idx(2)) .ne. 0) stop 300

!   deallocate(cbuf)
  
!   ! Get a index for a different message.
!   mnum = 2
!   call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
! !  print *, 'iret, nlen, nnum, nmess: ', iret, nlen, nnum, nmess
!   if (iret .ne. 0) stop 101
!   if (nlen .ne. 3400 .or. nnum .ne. 17 .or. nmess .ne. 19) stop 102
  
!   call parse_cbuf(cbuf, idx)
!   !call print_index(idx)
!   if (cmp_idx(idx, expected_idx(3)) .ne. 0) stop 300

!   deallocate(cbuf)
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getg2ir_gdas


