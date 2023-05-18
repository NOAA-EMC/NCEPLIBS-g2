! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir() with the test GDAS file.
!
! Ed Hartnett 5/17/23
program test_getg2ir_gdas
  use bacio_module
  use index_rec
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')

  integer :: lugb = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nlen, nnum, nmess, iret
  integer :: nlen_expected
  type (index_rec_data) :: idx, expected_idx(10)

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
  call init_index(200, 53794, 0, 37, 109, 143, 166, 4721, 15270, 2, 0, 1, expected_idx(4))
  call init_index(200, 69064, 0, 37, 109, 143, 166, 4721, 10418, 2, 0, 1, expected_idx(5))
  call init_index(200, 79482, 0, 37, 109, 143, 166, 4721, 11826, 2, 0, 1, expected_idx(6))
  call init_index(200, 91308, 0, 37, 109, 143, 166, 4721, 17233, 2, 0, 1, expected_idx(7))
  call init_index(200, 108541, 0, 37, 109, 143, 166, 4721, 8175, 2, 0, 1, expected_idx(8))
  call init_index(200, 116716, 0, 37, 109, 143, 166, 4721, 12116, 2, 0, 1, expected_idx(9))
  call init_index(200, 128832, 0, 37, 109, 143, 166, 4721, 12016, 2, 0, 1, expected_idx(10))

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
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getg2ir_gdas


