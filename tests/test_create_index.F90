! This is a test program for NCEPLIBS-g2.
!
! This program tests index file functionality with g2_create_index().
!
! Ed Hartnett 2/15/24
program test_create_index
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'gdaswave.t00z.wcoast.0p16.f000.grb2index')
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver = 1, nlen, nnum, lugi = 31, lugb = 11
  integer :: iret, ios

  interface
     subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
       integer, intent(in) :: lugi
       character(len=1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: idxver, nlen, nnum, iret
     end subroutine getg2i2
     subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
       integer, intent(in) :: lugb, lugi, idxver
       character*(*) :: filename
       integer, intent(out) :: iret
     end subroutine g2_create_index
  end interface

  print *, 'Testing g2_create_index on ', TEST_FILE_GDAS

  ! Open GRIB2 file for reading.
  call baopenr(lugb, TEST_FILE_GDAS, ios)
  if (ios .ne. 0) stop 2

  ! Open output file where index will be written.
  call baopen(lugi, TEST_FILE_GDAS_INDEX, ios)
  if (ios .ne. 0) stop 3

  call g2_create_index(lugb, lugi, idxver, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 10

  call baclose(lugb, ios)
  if (ios .ne. 0) stop 11
  call baclose(lugi, ios)
  if (ios .ne. 0) stop 12

  ! Open the index file.
  call baopen(lugi, TEST_FILE_GDAS_INDEX, iret)
  if (iret .ne. 0) stop 20

  ! Read the index file.
  call getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
  if (nlen .ne. 3800 .or. nnum .ne. 59 .or. iret .ne. 0) stop 80

  ! Close the index file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 100

  deallocate(cbuf)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 200
  
  print *, 'SUCCESS!...'
end program test_create_index
