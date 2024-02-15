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
  integer :: idxver = 1, nlen, nnum, lugi = 31
  integer :: iret

  print *, 'Testing g2_create_index on ', TEST_FILE_GDAS

  call g2_create_index(TEST_FILE_GDAS, TEST_FILE_GDAS_INDEX, idxver, iret)
  if (iret .ne. 0) stop 10

  ! ! Open the index file.
  ! call baopen(lugi, TEST_FILE_GDAS_INDEX, iret)
  ! if (iret .ne. 0) stop 20

  ! ! Read the index file.
  ! call getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)

  ! ! Close the index file.
  ! call baclose(lugi, iret)
  ! if (iret .ne. 0) stop 100

  call gf_finalize(iret)
  if (iret .ne. 0) stop 200
  
  print *, 'SUCCESS!...'
end program test_create_index
