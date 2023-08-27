! This is a test program for NCEPLIBS-g2.
!
! This program tests reading a very large FV3 GRIB2 file, only
! downloaded if both the FTP_TEST_FILES and the FTP_LARGE_TEST_FILES
! are set to ON at build time.
!
! Ed Hartnett 7/17/23
program test_fv3
  use grib_mod
  implicit none
  
  ! THese are the test files we will use.
  character(*) :: TEST_FILE
  parameter (TEST_FILE = 'data/fv3lam.t00z.prslev.f000.grib2')
  integer :: NUM_MSG
  parameter (NUM_MSG = 555)
  integer :: lugb = 10
  integer :: jdisc = -1, jpdtn = -1, jgdtn = -1, jskp = 0
  integer, dimension(200) :: jids, jpdt, jgdt
  logical :: unpack = .true.
  type(gribfield) :: gfld
  integer :: i, start
  integer :: lskip, lgrib
  integer :: iret
  
  print *, 'Testing reading GRIB2 file ', TEST_FILE
  print *, 'trying getgb2()...'

  ! Open the file.
  call baopenr(lugb, TEST_FILE, iret)
  if (iret .ne. 0) stop 2

  ! Learn about the file.
  jids = -9999
  jpdt = -9999
  jgdt = -9999
  call getgb2(lugb, 0, jskp, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          unpack, jskp, gfld, iret)
  if (iret .ne. 0) stop 3

  ! Close the file.
  call baclose(lugb, iret)  
  if (iret .ne. 0) stop 200

  ! Free memory.
  call gf_free(gfld)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 5
  
  print *, 'OK!'
  print *, 'trying skgb()...'
  
  ! Open the file.
  call baopenr(lugb, TEST_FILE, iret)
  if (iret .ne. 0) stop 2

  ! Loop through the file, checking location of each message.
  start = 0
  do i = 1, NUM_MSG
     call skgb(lugb, start, 10000, lskip, lgrib)
     print *, i, lskip, lgrib
!     if (lskip .ne. expected_lskip(i) .or. lgrib .ne. expected_lgrib(i)) stop 101
     start = start + lgrib
  end do

  ! Close the file.
  call baclose(lugb, iret)  
  if (iret .ne. 0) stop 200

  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_fv3
