! This is a test program for NCEPLIBS-g2.
!
! This program tests skgb().
!
! Ed Hartnett 5/16/23
program test_skgb
  use bacio_module
  implicit none

  ! THese are the test files we will use.
  character(*) :: TEST_FILE_WW3_WEST
  parameter (TEST_FILE_WW3_WEST = 'data/WW3_Regional_US_West_Coast_20220718_0000.grib2')
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'data/gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'data/ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')

  integer :: lugb = 5
  integer :: lskip, lgrib
  integer*8 :: lskip8, lgrib8
  integer :: NUM_MSG
  parameter (NUM_MSG = 26)
  integer :: expected_lskip(NUM_MSG) = (/ &
       202, 11591, 30714, 43564, 58836, 74084, 84652, 96184, 117557, 132886, 152913, 175331, 191386, &
       204222, 226323, 239313, 254572, 266120, 279283, 299055, 314265, 329433, 342106, 361989, 372633, 394921 /)
  integer :: expected_lgrib(NUM_MSG) = (/ &
       11183, 18917, 12644, 15067, 15042, 10362, 11326, 21168, 15123, 19821, 22210, 15849, 12631, 21895, &
       12785, 15053, 11342, 12957, 19567, 15006, 14962, 12467, 19677, 10438, 22082, 12497 /)
  integer :: i, start
  integer*8 :: start8
  integer :: iret
  
  print *, 'Testing skgb()...'

  ! Open a real GRIB2 file.
  call baopenr(lugb, TEST_FILE_WW3_WEST, iret)
  if (iret .ne. 0) stop 1

  ! Loop through the file, checking location of each message.
  start = 0
  do i = 1, NUM_MSG
     call skgb(lugb, start, 10000, lskip, lgrib)
     print *, 'i', i, ' lskip ', lskip, ' lgrib ',lgrib
     if (lskip .ne. expected_lskip(i) .or. lgrib .ne. expected_lgrib(i)) stop 10
     start = start + lgrib
  end do

  ! Try and read past end of file. This should work but doesn't see:
  ! https://github.com/NOAA-EMC/NCEPLIBS-g2/issues/395
  !  call skgb(lugb, start, 10000, lskip, lgrib)
  !  print *, i, lskip, lgrib

  ! Close the file.
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 20

  print *, 'OK!'
  print *, 'Testing skgb8()...'
  ! Open a real GRIB2 file.
  call baopenr(lugb, TEST_FILE_WW3_WEST, iret)
  if (iret .ne. 0) stop 100

  ! Loop through the file, checking location of each message.
  start8 = 0
  do i = 1, NUM_MSG
     call skgb8(lugb, start8, 10000_8, lskip8, lgrib8)
     print *, 'i', i, ' lskip8 ', lskip8, ' lgrib8 ',lgrib8
     if (lskip8 .ne. expected_lskip(i) .or. lgrib8 .ne. expected_lgrib(i)) stop 110
     start8 = start8 + lgrib8
  end do

  ! Close the file.
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 120
  print *, 'SUCCESS!...'

end program test_skgb
