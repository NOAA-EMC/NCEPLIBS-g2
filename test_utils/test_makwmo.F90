! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the makwmo subroutine.
program test_makwmo
    character(len = 6) :: BULHED = "YAAK37"
    character(len = 4) :: KWBX = "KWBB"
    integer :: IDAY = 28, IHOUR = 0, IMIN = 0
    character(len = 21) :: HEADER

    call makwmo(BULHED, IDAY, IHOUR, IMIN, KWBX, HEADER)

    if (HEADER(1:6) .ne. BULHED) stop 10
    if (HEADER(8:11) .ne. KWBX) stop 11
    if (HEADER(13:18) .ne. "280000") stop 12

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_makwmo