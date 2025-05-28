! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the makwmo subroutine.
program test_makwmo
    character(len = 6) :: BULHED = "YAAK37"
    character(len = 4) :: KWBX = "KWBB"
    integer :: IDAY = 28, IHOUR = 0, IMIN = 0
    character(len = 21) :: HEADER, EXP_HEADER = "YAAK37 KWBB 280000\r\r\n"
    
    call makwmo(BULHED, IDAY, IHOUR, IMIN, KWBX, HEADER)

    if 
    if (HEADER .ne. EXP_HEADER) stop 10

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_makwmo