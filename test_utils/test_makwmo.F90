! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the makwmo subroutine.
program test_makwmo
    character(len = 6) :: BULHED = "YAAK37"
    character(len = 4) :: KWBX = "KWBB"
    integer :: IDAY = 28, IHOUR = 0, IMIN = 0
    character(21) :: HEADER
    
    call makwmo(BULHED, IDAY, IHOUR, IMIN, KWBX, HEADER)

    if (trim(HEADER) .ne. "YAAK37 KWBB 280000") stop 10

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_makwmo