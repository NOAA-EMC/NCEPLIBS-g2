!Test for the getdim source file
!Brian Curtis 2021-12-08
program test_getdim
     implicit none

     integer, parameter :: lcsec3 = 72
     character(len=1) :: csec3(lcsec3)
     integer :: width, height, iscan

     csec3 = (/ achar(0), achar(0), achar(0), achar(72), &
     achar(3), achar(0), achar(0), achar(0), achar(0), achar(4), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(1), achar(0), achar(0), achar(0), achar(2), achar(3), achar(0), achar(0), achar(0), achar(4), achar(5), &
     achar(0), achar(0), achar(0), achar(6), achar(0), achar(0), achar(0), achar(7), achar(0), achar(0), achar(0), &
     achar(8), achar(0), achar(0), achar(0), achar(9), achar(0), achar(0), achar(0), achar(10), achar(0), achar(0), &
     achar(0), achar(11), achar(0), achar(0), achar(0), achar(12), achar(13), achar(0), achar(0), achar(0), achar(14), &
     achar(0), achar(0), achar(0), achar(15), achar(0), achar(0), achar(0), achar(16), achar(0), achar(0), achar(0), &
     achar(17), achar(18) /)

     call getdim(csec3, lcsec3, width, height, iscan)

     if (width .ne. 7) stop 10
     if (height .ne. 8) stop 20
     if (iscan .ne. 18) stop 30

end program test_getdim
