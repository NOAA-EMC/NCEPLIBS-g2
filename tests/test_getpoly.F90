! test getpoly.F90
! Author Brian Curtis 2021/12/21

program test_getpoly
    implicit none

    integer, parameter :: lcsec3 = 72
    character(len=1) :: csec3(lcsec3)
    integer :: jj, kk, mm

    csec3 = (/ achar(0), achar(0), achar(0), achar(72), &
    achar(3), achar(0), achar(0), achar(0), achar(0), achar(4), achar(0), achar(0), achar(0), achar(0), achar(0), &
    achar(1), achar(0), achar(0), achar(0), achar(2), achar(3), achar(0), achar(0), achar(0), achar(4), achar(5), &
    achar(0), achar(0), achar(0), achar(6), achar(0), achar(0), achar(0), achar(7), achar(0), achar(0), achar(0), &
    achar(8), achar(0), achar(0), achar(0), achar(9), achar(0), achar(0), achar(0), achar(10), achar(0), achar(0), &
    achar(0), achar(11), achar(0), achar(0), achar(0), achar(12), achar(13), achar(0), achar(0), achar(0), achar(14), &
    achar(0), achar(0), achar(0), achar(15), achar(0), achar(0), achar(0), achar(16), achar(0), achar(0), achar(0), &
    achar(17), achar(18) /)

    call getpoly(csec3, lcsec3, jj, kk, mm)

    ! Grid template number not 50:53, values should all be 0
    if (jj .ne. 0) stop 10
    if (kk .ne. 0) stop 20
    if (mm .ne. 0) stop 30

    ! Lets get into the igds(5) 50:53 section of code
    csec3(13) = achar(0)
    csec3(14) = achar(51)

    call getpoly(csec3, lcsec3, jj, kk, mm)
    if (jj .ne. 65536) stop 40
    if (kk .ne. 131840) stop 50
    if (mm .ne. 1029) stop 60

end program test_getpoly

