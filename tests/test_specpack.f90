program test_specpack
    implicit none

    integer, parameter :: idrstmplen = 10
    integer, parameter :: ndpts = 6
    integer(4) :: idrstmpl(idrstmplen)
    real(8) :: fld(ndpts)
    ! real(4) :: fld_in(ndpts)
    real(8) :: fld2(ndpts)
    integer :: JJ, KK, MM
    character(len=1) :: cpack(24)
    integer :: lcpack
    integer :: i

    ! fld_in = (/ cmplx(1.0000, 2), cmplx(1.2000, 1), cmplx(1.4000, 3), cmplx(1.6000, 4), cmplx(1.8000, 2), cmplx(2.0000, 1) /)
    fld = (/ 1.0000d0, 1.2000d0, 1.4000d0, 1.6000d0, 1.8000d0, 2.0000d0/)
    JJ = 1
    KK = 1
    MM = 1

    idrstmpl(1) = 1
    idrstmpl(2) = 0
    idrstmpl(3) = 0
    idrstmpl(4) = 64
    idrstmpl(5) = 1
    idrstmpl(6) = 1
    idrstmpl(7) = 1
    idrstmpl(8) = 1
    idrstmpl(9) = 6
    idrstmpl(10) = 2

    ! fld = fld_in
    print *, 'fld: ', fld

    call specpack(fld, ndpts, JJ, KK, MM, idrstmpl, cpack, lcpack)

    ! print *, 'lcpack: ', lcpack
    if (lcpack .ne. 24) stop 3

    call specunpack(cpack, lcpack, idrstmpl, ndpts, JJ, KK, MM, fld2)
    print *, 'fld2: ', fld2
    print *, 'ndpts: ', ndpts
    do i = 1, ndpts
        print *, 'fld, fld2, fld - fld2:', fld(i), fld2(i), fld(i)-fld2(i)
        if (abs(fld(i) - fld2(i)) .gt. 0.000000001) stop 33
    end do

end program test_specpack