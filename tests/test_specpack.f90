program test_specpack

    integer, parameter :: idrstmplen = 10
    integer, parameter :: ndpts = 6
    integer :: idrstmpl(idrstmplen)
    real(8) :: fld(ndpts), fld2(ndpts)
    integer :: JJ, KK, MM
    character(len = 1) :: cpack
    integer :: lcpack

    fld = (/ 1.0d0, 1.2d0, 1.4d0, 1.6d0, 1.8d0, 2.0d0 /)
    JJ = 1
    KK = 1
    MM = 1

    idrstmpl(1) = 1.0d0
    idrstmpl(2) = 1
    idrstmpl(3) = 1
    idrstmpl(4) = 8
    idrstmpl(5) = 10e6
    idrstmpl(6) = 2
    idrstmpl(7) = 2
    idrstmpl(8) = 2
    idrstmpl(9) = 6
    idrstmpl(10) = 1

    print *, 'fld going in: ', fld
    call specpack(fld, ndpts, JJ, KK, MM, idrstmpl, cpack, lcpack)

    print *, 'Length of cpack: ', lcpack

    call specunpack(cpack, lcpack, idrstmpl, ndpts, JJ, KK, MM, fld2)

    print *, 'fld2: ', fld2

end program test_specpack