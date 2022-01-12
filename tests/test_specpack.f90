program test_specpack

    integer, parameter :: idrstmplen = 10
    integer, parameter :: ndpts = 6
    integer :: idrstmpl(idrstmplen)
    real :: fld(ndpts), fld2(ndpts)
    integer :: JJ, KK, MM
    character(len = 1) :: cpack
    integer :: lcpack

    fld = (/ 1.0d0, 1.2d0, 1.4d0, 2.0d0, 2.2d0, 2.4d0 /)
    JJ = 2
    KK = 2
    MM = 2

    idrstmpl(1) = 0
    idrstmpl(2) = 1
    idrstmpl(3) = 1
    idrstmpl(4) = 16
    idrstmpl(5) = 1
    idrstmpl(6) = 1
    idrstmpl(7) = 1
    idrstmpl(8) = 1
    idrstmpl(9) = 6
    idrstmpl(10) = 2

    call specpack(fld,ndpts,JJ,KK,MM,idrstmpl,cpack,lcpack)

    print *, 'Length of cpack: ', lcpack

    call specunpack(cpack,lcpack,idrstmpl,ndpts,JJ,KK,MM,fld2)

    print *, 'fld2: ', fld2

end program test_specpack