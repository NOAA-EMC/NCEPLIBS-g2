! This program tests the jpcpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-18
! Ed Hartnett, 7/31/23
program test_jpcpack
    implicit none

    integer, parameter :: width = 2, height = 2, ndpts = 4
    real, parameter :: delta = 0.2
    real :: fld(ndpts)
    real :: fld2(ndpts)
    integer :: idrstmpl(7)
    character*1 :: cpack(200)
    integer :: lcpack = 200
    integer :: ii

    ! Create the fld variable with data to pack.
    fld = (/1.1, 2.2, 3.3, 4.4/)
    fld2 = (/0, 0, 0, 0/)

    idrstmpl(1) = 0
    idrstmpl(2) = 1
    idrstmpl(3) = 1
    idrstmpl(4) = 32
    idrstmpl(5) = 0
    idrstmpl(6) = 0
    idrstmpl(7) = 1

    print *, 'Testing jpcpack/jpcunpack with no data...'

    ! Pack the data.
    call jpcpack(fld, 0, height, idrstmpl, cpack, lcpack)
    print *, 'lcpack: ', lcpack
    if (lcpack .ne. 0) stop 1
    
    ! Unpack the data.
    call jpcunpack(cpack, lcpack, idrstmpl, 0, fld2)

    ! Compare each value to see match, remember, comparing reals.
    print *, 'ok!'
    print *, 'Testing jpcpack/jpcunpack with data...'

    ! Pack the data.
    lcpack = 200
    call jpcpack(fld, width, height, idrstmpl, cpack, lcpack)
    ! The size of lcpack will depend on the version of jasper used to compress the data.
    !print *,lcpack
    if (lcpack .le. 0) stop 2
    
    ! Unpack the data.
    call jpcunpack(cpack, lcpack, idrstmpl, ndpts, fld2)

    ! Compare each value to see match, remember, comparing reals.
    !print *, fld
    !print *, fld2
    do ii = 1, ndpts
        if (abs(fld(ii) - fld2(ii)) .gt. delta) then
            print *, fld(ii), fld2(ii), 'do not match'
            stop 4
        end if
    end do

    print *, 'ok!'
    print *, 'Testing jpcpack/jpcunpack with data and idrstmpl(2) = 0...'

    ! Pack the data.
    idrstmpl(2) = 0
    lcpack = 200
    call jpcpack(fld, width, height, idrstmpl, cpack, lcpack)
    ! The size of lcpack will depend on the version of jasper used to compress the data.
    !print *,lcpack
    if (lcpack .le. 0) stop 2
    
    ! Unpack the data.
    call jpcunpack(cpack, lcpack, idrstmpl, ndpts, fld2)

    ! Compare each value to see match, remember, comparing reals.
    !print *, fld
    !print *, fld2
    do ii = 1, ndpts
        if (abs(fld(ii) - fld2(ii)) .gt. delta) then
            print *, fld(ii), fld2(ii), 'do not match'
            stop 4
        end if
    end do

    print *, 'ok!'
    print *, 'SUCCESS!'
end program test_jpcpack
