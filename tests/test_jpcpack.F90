! This program tests the jpcpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-18
program test_jpcpack

    implicit none

    integer, parameter :: width = 2, height = 2, ndpts = 4
    real(8), parameter :: delta = 0.1
    real(8) :: fld_orig(ndpts)
    real(8) :: fld(ndpts)
    real(8) :: fld2(ndpts)
    integer :: idrstmpl(16)
    character*1 :: cpack(200)
    integer :: lcpack
    integer :: ii

    ! Create the fld variable with data to pack
    fld_orig = (/1.1, 2.2, 3.3, 4.4/)
    fld = fld_orig

    idrstmpl(1) = 0
    idrstmpl(2) = 1
    idrstmpl(3) = 1
    idrstmpl(4) = 32
    idrstmpl(5) = 0
    idrstmpl(6) = 0

    ! Testing jpcpack
    call jpcpack(fld, width, height, idrstmpl, cpack, lcpack)
    print *, 'lcpack: ', lcpack
    ! Testing jpcunpack
    call jpcunpack(cpack, lcpack, idrstmpl, ndpts, fld2)

    ! Compare each value to see match, remember, comparing reals
    print *, fld_orig
    print *, fld
    print *, fld2
    do ii = 1, ndpts
        print *, fld_orig(ii)
        if (dabs(fld_orig(ii) - fld2(ii)) .gt. delta) then
            print *, fld_orig(ii), fld2(ii), 'do not match'
            stop 4
        end if
    end do

    print *, 'SUCCESS!'

end program test_jpcpack
