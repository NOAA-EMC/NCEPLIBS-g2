! This program tests the jpcpack subroutine of the NCEPLIBS-g2
! project.
!
! Brian Curtis 2021-10-18
program test_jpcpack

    implicit none

    integer, parameter :: width=2, height=2, ndpts=4
    real(kind=8) :: fld(ndpts), fld2(ndpts)
    integer :: idrstmpl(7)
    character(len=1) :: cpack(200)
    integer :: lcpack
    integer :: i

    ! Create the fld variable with data to pack
    fld = (/1.0, 2.0, 3.0, 4.0/)

    ! No idea what this needs to be, documentation confusing
    idrstmpl(1)=0
    idrstmpl(2)=1
    idrstmpl(3)=1
    idrstmpl(4)=21
    idrstmpl(5)=0
    idrstmpl(6)=0
    idrstmpl(7)=0

    ! Testing jpcpack
    call jpcpack(fld, width, height, idrstmpl, cpack, lcpack)

    ! Testing jpcunpack
    call jpcunpack(cpack, lcpack, idrstmpl, ndpts, fld2)
    
    ! Compare each value to see match, reals do not compare well
    do i = 1, ndpts
        if (fld(i) .ne. fld2(i)) then
            print *, fld(i), fld2(i), 'do not match'
            stop 4
        end if
    end do

    print *, 'SUCCESS!'

end program test_jpcpack
