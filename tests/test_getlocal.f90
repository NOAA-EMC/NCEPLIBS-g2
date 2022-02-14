! Test the getlocal call in NCEPLIBS-g2
!
! Brian Curtis 2/11/2022

program test_getlocal
    use creategrib
    implicit none
    
    integer :: lengrib
    character :: cgrib(200)
    integer :: localnum = 1
    character(len=1) :: csec2(3), csec2_comp(3)
    integer :: lcsec2, ierr, i

    call create_cgrib(cgrib, lengrib)
    print *, 'Testing call to getlocal and comparing results...'
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)

    csec2_comp = (/ achar(1), achar(2), achar(3) /)

    if (ierr .ne. 0) stop 3
    if (lcsec2 .ne. 3) stop 4

    do i = 1, 3
        if (csec2(i) .ne. csec2_comp(i)) stop 5
    end do

    print *, 'SUCCESS!!!...'

end program test_getlocal