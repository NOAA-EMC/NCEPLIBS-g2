! Test the g2grids.f src file
! Brian Curtis 2021/12/09

program test_g2grids
    use bacio_module
    use g2grids
    implicit none

    integer :: igdtn, igdtmpl(200)
    character(len=9) :: namein = "cartesian"

    integer :: iret, count


    OPEN(1, FILE='testgrib.grb2')
    count = readgrids(1)
    print *, 'Count: ', count
    ! getgridby<> subroutines use readgrids
    call getgridbynum(1, 0, igdtn, igdtmpl, iret)
    print *, 'iret: ', iret
    if (iret .ne. 0) stop 10
    call getgridbyname(1, namein, igdtn, igdtmpl, iret)
    if (iret .ne. 0) stop 20

    ! call baclose(1, iret)
    ! if (iret .ne. 0) stop 30

end program test_g2grids
