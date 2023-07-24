! This is a test program for NCEPLIBS-g2.
!
! This program tests the gf_unpack2.f file.
!
! Andrew King 6/23/23

 program test_gf_unpack2
    
    implicit none

    character, dimension(269) :: fgrib
    integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    integer :: n1, n2, n3, nan, fgrib_len
    integer :: iofst, lensec2, ierr, i
    character(len=1),pointer,dimension(:) :: csec2

    interface
        subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
        character(len = 1), intent(in) :: cgrib(lcgrib)
        integer, intent(in) :: lcgrib
        integer, intent(inout) :: iofst
        integer, intent(out) :: lencsec2
        integer, intent(out) :: ierr
        character(len = 1), pointer, dimension(:) :: csec2
        end subroutine gf_unpack2
    end interface

    k1=6371200                   ! Radius of the earth
    k2=1073                      ! Nx for lambert 5km
    k3=689                       ! Ny for lambert 5km
    k4=2019199                   ! La1 - latitude of first grid point 
    k5=2384459                   ! Lo1 - longitude of first grid point
    k6=25000000                  ! Latitude where grid spacing is defined
    k7=265000000                 ! Orientation longitude
    k8=2539703                   ! X & Y direction grid length
    k9=-90000000                 ! Latitude of pole 
    k10=739297                   ! Number of grid Nx*Ny
    Nan=9999                     ! Missing value
    fgrib_len=269                ! Grib message length

    ! Large numbers in message initials
    n1=229
    n2=254
    n3=255

    ! The grib message from test_g2_decode
    fgrib(:)=(/                                                    &
        ! section 0
    & "G", "R", "I", "B", char(0), char(0),                        &
    & char(0), char(2), char(0), char(0), char(0), char(0),    &
    & char(0), char(0), char(1), char(13),                       &
        ! section 1
    & char(0), char(0), char(0), char(21), char(1), char(0),   &
    & char(8), char(0), char(0), char(1), char(0), char(1),    &
    & char(7), char(n1), char(11), char(15), char(10),          &
    & char(10), char(10), char(0), char(1),                      &
        ! section 2
    & char(0), char(0), char(0), char(11), char(2),             &
    & char(1), "H", "K", "T", "A", "R",                             &
        ! section 3
    & char(0), char(0), char(0), char(81), char(3), char(0),   &
    & char(0), char(0), char(0), char(k10), char(0), char(0),  &
    & char(0), char(30), char(1), char(0), char(k1), char(k1), &
    & char(k1), char(k1), char(0), char(0), char(0), char(0),  &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
    & char(k2), char(k2), char(k2), char(k2), char(k3),         &
    & char(k3), char(k3), char(k3), char(k4), char(k4),         &
    & char(k4), char(k4),char(k5),char(k5), char(k5),           &
    & char(k5), char(0), char(k6), char(k6), char(k6),          &
    & char(k6), char(k7), char(k7), char(k7), char(k7),         &
    & char(k8), char(k8), char(k8), char(k8), char(k8),         &
    & char(k8), char(k8), char(k8), char(0), char(80),          &
    & char(k6), char(k6), char(k6), char(k6), char(k6),         &
    & char(k6), char(k6), char(k6), char(k9), char(k9),         &
    & char(k9), char(k9), char(0), char(0), char(0), char(0),  &
        ! section 4
    & char(0), char(0), char(0), char(71), char(4), char(0),   &
    & char(0), char(0), char(9), char(1), char(8), char(2),    &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
    & char(0), char(3), char(6), char(9), char(1), char(0),    &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
    & char(0), char(0), char(0),  char(0), char(0), char(0),   &
    & char(1), char(0), char(0), char(0), char(0), char(0),    &
    & char(3), char(0), char(0), char(0), char(n2), char(7),   & 
    & char(n1), char(11), char(15), char(12), char(20),         &
    & char(10), char(1), char(0), char(0), char(0), char(0),   &
    & char(1), char(0), char(1), char(0), char(0), char(0),    &
    & char(12), char(1), char(0), char(0), char(0), char(0),   &
        ! section 5
    & char(0), char(0), char(0), char(47), char(5),  char(0),  &
    & char(0), char(0), char(k10), char(0), char(2),  char(0), &
    & char(0), char(0), char(0), char(0), char(0),  char(0),   &
    & char(1), char(8), char(1), char(1), char(1), char(Nan),  &
    & char(Nan), char(Nan), char(Nan), char(0), char(0),        &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
    & char(0), char(0), char(0), char(0), char(0), char(0),    &
        ! section 6
    & char(0), char(0), char(0), char(6), char(6),  char(n3),  &
        ! section 7
    & char(0), char(0), char(0), char(12), char(7),  char(1),  &
    & char(0), char(0), char(0), char(0), char(0),  char(0),   &
        ! section 8
    & "7", "7", "7", "7" /) 

    print *, 'Testging gf_unpack2'

    ! Normal test, should return no error and allocate csec2
    iofst = 129
    call gf_unpack2(fgrib, fgrib_len, iofst, lensec2, csec2, ierr)
    if (ierr .ne. 0) stop 1
    deallocate(csec2)

    ! Offset not to section 2 data, should error without allocating csec2
    iofst = 130
    call gf_unpack2(fgrib, fgrib_len, iofst, lensec2, csec2, ierr)
    if (ierr .ne. 6) stop 2

    if (.false.) then
        ! Offset to section 2 data with already allocated csec2 array, should error and nullify csec2
        iofst = 129
        allocate(csec2(100))
        call gf_unpack2(fgrib, fgrib_len, iofst, lensec2, csec2, ierr)
        print *,'ierr: ', ierr
        if (ierr .ne. 6) stop 3
        

        ! Printing all offsets which return as section 2 in gf_unpack2
        print *,''//NEW_LINE('A')//'Offsets that give section 2:'

        do i=1, 269
            iofst = i
            if(i .ne. 24 .and. i .ne. 117 .and. i .ne. 142 .and. i .ne. 169 .and. i .ne. 257) then
                call gf_unpack2(fgrib, fgrib_len, iofst, lensec2, csec2, ierr)
                if (ierr .eq. 0) deallocate(csec2)
            end if
        end do
    end if

    print *, ''//NEW_LINE('A')//'SUCCESS!'

end program test_gf_unpack2

! Lengths of section 2 offsets
! iofst   lensec
! 24      1107296251
! 81      -5
! 117     564133883
! 129     37
! 142     344123
! 169     1048571
! 185     507
! 257     336860155