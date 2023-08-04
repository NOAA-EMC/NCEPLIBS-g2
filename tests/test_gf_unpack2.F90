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
    integer :: iofst, lencsec2, lensec, ierr, i
    integer :: isecnum, numlocal, numfields
    integer :: listsec0(3), listsec1(13), maxvals(7)
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

    ! To initialize large parameters in typical grib2 file.       
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

    ! The grib message for a typical grib2 file in fortran.
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

    print *, ''//NEW_LINE('A')//'Calling grib_info ...'
    call gribinfo(fgrib, fgrib_len, listsec0, listsec1,  &
    numlocal, numfields, maxvals, ierr)
    print *, 'Message length: ', listsec0(3)
    print *, 'Section 2 occurence: ', numlocal
    print *, 'Section 2 length: ', maxvals(1)
    

    print *, ''//NEW_LINE('A')//'Testging gf_unpack2 ...'

    ! Normal test, should return no error and allocate csec2
    ! Start byte = 37 => start bit = 296
    ! Length of section (in octets) is 6
    iofst = 296
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    if (ierr .ne. 0) stop 1
    deallocate(csec2)

    ! Offset not to section 2 data, should error without allocating csec2
    iofst = 1
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    if (ierr .ne. 6) stop 2

    if (.false.) then
        ! Offset to section 2 data with already allocated csec2 array
        ! should error and nullify csec2
        iofst = 296
        allocate(csec2(100))
        call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
        print *,'ierr: ', ierr
        if (ierr .ne. 6) stop 3
    

    ! Printing all offsets which return as section 2 in gf_unpack2
    print *,''//NEW_LINE('A')//'Offsets that give section 2:'

    do i=1, 465
        iofst = i
        ierr=0
        lencsec2=0
        nullify(csec2)

        call g2_gbytec(fgrib,lensec,iofst,32)        ! Get Length of Section
        iofst=iofst+32    
        lencsec2=lensec-5
        call g2_gbytec(fgrib,isecnum,iofst,8)         ! Get Section Number
        iofst=iofst+8   
        iofst=iofst+(lencsec2*8)

        if (isecnum .eq. 2) print *, 'Offset: ', i, 'length: ', lencsec2

    end do

    print *, ''//NEW_LINE('A')//'Confirming lengths with gf_unpack2:'
    iofst = 81
    i = iofst
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)
    print *, 'Offset: ', i, 'length: ', lencsec2
    iofst = 129
    i = iofst
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)
    print *, 'Offset: ', i, 'length: ', lencsec2
    iofst = 185
    i = iofst
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)
    print *, 'Offset: ', i, 'length: ', lencsec2
    iofst = 296
    i = iofst
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)
    print *, 'Offset: ', i, 'length: ', lencsec2
    iofst = 465
    i = iofst
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)
    print *, 'Offset: ', i, 'length: ', lencsec2

    print *, ''//NEW_LINE('A')//'Calling gf_unpack with offset 24 ...'
    iofst = 24
    call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
    deallocate(csec2)

    end if


    print *, ''//NEW_LINE('A')//'SUCCESS!'

end program test_gf_unpack2

! Lengths of section 2 offsets [1-2152]
! Offset:           24 length:   1107296251
! Offset:           81 length:           -5
! Offset:          117 length:    564133883
! Offset:          129 length:           37
! Offset:          142 length:       344123
! Offset:          169 length:      1048571
! Offset:          185 length:          507
! Offset:          257 length:    336860155
! Offset:          286 length:      4194299
! Offset:          296 length:            6
! Offset:          305 length:         5631
! Offset:          371 length:    177209339
! Offset:          465 length:           55
! Offset:          474 length:        30719
! Offset:          482 length:      7865341
! Offset:          490 length:   2013528573
! Offset:          498 length:     67240445
! Offset:          723 length:   1515870803
! Offset:          731 length:   1515870205
! Offset:          739 length:   1515717117
! Offset:          747 length:   1476526589
! Offset:          755 length:     33686013
! Offset:          763 length:     33686013
! Offset:          771 length:     33686013
! Offset:          779 length:     33686013
! Offset:          786 length:     16843004
! Offset:          859 length:  -1111638605
! Offset:          875 length:  -1112014211
! Offset:          883 length:  -1207795203
! Offset:          891 length:     42074621
! Offset:          899 length:  -2113797635
! Offset:          907 length:     33686013
! Offset:          915 length:     33686013
! Offset:          923 length:     33686013
! Offset:          930 length:     16843004
! Offset:          938 length:     16843005
! Offset:          946 length:     16843261
! Offset:          954 length:     16908797
! Offset:         1019 length:           -5
! Offset:         1062 length:  -1056964613
! Offset:         1073 length:           13
! Offset:         1088 length:       590083
! Offset:         1177 length:       396301
! Offset:         1289 length:           -5
! Offset:         1422 length:   1120125696
! Offset:         1433 length:    504899599
! Offset:         1473 length:           -5
! Offset:         1489 length:          507
! Offset:         1529 length:           19
! Offset:         1588 length:           -5
! Offset:         1648 length:        57595
! Offset:         1713 length:           -5
! Offset:         1729 length:          523
! Offset:         1737 length:       135165
! Offset:         1745 length:     34603517
! Offset:         2033 length:         6153