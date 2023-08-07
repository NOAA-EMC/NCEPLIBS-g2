! Test the getlocal call in NCEPLIBS-g2
!
! Brian Curtis 2/11/2022

program test_getlocal
    implicit none
    
    integer :: lengrib, lengribprime
    integer :: lcsec2, ierr, i, n1, n2, n3, Nan
    integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    integer :: localnum = 1
    character :: cgrib(200), fgrib(269), old_val, old_arr(4), tmp(71)
    character(len=1) :: csec2(3), csec2_comp(3)

    ! Initializing GRIB Message manually    
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
  
    ! Large numbers in message initials
    n1=229
    n2=254
    n3=255

    ! Message length
    lengribprime = 269
  
    ! Grib message from g2_decode
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

    ! Using util.F90 to make GRIB message
    call create_cgrib(cgrib, lengrib)
    print *, ''//NEW_LINE('A')//'Testing call to getlocal and comparing results...'
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)

    csec2_comp = (/ achar(1), achar(2), achar(3) /)

    if (ierr .ne. 0) stop 1
    if (lcsec2 .ne. 3) stop 2

    do i = 1, 3
        if (csec2(i) .ne. csec2_comp(i)) stop 3
    end do

    print *, 'Test: message not starting with "GRIB" ierr should be 1'
    old_val = cgrib(1)
    cgrib(1) = achar(0)
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 1) stop 4
    cgrib(1) = old_val

    print *, 'Test: not grib 2 format, ierr should be 2'
    old_val = cgrib(8)
    cgrib(8) = achar(1)
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 2) stop 5
    cgrib(8) = old_val

    print *, 'Test: call to getlocal with localnum=0, it should return ierr=3'
    localnum = 0
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 3) stop 6
    localnum = 1

    print *, 'Test: end string not at the end, ierr should be 4'
    ! Need to have '7777' at the beggining of a section i.e. offset 17
    old_arr = cgrib(17:20)
    cgrib(17:20) = cgrib(190)
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 4) stop 7
    cgrib(17:20) = old_arr

    print *, 'Test: no 7777 end string, ierr should be 5'
    ! Using manually input GRIB message because otherwise g2_gbytec causes memory error
    old_val = fgrib(lengribprime-1) 
    fgrib(lengribprime-1) = achar(1)
    tmp = fgrib(130:200)
    fgrib(130:200) = ''
    localnum = 2 
    call getlocal(fgrib, lengribprime, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 5) stop 8
    fgrib(lengribprime-1) = old_val
    fgrib(130:200) = tmp

    localnum = 3
    print *, 'Test: call to getlocal with localnum=3, it should return ierr=6'
    call getlocal(cgrib, lengrib, localnum, csec2, lcsec2, ierr)
    if (ierr .ne. 6) stop 9

    print *, ''//NEW_LINE('A')//'SUCCESS!!!...'

end program test_getlocal
