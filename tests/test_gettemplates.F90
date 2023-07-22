! This is a test program for NCEPLIBS-g2.
!
! This program tests the gettemplates function.
!
! Andrew King 6/20/23
program test_gettemplates

    implicit none

    integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    integer :: n1, n2, n3, Nan, lengrib, i
    integer, dimension(5) :: igds
    integer :: ipdslen, numcoord, ierr, idgslen, idefnum, ipdsnum
    character, dimension(269) :: fgrib
    integer :: listsec0(3), listsec1(13), maxvals(7)
    integer :: numlocal, numfields 
    integer, dimension(67) :: igdstmpl
    integer, dimension(62) :: ipdstmpl
    integer, dimension(225) :: ideflist
    real :: coordlist(225)
    character :: old_val, old_val1, old_val2, old_val3
  
    ! Variables in grib message
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
  
    ! Large numbers in message initials
    n1=229
    n2=254
    n3=255

    ! Message length
    lengrib = 269
  
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

  
    !print *, 'Calling gribinfo ...'
    !call gribinfo(fgrib, lengrib, listsec0, listsec1,  &
    !            numlocal, numfields, maxvals, ierr)
    !print *, 'Max igdstmpl: ', maxvals(2), ', size: ', size(igdstmpl)
    !print *, 'Max ipdstmpl: ', maxvals(4), ', size: ', size(ipdstmpl)
    ! gettemplates says this should be max for coordlist but gribinfo says its PDS optional list
    !print *, 'Max drs: ', maxvals(6)
    !print *, 'Max coordlist(7): ', maxvals(7), ', size: ', size(coordlist)
  
    print *, 'Testing gettemplates ...'
    
    ! Valid GRIB message, should have no errors
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 0) stop 199
    
    ! Invalid field number, ierr should be 3
    call gettemplates(fgrib, lengrib, -1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 3) stop 1
    
    ! Message not starting with "GRIB" ierr should be 1
    old_val = fgrib(1)
    fgrib(1) = char(0)
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 1) stop 2
    fgrib(1) = old_val

    ! SKIPPING THIS FOR NOW
    if (.false.) then

    ! End string not at the end, ierr should be 4
    old_val = fgrib(lengrib-1)
    print *,'old val: ', old_val
    fgrib(lengrib-1) = "f"
    print *,'new val: ', fgrib(lengrib-1)
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    print *,'ierr7: ', ierr
    if (ierr .ne. 7) stop 6
    fgrib(lengrib-1) = old_val
    
    ! Bad section 3, ierr should be 10
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    print *, 'ierr call 4: ', ierr
    if (ierr .ne. 10) stop 4
    
    ! Bad section 4, ierr should be 10
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    print *, 'ierr call 5: ', ierr
    if (ierr .ne. 11) stop 5
    
    ! No 7777 end string, ierr should be 7
    old_val = fgrib(205)
    old_val1 = fgrib(206)
    old_val2 = fgrib(207)
    old_val3 = fgrib(208)
    fgrib(205) = fgrib(lengrib)
    fgrib(206) = fgrib(lengrib)
    fgrib(207) = fgrib(lengrib)
    fgrib(208) = fgrib(lengrib)
    do i=1,6
        print *, '<', fgrib(203+i), '>'
    end do
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    print *,'ierr7: ', ierr
    if (ierr .ne. 4) stop 3
    fgrib(205) = old_val
    fgrib(206) = old_val1
    fgrib(207) = old_val2
    fgrib(208) = old_val3
    
    ! Not grib 2 format, ierr should be 2
    call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 2) stop 7
      
    ! Field to be returned not found in message, ierr should be 6
    call gettemplates(fgrib, lengrib, 9, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 6) stop 8

    end if
  
    print *, 'SUCCESS!'
  
  end program test_gettemplates
  