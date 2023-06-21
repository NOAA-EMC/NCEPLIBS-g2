! This is a test program for NCEPLIBS-g2.
!
! This program tests the gettemplates function.
!
! Andrew King 6/20/23
program test_gettemplates
    implicit none
    integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
    integer :: n1, n2, n3, Nan
    character, dimension(269) :: fgrib

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
  
    print *, 'SUCCESS!'
  end program test_gettemplates
