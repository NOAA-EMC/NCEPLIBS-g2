! This is a test program for NCEPLIBS-g2.
!
! This program tests the gettemplates function.
!
! Andrew King 6/20/23
program test_gettemplates

  !implicit none
  integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
  integer :: n1, n2, n3, Nan
  integer, dimension(5) :: igds
  integer :: ipdslen, numcoord, ierr, idgslen, idefnum, ipdsnum
  character, dimension(269) :: fgrib
  character, dimension(269) :: fgrib_3
  character, dimension(269) :: fgrib_4
  character, dimension(269) :: fgrib_nog2
  character, dimension(265) :: fgrib_nogrib
  character, dimension(188) :: fgrib_7777
  character, dimension(184) :: fgrib_no7777
  integer :: listsec0(3), listsec1(13), maxvals(7)
  integer :: numlocal, numfields 
  integer, dimension(67) :: igdstmpl
  integer, dimension(62) :: ipdstmpl
  integer, dimension(225) :: ideflist
  real :: coordlist(225)

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

    ! Grib message with no 'GRIB'
    fgrib_nogrib(:)=(/                                                    &
    ! section 0
    & char(0), char(0),                        &
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
    ! Grib message with early ending string
    ! -no section 3, 7777 at beginning of section 7
    fgrib_7777(:)=(/                                                    &
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
    & "7", "7", "7", "7", char(0), char(0), &
    & char(0), char(12), char(7),  char(1), char(0), char(0), &
      ! section 8
    &  char(0), char(0), char(0),  char(0) /) 

  ! Grib message bad section 3 
  fgrib_3(:)=(/                                                    &
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

  ! Grib message bad section 4
  fgrib_4(:)=(/                                                    &
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

  ! Grib message not in GRIB2
  fgrib_nog2(:)=(/                                                    &
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
  
  ! Grib message with no end string and no section 3
  fgrib_no7777(:)=(/                                                    &
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
    & char(0), char(0), char(0), char(0), char(0),  char(0) /) 


  print *, 'Calling gribinfo ...'
  call gribinfo(fgrib, 269, listsec0, listsec1,  &
                numlocal, numfields, maxvals, ierr)
  print *, 'Max igdstmpl: ', maxvals(2), ', size: ', size(igdstmpl)
  print *, 'Max ipdstmpl: ', maxvals(4), ', size: ', size(ipdstmpl)
  ! gettemplates says this should be max for coordlist but gribinfo says its PDS optional list
  print *, 'Max drs: ', maxvals(6)
  print *, 'Max coordlist(7): ', maxvals(7), ', size: ', size(coordlist)

  print *, 'Testing gettemplates ...'
  ! Wrong field return number, ierr should be 3
  print *, '-----------------call 1-----------------'
  call gettemplates(fgrib, 269, -1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 3) stop 1
  ! Message not starting with "GRIB" ierr should be 1
  print *, '-----------------call 2-----------------'
  call gettemplates(fgrib_nogrib, 265, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 1) stop 2
    ! End string not at the end, ierr should be 4
  print *, '-----------------call 3-----------------'
  call gettemplates(fgrib_7777, 188, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 4) stop 3
  
  ! SKIPPING THIS FOR NOW
  if (.false.) then
  ! Bad section 3, ierr should be 10
  print *, '-----------------call 4-----------------'
  call gettemplates(fgrib_3, 269, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  print *, 'ierr call 4: ', ierr
  if (ierr .ne. 10) stop 4
  ! Bad section 4, ierr should be 10
  print *, '-----------------call 5-----------------'
  call gettemplates(fgrib_4, 269, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  print *, 'ierr call 5: ', ierr
  if (ierr .ne. 11) stop 5
  ! No 7777 end string, ierr should be 7
  print *, '-----------------call 6-----------------'
  call gettemplates(fgrib_no7777, 184, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 7) stop 6
  ! Not grib 2 format, ierr should be 2
  print *, '-----------------call 7-----------------'
  call gettemplates(fgrib_nog2, 269, 1, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 2) stop 7
  end if

  ! Section to be returned not valid section number, ierr should be 6
  print *, '-----------------call 8-----------------'
  call gettemplates(fgrib, 269, 9, igds, igdstmpl, &
                    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                    ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 6) stop 8


  print *, 'SUCCESS!'

end program test_gettemplates
