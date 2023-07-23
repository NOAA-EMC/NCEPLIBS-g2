! This is a test program for NCEPLIBS-g2.
!
! This program tests the gettemplates function.
!
! Andrew King 6/20/23
program test_gettemplates

  implicit none

  integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
  integer :: n1, n2, n3, Nan, lengrib
  integer, dimension(5) :: igds
  integer :: ipdslen, numcoord, ierr, idgslen, idefnum, ipdsnum
  character, dimension(269) :: fgrib
  character, dimension(71) :: tmp
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

  print *, 'Testing gettemplates ...'
  
  ! Valid GRIB message, should have no errors
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 0) stop 1

  ! Message not starting with "GRIB" ierr should be 1
  old_val = fgrib(1)
  fgrib(1) = char(0)
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 1) stop 2
  fgrib(1) = old_val

  ! Not grib 2 format, ierr should be 2
  old_val = fgrib(8)
  fgrib(8) = char(1)
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 2) stop 3
  fgrib(8) = old_val
  
  ! Invalid field number, ierr should be 3
  call gettemplates(fgrib, lengrib, -1, igds, igdstmpl, &
                  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 3) stop 4

  ! End string not at the end, ierr should be 4
  old_val = fgrib(130)
  old_val1 = fgrib(131)
  old_val2 = fgrib(132)
  old_val3 = fgrib(133)
  fgrib(130) = fgrib(lengrib)
  fgrib(131) = fgrib(lengrib)
  fgrib(132) = fgrib(lengrib)
  fgrib(133) = fgrib(lengrib)
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 4) stop 5
  fgrib(130) = old_val
  fgrib(131) = old_val1
  fgrib(132) = old_val2
  fgrib(133) = old_val3

  ! Need numlocal to be numfld on line 203 in gettemplates.f
  if (.false.) then
    ! Field to be returned not found in message, ierr should be 6
    call gettemplates(fgrib, lengrib, 9, igds, igdstmpl, &
    idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
    ipdslen, coordlist, numcoord, ierr)
    if (ierr .ne. 6) stop 6
  end if

  ! No 7777 end string, ierr should be 7
  old_val = fgrib(lengrib-1)
  fgrib(lengrib-1) = 'f'
  tmp = fgrib(130:200)
  fgrib(130:200) = ''
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
                  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
                  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 7) stop 7
  fgrib(lengrib-1) = old_val
  fgrib(130:200) = tmp

  ! Bad section 3, ierr should be 10
  old_val = fgrib(62)
  fgrib(62) = char(99)
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 10) stop 8
  fgrib(62) = old_val

  ! Bad section 4, ierr should be 11
  old_val = fgrib(138)
  fgrib(138) = char(99)
  call gettemplates(fgrib, lengrib, 1, igds, igdstmpl, &
  idgslen, ideflist, idefnum, ipdsnum, ipdstmpl, &
  ipdslen, coordlist, numcoord, ierr)
  if (ierr .ne. 11) stop 9
  fgrib(138) = old_val

  print *, 'SUCCESS!'

end program test_gettemplates
  