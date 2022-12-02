! This is a test program in the NCEPLIBS-g2 project.
!
! This program tests the getgb2() subroutine.
!
! Brian Curtis 11/26/2021
! Ed Hartnett
program test_getgb2
  use grib_mod
  use bacio_module
  use creategrib
  implicit none

  ! Define what I need
  integer, parameter :: lugb = 1
  integer :: lugi = lugb
  integer, parameter :: j = 0
  integer, parameter :: jdisc = 0
  integer :: jids(13) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)
  integer, parameter :: jpdtn = 0
  integer, parameter :: ipdstmplen = 15
  integer :: jpdt(ipdstmplen) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer, parameter :: jgdtn = 0
  integer, parameter :: igdstmplen = 19
  integer :: idrstmpl(5) = (/ 0, 1, 1, 8, 0 /)
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .true.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret
  integer :: i
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)
  real(8) :: fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)
  real, parameter :: EPSILON = .2 ! mighty large epsilon is required!

  print *, 'Testing open/read/close of GRIB2 file created with creategrib.f90..'
  print *, 'testing getgb2()..'

  ! Write the test file.
  call write_grib2_file()

  ! Open the test file for reading.
  call baopenr(1, "testgrib.grb2", iret)
  if (iret .ne. 0) stop 3

  ! Read a field from the test file.
  call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
       unpack, k, gfld, iret)
  if (iret .ne. 0) stop 4

  ! Check results.
  if (gfld%version .ne. 2) stop 10
  if (gfld%discipline .ne. 0) stop 20
  do i = 1, 13
     if (gfld%idsect(i) .ne. jids(i)) stop 30
  end do
  if (gfld%idsectlen .ne. 13) stop 40
  do i = 1, lcsec2
     if (gfld%local(i) .ne. csec2(i)) stop 50
  end do
  if (gfld%locallen .ne. lcsec2) stop 60
  if (gfld%ifldnum .ne. 1) stop 70
  if (gfld%griddef .ne. 0) stop 80
  if (gfld%ngrdpts .ne. 4) stop 90
  if (gfld%numoct_opt .ne. 0) stop 91
  if (gfld%interp_opt .ne. 0) stop 92
  if (gfld%num_opt .ne. 0) stop 100
  if (gfld%igdtnum .ne. 0) stop 101
  if (gfld%igdtlen .ne. 19) stop 110
  do i = 1, 19
     if (gfld%igdtmpl(i) .ne. jgdt(i)) stop 120
  end do
  if (gfld%ipdtnum .ne. 0) stop 130
  if (gfld%ipdtlen .ne. 15) stop 140
  do i = 1, 15
     if (gfld%ipdtmpl(i) .ne. jpdt(i)) stop 150
  end do
  if (gfld%num_coord .ne. 0) stop 160
  if (gfld%ndpts .ne. 4) stop 170
  if (gfld%idrtnum .ne. 0) stop 180
  if (gfld%idrtlen .ne. 5) stop 190
  ! The first value of the DRS template gets changed to an IEEE
  ! floating point reference value when the data are written. So the
  ! first value of gfld%idrtmpl will not match.
#ifdef KIND_4
  if (gfld%idrtmpl(1) .ne. -1583349760) stop 191
#else
  if (gfld%idrtmpl(1) .ne. 1093664768) stop 191
#endif
  do i = 2, 5
!    print *, gfld%idrtmpl(i), idrstmpl(i)
    if (gfld%idrtmpl(i) .ne. idrstmpl(i)) stop 200
  end do
  if (gfld%unpacked .neqv. .false.) stop 201
  if (gfld%ibmap .ne. 255) stop 203
!  print *, gfld%bmap(1)
  do i = 1, 4
!      print *, gfld%fld(i), fld(i), abs(gfld%fld(i) - fld(i))
      if (abs(gfld%fld(i) - fld(i)) .gt. EPSILON) stop 205
  end do

  ! Close file.
  call baclose(1, iret)
  if (iret .ne. 0) stop 5

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_getgb2
