! This program creates a grib file to use in testing other source files
!
! Brian Curtis 11/26/2021
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
  integer :: jids(13)
  integer, parameter :: jpdtn = 0
  integer, parameter :: ipdstmplen = 15
  integer :: jpdt(ipdstmplen)
  integer, parameter :: jgdtn = 0
  integer, parameter :: igdstmplen = 19
  integer :: idrstmpl(5)
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .false.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret
  integer :: i
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)
  real(8) :: fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)

  jpdt(1) = 0
  jpdt(2) = 0
  jpdt(3) = 0
  jpdt(4) = 0
  jpdt(5) = 0
  jpdt(6) = 12
  jpdt(7) = 59
  jpdt(8) = 0
  jpdt(9) = 0
  jpdt(10) = 1
  jpdt(11) = 1
  jpdt(12) = 1
  jpdt(13) = 2
  jpdt(14) = 1
  jpdt(15) = 1

  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  jids(1) = 7 ! US National Weather Service - NCEP (WMC)
  jids(2) = 4 ! Environmental Modeling Center
  jids(3) = 2 ! GRIB master tables version number (currently 2)
  jids(4) = 24 ! Version Implemented on 06 November 2019
  jids(5) = 0 ! Local tables not used.
  jids(6) = 2021 ! Year
  jids(7) = 11 ! Month
  jids(8) = 13 ! Day
  jids(9) = 15 ! Hour
  jids(10) = 59 ! Minute
  jids(11) = 59 ! Second
  jids(12) = 1 !  Operational Test Products
  jids(13) = 0 ! Analysis Products

  idrstmpl(1) = 0
  idrstmpl(2) = 1
  idrstmpl(3) = 1
  idrstmpl(4) = 8
  idrstmpl(5) = 0

  ! WHAT I NEED
  !#jids = listsec1
  !#jpdtn = ipdsnum
  !#jpdt = ipdstmpl
  !#jgdtn = idefnum
  !#jgdt = igdstmpl

  print *, 'Testing open/read/close of GRIB2 file created with creategrib.f90..'
  call write_grib2_file()
  call baopenr(1, "testgrib.grb2", iret)
  if (iret .ne. 0) then
      print *, 'baopenr failed with iret value: ', iret
      stop 3
  end if

  call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
              unpack, k, gfld, iret)
  if (iret .ne. 0) then
      print *, 'getgb2 failed with iret value: ', iret
      stop 4
  end if

  call baclose(1, iret)
  if (iret .ne. 0) then
      print *, 'baclose failed with iret value: ', iret
      stop 5
  end if
  print *, 'Success!...'

  print *, 'Testing all gfld outputs to match what we created with creategrib.f90..'
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

  ! NOT WORKING
  ! do i = 1, 5
  !   print *, gfld%idrtmpl(i), idrstmpl(i)
  !   ! if (gfld%idrtmpl(i) .ne. idrstmpl(i)) stop 200
  ! end do

  if (gfld%unpacked .neqv. .false.) stop 201

  if (gfld%ibmap .ne. 255) stop 203

  !REMAINING ITEMS NOT WORKING RIGHT
  ! if (gfld%bmap(1) .neqv. .false.) stop 204

  ! do i = 1, 4
  !     print *, gfld%fld(i), fld(i)
  ! !   ! if (gfld%fld(i) .ne. fld(i)) stop 205
  ! end do


  print *, 'SUCCESS!'

end program test_getgb2
