! This program creates a grib file to use in testing other source files
!
! Brian Curtis 11/26/2021
program test_putgb2
  use grib_mod
  use bacio_module
  implicit none

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
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .true.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret

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

  print *, 'gfld%ngrdpts from getgb2 is: ', gfld%ngrdpts
  print *, 'gfld%fld from getgb2 is: ', gfld%fld

  print *, 'Testing open/write/close of GRIB2 file'

  call baopenw(2, "testgrib2.grb2", iret)
  if (iret .ne. 0) then
     print *, 'baopenr failed with iret value: ', iret
     stop 6
  end if

  call putgb2(2, gfld, iret)
  if (iret .ne. 0) then
     print *, 'putgb2 failed with iret value: ', iret
     stop 7
  end if

  call baclose(2, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 8
  end if

  print *, 'SUCCESS!'

end program test_putgb2
