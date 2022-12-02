! This is a test program in the NCEPLIBS-g2 project.
!
! This program tests the putgb2() subroutine.
!
! Brian Curtis 11/26/2021
! Ed Hartnett
program test_putgb2
  use grib_mod
  use bacio_module
  implicit none

  integer, parameter :: lugb = 1
  integer :: lugi = lugb
  integer, parameter :: j = 0
  integer, parameter :: jdisc = 0
  integer :: jids(13) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)  
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
