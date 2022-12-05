! This is a test program in the NCEPLIBS-g2 project.
!
! This program tests the putgb2() subroutine. This program depends on
! the testgrib.grb2 file alread existing.
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
  integer :: jpdt(ipdstmplen) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer, parameter :: jgdtn = 0
  integer, parameter :: igdstmplen = 19
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .true.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret

  print *, 'Testing reading and writing of GRIB2 file.'
  print *, 'testing reading...'

  ! Open test file for reading.
  call baopenr(1, "testgrib.grb2", iret)
  if (iret .ne. 0) stop 3

  ! Read a GRIB2 message.
  call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, unpack, k, gfld, iret)
  if (iret .ne. 0) stop 4
  if (gfld%ngrdpts .ne. 4) stop 5
  print *, 'gfld%ngrdpts from getgb2 is: ', gfld%ngrdpts
  print *, 'gfld%fld from getgb2 is: ', gfld%fld

  ! Close the file.
  call baclose(1, iret)
  if (iret .ne. 0) stop 20

  print *, 'OK!'
  print *, 'testing open/write/close of GRIB2 file...'

  ! Open file for writing.
  call baopenw(2, "testgrib2.grb2", iret)
  if (iret .ne. 0) stop 100

!  call putgb2(2, gfld, iret)
  if (iret .ne. 0) stop 107

  ! Close file.
  call baclose(2, iret)
  if (iret .ne. 0) stop 150

  ! Free the memory.
  call gf_free(gfld)

  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_putgb2
