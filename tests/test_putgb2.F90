! This program creates a grib file to use in testing other source files
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

  ! Open test file for reading. 
  call baopenr(1, "testgrib.grb2", iret)
  if (iret .ne. 0) stop 3

  ! Read a GRIB2 message.
  call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
       unpack, k, gfld, iret)
  if (iret .ne. 0) stop 4

  ! Close the file.
  call baclose(1, iret)
  if (iret .ne. 0) stop 5

  print *, 'gfld%ngrdpts from getgb2 is: ', gfld%ngrdpts
  print *, 'gfld%fld from getgb2 is: ', gfld%fld

  print *, 'Testing open/write/close of GRIB2 file'

  ! Open file for writing.
  call baopenw(2, "testgrib2.grb2", iret)
  if (iret .ne. 0) stop 6

  ! Write packed field out to message.
  ! call putgb2(2, gfld, iret)
  ! if (iret .ne. 0) then
  !    print *, 'putgb2 failed with iret value: ', iret
  !    stop 7
  ! end if

  ! Close file.
  call baclose(2, iret)
  if (iret .ne. 0) stop 8

  ! Free the memory.
  call gf_free(gfld)

  print *, 'SUCCESS!'

end program test_putgb2
