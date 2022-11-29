! This is a test program for NCEPLIBS-g2.
!
! This program tests reading a GRIB2 file.
!
! Ed Hartnett 9/3/22
program test_files
  use grib_mod
  implicit none
  
  integer :: lugb = 10
  integer :: jdisc = -1, jpdtn = -1, jgdtn = -1, jskp = 0
  integer, dimension(200) :: jids, jpdt, jgdt
  logical :: unpack = .true.
  type(gribfield) :: gfld
  integer :: iret
  
  print *, 'Testing reading GRIB2 file gdaswave.t00z.wcoast.0p16.f000.grib2...'

  ! Open the file.
  call baopenr(lugb, 'data/gdaswave.t00z.wcoast.0p16.f000.grib2', iret)
  if (iret .ne. 0) stop 2

  ! Learn about the file.
  jids = -9999
  jpdt = -9999
  jgdt = -9999
  call getgb2(lugb, 0, jskp, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          unpack, jskp, gfld, iret)
  if (iret .ne. 0) stop 3

  ! Close the file.
  call baclose(lugb, iret)  
  if (iret .ne. 0) stop 200
  
  print *, 'SUCCESS!'
end program test_files
