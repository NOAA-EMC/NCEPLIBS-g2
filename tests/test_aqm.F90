! This is a test program for NCEPLIBS-g2.
!
! This program tests reading a GRIB2 file with a negative forecast time.
!
! Ed Hartnett 6/28/23
program test_aqm
  use grib_mod
  implicit none
  
  integer :: lugb = 10
  integer :: jdisc = -1, jpdtn = -1, jgdtn = -1, jskp = 0
  integer, dimension(200) :: jids, jpdt, jgdt
  logical :: unpack = .true.
  type(gribfield) :: gfld
  integer :: iret
  
  print *, 'Testing reading GRIB2 file aqm.t12z.max_8hr_o3.227.grib2...'

  ! Open the file.
  call baopenr(lugb, 'data/aqm.t12z.max_8hr_o3.227.grib2', iret)
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

  ! Free memory in derived type gribfield.
  call gf_free(gfld)
  
  ! Free library memory used by getidx().
  call gf_finalize(iret)
  if (iret .ne. 0) stop 201
  
  print *, 'SUCCESS!'
end program test_aqm
