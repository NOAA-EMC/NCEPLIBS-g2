! This is a test program for NCEPLIBS-g2.
!
! This program tests the contents of a file downloaded from the FTP
! site, blend.t19z.core.f001.co.grib2.
!
! Ed Hartnett 8/11/23
program test_file_blend
  use grib_mod
  implicit none

  character*(*) :: FILE_NAME
  parameter (FILE_NAME = 'data/blend.t19z.core.f001.co.grib2')
  integer :: LUGI
  parameter (LUGI = 3)
  integer :: iret

  print *, 'Testing reading file ', FILE_NAME

  ! Open the file.
  call baopenr(LUGI, FILE_NAME, iret)
  if (iret .ne. 0) stop 3

  ! Close the file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_file_blend
