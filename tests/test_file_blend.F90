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
  character*(2000) :: cbuf
  integer numfld, mlen, lgrib
  integer :: iret

  print *, 'Testing reading file ', FILE_NAME

  ! Open the file.
  call baopenr(LUGI, FILE_NAME, iret)
  if (iret .ne. 0) stop 3

  lgrib = 2000
  call ixgb2(lugi, 0, lgrib, cbuf, numfld, mlen, iret)
  print *,iret, numfld, mlen
  if (iret .ne. 0) stop 101
  if (numfld .ne. 1 .or. mlen .ne. 235) stop 102

  ! Close the file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_file_blend
