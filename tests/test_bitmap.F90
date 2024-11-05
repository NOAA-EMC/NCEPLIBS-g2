! This is a test program for NCEPLIBS-g2.
!
! This program tests processing a GRIB2 file with bitmaps.
!
! Alyson Stahl 11/5/24
program test_bitmap
  use bacio_module
  use grib_mod
  implicit none

  character(*) :: BITMAP_FILE
  parameter(BITMAP_FILE = 'data/ref_png_bitmap.png')

  integer :: LUGB, LUGI
  parameter(LUGB = 3, LUGI = 4)
  integer :: iret

  call baopenr(LUGI, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 3

  print *, 'SUCCESS!'
end program test_bitmap
