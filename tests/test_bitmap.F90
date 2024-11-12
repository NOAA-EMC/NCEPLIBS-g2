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
  character(*) :: BITMAP_FILE_INDEX
  parameter(BITMAP_FILE_INDEX = 'test_bitmap_index.grb2index')

  integer :: lugi, lugb
  parameter(lugi = 11, lugb = 10)

  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver, myidxver, nlen, nnum, ifldnum, iret
  type(gribfield) :: gfld

  interface
    subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
      integer, intent(in) :: lugi
      character(len=1), pointer, dimension(:) :: cbuf
      integer, intent(out) :: idxver, nlen, nnum, iret
    end subroutine getg2i2
    subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
      integer, intent(in) :: lugb, lugi, idxver
      character*(*) :: filename
      integer, intent(out) :: iret
    end subroutine g2_create_index
  end interface

  ! Open GRIB2 file for reading.
  call baopenr(lugb, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 3

  ! Open output file where index will be written.
  call baopen(lugi, BITMAP_FILE_INDEX, iret)
  if (iret .ne. 0) stop 4

  call g2_create_index(lugb, lugi, idxver, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 5

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 6

  ! Read the index file.
  call getg2i2(lugi, cbuf, myidxver, nlen, nnum, iret)
  if (iret .ne. 0) stop 7

  call baclose(lugi, iret)
  if (iret .ne. 0) stop 8

  ifldnum = 6
  call gf_getfld(cbuf, nlen, ifldnum, .true., .true., gfld, iret)
  if (iret .ne. 0) stop 9

  ! Free resources.
  call gf_free(gfld)

  print *, 'SUCCESS!'
end program test_bitmap
