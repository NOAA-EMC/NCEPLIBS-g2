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
  parameter(lugi = 31, lugb = 11)

  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver = 2
  integer :: myidxver, nlen, nnum, ifldnum, iret, i
  integer :: j = 0, jdisc = -1, jpdtn = -1, jgdtn = -1
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: k, lpos
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
  if (iret .ne. 0) stop 2

  ! Open output file where index will be written.
  call baopen(lugi, BITMAP_FILE_INDEX, iret)
  if (iret .ne. 0) stop 3

  call g2_create_index(lugb, lugi, idxver, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 4

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 5
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 6

  call baopen(lugi, BITMAP_FILE_INDEX, iret)
  if (iret .ne. 0) stop 7

  ! Read the index file.
  call getg2i2(lugi, cbuf, myidxver, nlen, nnum, iret)
  if (iret .ne. 0) stop 8

  call baclose(lugi, iret)
  if (iret .ne. 0) stop 9
  if (myidxver .ne. idxver) stop 10
  !if (nlen .ne. 198) stop 11
  if (nlen .ne. 226) stop 11
  if (nnum .ne. 1) stop 12

  do i = 1, 13
    jids(i) = -9999
  end do
  jpdtn = -1
  do i = 1, 100
     jpdt(i) = -9999
  end do
  do i = 1, 250
     jgdt(i) = -9999
  end do
  call getgb2s2(cbuf, myidxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
       k, gfld, lpos, iret)
  if (iret .ne. 0) stop 20

  ! Free resources.
  deallocate(cbuf)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 200

  print *, 'SUCCESS!'
end program test_bitmap
