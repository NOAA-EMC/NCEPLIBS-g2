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
  integer, parameter :: idxver = 2

  character(len=1), dimension(:), pointer :: cbuf
  integer :: iret, nnum, nlen, numlocal, numfields
  integer :: listsec0(3), listsec1(13), maxvals(7)
  !integer, dimension(*):: igds, igdstmpl, ideflist, ipdstmpl, coordlist, &
  !    idrstmpl, bmap, fld
  !integer :: igdslen, idefnum, ipdsnum, ipdslen, numcoord, ndpts, idrsnum, &
  !    idrslen, ibmap

  interface
    subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
      integer, intent(in) :: lugb, lugi, idxver
      character*(*) :: filename
      integer, intent(out) :: iret
    end subroutine g2_create_index
    subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
      integer, intent(in) :: lugb, lugi, idxver
      character(len = 1), pointer, dimension(:) :: cindex
      integer, intent(out) :: nlen, nnum, iret
    end subroutine getidx2
  end interface

  ! Open GRIB2 file for reading.
  call baopenr(lugb, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 2

  ! Open output file where index will be written.
  call baopen(lugi, BITMAP_FILE_INDEX, iret)
  if (iret .ne. 0) stop 3

  call g2_create_index(lugb, lugi, idxver, BITMAP_FILE, iret)
  if (iret .ne. 0) stop 4

  call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 5
  if (nnum .ne. 1) stop 6
  if (nlen .ne. 226) stop 7

  call gribinfo(cbuf, nnum, listsec0, listsec1, numlocal, numfields, maxvals, iret)
  !call getfield(cbuf, nnum, 6, igds, igdstmpl, igdslen, ideflist, idefnum, &
  !     ipdsnum, ipdstmpl, ipdslen, coordlist, numcoord, ndpts, idrsnum, &
  !     idrstmpl, idrslen, ibmap, bmap, fld, iret)
  if (iret .ne. 0) stop 8

  print *, maxvals(1)
  print *, maxvals(2)
  print *, maxvals(3)
  print *, maxvals(4)
  print *, maxvals(5)
  print *, maxvals(6)
  print *, maxvals(7)

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 100
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 101

  ! Free resources.
  call gf_finalize(iret)
  if (iret .ne. 0) stop 102

  print *, 'SUCCESS!'
end program test_bitmap
