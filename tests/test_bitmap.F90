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

  integer :: idxver = 2, j = 0, jdisc = 0, jpdtn = 0, jgdtn = 0
  integer :: iret, k, i
  integer :: jids(13), jpdt(100), jgdt(250)
  logical :: unpack = .false.

  integer :: expected_idsect(13) = (/ 57, 90, 2, 0, 0, 2021, 4, 25, 0, 0, 0, 0, 1/)
  integer :: expected_ipdtmpl(15) = (/ 19, 10, 0, 0, 92, 0, 0, 1, 0, 105, 0, 10, 255, 0, 255 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 5760, 2882, 0, 0, -90000000, &
      180000000, 48, 90000000, 179937500, 62500, 62500, 64/)
  integer :: expected_idrtmpl(5) = (/ 0, 0, 0, 3, 0 /)
  type(gribfield) :: gfld

  interface
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

  jids = -9999
  jpdt = -9999
  jgdt = -9999

  call getgb2i2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, unpack, idxver, k, gfld, iret)
  if (iret .ne. 0) stop 10
  if (k .ne. 1) stop 11
  if (gfld%version .ne. 2 .or. gfld%discipline .ne. 0 .or. gfld%idsectlen .ne. 13 .or. &
       gfld%locallen .ne. 0 .or. gfld%ifldnum .ne. 1 .or. gfld%griddef .ne. 0 .or. &
       gfld%ngrdpts .ne. 16600320 .or. gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. &
       gfld%num_opt .ne. 0 .or. gfld%igdtnum .ne. 0 .or. gfld%igdtlen .ne. 19 .or. &
       gfld%ipdtnum .ne. 0 .or. gfld%ipdtlen .ne. 15 .or. gfld%ndpts .ne. 16600303 .or. &
       gfld%idrtnum .ne. 0 .or. gfld%idrtlen .ne. 5 .or. gfld%unpacked .neqv. .false. .or. &
       gfld%expanded .neqv. .true. .or. gfld%ibmap .ne. 0) stop 12

  do i=1,13
    if (gfld%idsect(i) .ne. expected_idsect(i)) stop 20
  enddo
  do i=1,15
    if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) stop 21
  enddo
  do i=1,19
    if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) stop 22
  enddo
  do i=1,5
    if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) stop 23
  enddo

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 100
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 101

  ! Free resources.
  call gf_free(gfld)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 102

  print *, 'SUCCESS!'
end program test_bitmap
