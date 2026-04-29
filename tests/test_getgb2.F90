! This is a test program in the NCEPLIBS-g2 project.
!
! This program tests the getgb2() subroutine.
!
! Brian Curtis 11/26/2021
! Ed Hartnett
program test_getgb2
  use grib_mod
  use bacio_module
  implicit none

  ! Define what I need
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
  integer :: idrstmpl(5) = (/ 0, 1, 1, 8, 0 /)
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .true.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret
  integer :: i
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)
#ifdef KIND_4  
  real(4) :: fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)
#else
  real(8) :: fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)
#endif
  real, parameter :: EPSILON = .2 ! mighty large epsilon is required!

  print *, 'Testing open/read/close of GRIB2 file created with creategrib.f90..'
  print *, 'testing getgb2()..'

  ! Write the test file.
  call write_grib2_file("test_getgb2.grib2")

  ! Open the test file for reading.
  call baopenr(1, "test_getgb2.grib2", iret)
  if (iret .ne. 0) stop 3

  ! Read a field from the test file.
  call getgb2i2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
       unpack, 2, k, gfld, iret)
  if (iret .ne. 0) stop 4

  ! Check results.
  if (gfld%version .ne. 2) stop 10
  if (gfld%discipline .ne. 0) stop 20
  do i = 1, 13
     if (gfld%idsect(i) .ne. jids(i)) stop 30
  end do
  if (gfld%idsectlen .ne. 13) stop 40
  do i = 1, lcsec2
     if (gfld%local(i) .ne. csec2(i)) stop 50
  end do
  if (gfld%locallen .ne. lcsec2) stop 60
  if (gfld%ifldnum .ne. 1) stop 70
  if (gfld%griddef .ne. 0) stop 80
  if (gfld%ngrdpts .ne. 4) stop 90
  if (gfld%numoct_opt .ne. 0) stop 91
  if (gfld%interp_opt .ne. 0) stop 92
  if (gfld%num_opt .ne. 0) stop 100
  if (gfld%igdtnum .ne. 0) stop 101
  if (gfld%igdtlen .ne. 19) stop 110
  do i = 1, 19
     if (gfld%igdtmpl(i) .ne. jgdt(i)) stop 120
  end do
  if (gfld%ipdtnum .ne. 0) stop 130
  if (gfld%ipdtlen .ne. 15) stop 140
  do i = 1, 15
     if (gfld%ipdtmpl(i) .ne. jpdt(i)) stop 150
  end do
  if (gfld%num_coord .ne. 0) stop 160
  if (gfld%ndpts .ne. 4) stop 170
  if (gfld%idrtnum .ne. 0) stop 180
  if (gfld%idrtlen .ne. 5) stop 190
  ! The first value of the DRS template gets changed to an IEEE
  ! floating point reference value when the data are written. So the
  ! first value of gfld%idrtmpl will not match.
  if (gfld%idrtmpl(1) .ne. 1093664768) stop 191
  do i = 2, 5
     !    print *, gfld%idrtmpl(i), idrstmpl(i)
     if (gfld%idrtmpl(i) .ne. idrstmpl(i)) stop 200
  end do
  if (gfld%unpacked .neqv. .false.) stop 201
  if (gfld%ibmap .ne. 255) stop 203
  !  print *, gfld%bmap()
  print *, 'fld: ',fld
  print *, 'gfld%fld ', gfld%fld
  do i = 1, 4
     print *, 'gfld%fld(i), fld(i), abs(gfld%fld(i) - fld(i))'
     print *, gfld%fld(i), fld(i), abs(gfld%fld(i) - fld(i))
     if (abs(gfld%fld(i) - fld(i)) .gt. EPSILON) stop 205
  end do

  ! Close file.
  call baclose(1, iret)
  if (iret .ne. 0) stop 5

  print *, 'OK!'
  print *, 'testing putgb2...'

  ! Open file for writing.
  call baopenw(2, "test_getgb2_copy.grib2", iret)
  if (iret .ne. 0) stop 100

  if (.not. associated(gfld%bmap)) allocate(gfld%bmap(1))
  call putgb2(2, gfld, iret)
  if (iret .ne. 0) stop 107

  ! Close file.
  call baclose(2, iret)
  if (iret .ne. 0) stop 150

  ! Free the memory.
  call gf_free(gfld)

  ! Trigger malformed-index len6 path for valgrind regression checks.
  call run_corrupt_len6_prev_bitmap_extract("test_getgb2.grib2", iret)
  if (iret .ne. 0) stop 530

  call gf_finalize(iret)
  if (iret .ne. 0) stop 5

  ! Call finalize again, should do nothing.
  call gf_finalize(iret)
  if (iret .ne. 0) stop 5

  print *, 'OK!'
  print *, 'SUCCESS!'

contains

  subroutine run_corrupt_len6_prev_bitmap_extract(filename, iret)
    use bacio_module
    implicit none

    character(*), intent(in) :: filename
    integer, intent(out) :: iret

    integer, parameter :: INT1_BITS = 8
    integer, parameter :: INT4_BITS = 32
    integer, parameter :: lugb_reg = 7
    integer :: lugi_reg
    integer :: idxver
    integer :: nlen, nnum
    integer :: mypos, sec6_pos
    integer :: len1, len3, len4, len5, len6
    integer :: iret_extract
    logical :: extract
    integer(kind = 8) :: leng8
    character(len = 1), pointer, dimension(:) :: cindex
    character(len = 1), pointer, dimension(:) :: gribm

    interface
       subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
         integer, intent(in) :: lugb, lugi
         integer, intent(inout) :: idxver
         character(len = 1), pointer, dimension(:) :: cindex
         integer, intent(out) :: nlen, nnum, iret
       end subroutine getidx2
       subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng8, iret)
         integer, intent(in) :: lugb, idxver
         character(len = 1), intent(in) :: cindex(*)
         logical, intent(in) :: extract
         character(len = 1), pointer, dimension(:) :: gribm
         integer(kind = 8), intent(out) :: leng8
         integer, intent(out) :: iret
       end subroutine getgb2rp2
       subroutine g2_gbytec1(input, siout, iskip, nbits)
         character*1, intent(in) :: input(*)
         integer, intent(inout) :: siout
         integer, intent(in) :: iskip, nbits
       end subroutine g2_gbytec1
       subroutine g2_sbytec1(out, input, iskip, nbits)
         character*1, intent(inout) :: out(*)
         integer, intent(in) :: input
         integer, intent(in) :: iskip, nbits
       end subroutine g2_sbytec1
    end interface

    iret = 0
    lugi_reg = 0
    idxver = 1
    extract = .true.
    nullify(cindex)
    nullify(gribm)

    call baopenr(lugb_reg, filename, iret)
    if (iret .ne. 0) return

    call getidx2(lugb_reg, lugi_reg, idxver, cindex, nlen, nnum, iret)
    if (iret .ne. 0) then
       call baclose(lugb_reg, iret_extract)
       return
    endif

    ! Parse idxver=1 record up to section 6 and corrupt its indexed length.
    mypos = INT4_BITS
    mypos = mypos + INT4_BITS + INT4_BITS
    mypos = mypos + 32 * INT1_BITS

    call g2_gbytec1(cindex, len1, mypos, INT4_BITS)
    mypos = mypos + len1 * INT1_BITS
    call g2_gbytec1(cindex, len3, mypos, INT4_BITS)
    mypos = mypos + len3 * INT1_BITS
    call g2_gbytec1(cindex, len4, mypos, INT4_BITS)
    mypos = mypos + len4 * INT1_BITS
    call g2_gbytec1(cindex, len5, mypos, INT4_BITS)
    mypos = mypos + len5 * INT1_BITS
    call g2_gbytec1(cindex, len6, mypos, INT4_BITS)
    sec6_pos = mypos

    ! Corrupt the len6 field to a huge value. With prior versions and a large 
    ! bitmap, this will cause reads past buffer bounds when sec6 data is accessed.
    ! The bitmap indicator (byte after length) determines if there's a bitmap.
    ! Set it to indicate a bitmap exists (e.g., 254), then corrupt len6 to cause
    ! out-of-bounds access when the bitmap is read.
    call g2_sbytec1(cindex, 999999999, sec6_pos, INT4_BITS)
    ! Set bitmap indicator to 254 (bitmap present) - this causes code to try
    ! to read the (nonexistent) large bitmap based on corrupted len6
    call g2_sbytec1(cindex, 254, sec6_pos + 5 * INT1_BITS, INT1_BITS)

    ! Attempt extraction with corrupted index
    call getgb2rp2(lugb_reg, idxver, cindex, extract, gribm, leng8, iret_extract)

    if (associated(gribm)) deallocate(gribm)
    ! cindex is owned by getidx2() internal cache and freed by gf_finalize().
    nullify(cindex)
    call baclose(lugb_reg, iret_extract)

    ! No strict checks here; this is a valgrind-focused memory regression path.
    iret = 0
  end subroutine run_corrupt_len6_prev_bitmap_extract

end program test_getgb2

