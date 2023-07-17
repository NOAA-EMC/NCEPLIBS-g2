! This program tests the gb_info() subroutine of the NCEPLIBS-g2
! project. 
!
! Ed Hartnett 7/17/23
program test_gb_info
  implicit none

  ! Length of our message.
  integer, parameter :: lcgrib = 191
  integer :: numfields, maxlocal, numlocal
  
  ! Section 0 and 1.
  integer :: listsec0(3), listsec1(13)

  ! Section 2.
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

  ! Section 3.
  integer, parameter :: expected_len_sec3 = 72
  integer, parameter :: igdstmplen = 19
  integer, parameter :: idefnum = 0
  integer, parameter :: ndata = 4
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml
  integer :: igds(5) = (/ 0, ndata, 0, 0, 0/)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp3-0.shtml
  integer :: igdstmpl(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  integer :: ideflist(idefnum)

  ! Sections 4-7.
  integer :: ipdsnum
  integer, parameter :: ipdstmplen = 15, numcoord = 0
  integer :: ipdstmpl(ipdstmplen)
  integer :: coordlist(1)
  integer :: idrsnum = 0
  integer, parameter :: idrstmplen = 5
  integer :: idrstmpl(idrstmplen)
  integer, parameter :: ngrdpts = 4, ibmap = 255
  logical :: bmap(1)
  real :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)

  ! Section 8
  integer :: lengrib

  ! This is a GRIB2 message.
  character :: cgrib(lcgrib) = (/  char( 71), char( 82),&
       & char( 73), char( 66), char(  0), char(  0), char(  0),&
       & char(  2), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(191), char(  0),&
       & char(  0), char(  0), char( 21), char(  1), char(  0),&
       & char(  7), char(  0), char(  4), char(  2), char( 24),&
       & char(  0), char(  7), char(229), char( 11), char( 13),&
       & char( 15), char( 59), char( 59), char(  1), char(  0),&
       & char(  0), char(  0), char(  0), char(  8), char(  2),&
       & char(  1), char(  2), char(  3), char(  0), char(  0),&
       & char(  0), char( 72), char(  3), char(  0), char(  0),&
       & char(  0), char(  0), char(  4), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  0), char(  0),&
       & char(  0), char(  2), char(  0), char(  0), char(  0),&
       & char(  2), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char( 45), char(  0), char(  0),&
       & char(  0), char( 91), char(  0), char(  0), char(  0),&
       & char(  0), char( 55), char(  0), char(  0), char(  0),&
       & char(101), char(  0), char(  0), char(  0), char(  5),&
       & char(  0), char(  0), char(  0), char(  5), char(  0),&
       & char(  0), char(  0), char(  0), char( 34), char(  4),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char( 12), char( 59), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  2), char(  1),&
       & char(  0), char(  0), char(  0), char(  1), char(  0),&
       & char(  0), char(  0), char( 21), char(  5), char(  0),&
       & char(  0), char(  0), char(  4), char(  0), char(  0),&
       & char( 65), char( 48), char(  0), char(  0), char(  0),&
       & char(  1), char(  0), char(  1), char(  8), char(  0),&
       & char(  0), char(  0), char(  0), char(  6), char(  6),&
       & char(255), char(  0), char(  0), char(  0), char(  9),&
       & char(  7), char(  0), char(  1), char(  1), char(  2),&
       & char( 55), char( 55), char( 55), char( 55) /)
  
  character :: old_val, ov2, ov3, ov4
  integer :: i, ierr

  print *, 'Testing gb_info(), explect and ignore error messages.'

  ! Change the first byte of the message, then try gb_info() - will
  ! not work.
  old_val = cgrib(1)
  cgrib(1) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 1) stop 1
  cgrib(1) = old_val  

  ! Change the GRIB version number, then try gb_info() - will
  ! not work.
  old_val = cgrib(8)
  cgrib(8) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 2) stop 2
  cgrib(8) = old_val  

  ! Change number of section 1, then try gb_info() - will
  ! not work.
  old_val = cgrib(21)
  cgrib(21) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 3) stop 3
  cgrib(21) = old_val  

  ! End message too soon, then try gb_info() - will
  ! not work.
  old_val = cgrib(38)
  ov2 = cgrib(39)
  ov3 = cgrib(40)
  ov4 = cgrib(41)
  cgrib(38) = char(55)
  cgrib(39) = char(55)
  cgrib(40) = char(55)
  cgrib(41) = char(55)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 4) stop 4
  cgrib(38) = old_val  
  cgrib(39) = ov2
  cgrib(40) = ov3
  cgrib(41) = ov4

  ! Change end of message, then try gb_info() - will
  ! not work.
  old_val = cgrib(190)
  cgrib(190) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 5) stop 5
  cgrib(190) = old_val  

  ! Change end of message, then try gb_info() - will
  ! not work.
  old_val = cgrib(42)
  cgrib(42) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 6) stop 6
  cgrib(42) = old_val  

  ! This will work.
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 0) stop 10
  if (numfields .ne. 1 .or. numlocal .ne. 1 .or. maxlocal .ne. 3) stop 11

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_gb_info
