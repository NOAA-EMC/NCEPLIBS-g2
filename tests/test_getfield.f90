! This program tests the getfield() subroutine of the NCEPLIBS-g2
! project. Link this to the _4 build of the library.
!
! Ed Hartnett 11/16/21
program test_getfield
  implicit none

  ! Storage for the grib2 message we are reading.
  integer, parameter :: lcgrib = 191
  
  ! This is a copy of the GRIB2 message generated in
  ! test_gribcreate.f90.
  character :: cgrib(lcgrib) = (/  achar( 71), achar( 82),&
       & achar( 73), achar( 66), achar(  0), achar(  0), achar(  0),&
       & achar(  2), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(191), achar(  0),&
       & achar(  0), achar(  0), achar( 21), achar(  1), achar(  0),&
       & achar(  7), achar(  0), achar(  4), achar(  2), achar( 24),&
       & achar(  0), achar(  7), achar(229), achar( 11), achar( 13),&
       & achar( 15), achar( 59), achar( 59), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(  8), achar(  2),&
       & achar(  1), achar(  2), achar(  3), achar(  0), achar(  0),&
       & achar(  0), achar( 72), achar(  3), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  4), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar(  1), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar(  1), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar(  1), achar(  0), achar(  0),&
       & achar(  0), achar(  2), achar(  0), achar(  0), achar(  0),&
       & achar(  2), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar( 45), achar(  0), achar(  0),&
       & achar(  0), achar( 91), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar( 55), achar(  0), achar(  0), achar(  0),&
       & achar(101), achar(  0), achar(  0), achar(  0), achar(  5),&
       & achar(  0), achar(  0), achar(  0), achar(  5), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar( 34), achar(  4),&
       & achar(  0), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar( 12), achar( 59), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  1), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar(  1), achar(  2), achar(  1),&
       & achar(  0), achar(  0), achar(  0), achar(  1), achar(  0),&
       & achar(  0), achar(  0), achar( 21), achar(  5), achar(  0),&
       & achar(  0), achar(  0), achar(  4), achar(  0), achar(  0),&
       & achar( 65), achar( 48), achar(  0), achar(  0), achar(  0),&
       & achar(  1), achar(  0), achar(  1), achar(  8), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(  6), achar(  6),&
       & achar(255), achar(  0), achar(  0), achar(  0), achar(  9),&
       & achar(  7), achar(  0), achar(  1), achar(  1), achar(  2),&
       & achar( 55), achar( 55), achar( 55), achar( 55) /)
  
  ! Section 0 and 1.
  integer :: listsec0(2), listsec1(13)

  ! Section 2.
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

  ! Section 3.
  integer, parameter :: expected_len_sec3 = 72
  integer, parameter :: igdstmplen = 19
  integer :: idefnum
  integer :: ndpts
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml
  integer :: igds(5)
!  integer :: igds(5) = (/ 0, ndpts, 0, 0, 0/)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp3-0.shtml
  integer :: igdstmpl(igdstmplen)
!  integer :: igdstmpl(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  integer :: ideflist(1)

  ! Sections 4-7.
  integer :: ipdsnum
  integer :: ipdstmplen = 15, numcoord = 0
  integer :: ipdstmpl(15)
  integer :: coordlist(1)
  integer :: idrsnum = 0
  integer, parameter :: idrstmplen = 5
  integer :: idrstmpl(idrstmplen)
  integer :: ngrdpts = 4, ibmap = 255
  logical :: bmap(1)
  real :: fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)

  ! Section 8
  integer :: lengrib

  integer :: idrslen, igdslen, ipdslen

  character :: old_val
  integer :: i, ierr

  print *, 'Testing gribcreate().'
  !   expected_cpack = (/ char(0), char(93), char(108), char(64) /)

  print *, 'Testing simple call to gribcreate(). Expect and ignore error messages.'

  ! ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml.
  ! listsec0(1) = 0
  ! listsec0(2) = 2

  ! ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  ! listsec1(1) = 7 ! US National Weather Service - NCEP (WMC)
  ! listsec1(2) = 4 ! Environmental Modeling Center
  ! listsec1(3) = 2 ! GRIB master tables version number (currently 2)
  ! listsec1(4) = 24 ! Version Implemented on 06 November 2019
  ! listsec1(5) = 0 ! Local tables not used. 
  ! listsec1(6) = 2021 ! Year
  ! listsec1(7) = 11 ! Month
  ! listsec1(8) = 13 ! Day
  ! listsec1(9) = 15 ! Hour
  ! listsec1(10) = 59 ! Minute
  ! listsec1(11) = 59 ! Second
  ! listsec1(12) = 1 !  Operational Test Products
  ! listsec1(13) = 0 ! Analysis Products

  ! ! Product definition template 0.
  ! ipdsnum = 0
  
  ! ! Set up product definition section template.
  ! ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp4-0.shtml
  ! ipdstmpl(1) = 0
  ! ipdstmpl(2) = 0
  ! ipdstmpl(3) = 0
  ! ipdstmpl(4) = 0
  ! ipdstmpl(5) = 0
  ! ipdstmpl(6) = 12
  ! ipdstmpl(7) = 59
  ! ipdstmpl(8) = 0
  ! ipdstmpl(9) = 0
  ! ipdstmpl(10) = 1
  ! ipdstmpl(11) = 1
  ! ipdstmpl(12) = 1
  ! ipdstmpl(13) = 2
  ! ipdstmpl(14) = 1
  ! ipdstmpl(15) = 1

  ! ! Set up data representation section template.
  ! ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-0.shtml
  ! idrstmpl(1) = 0
  ! idrstmpl(2) = 1
  ! idrstmpl(3) = 1
  ! idrstmpl(4) = 8
  ! idrstmpl(5) = 0

  ! Request an invalid field - won't work.
  call getfield(cgrib, lcgrib, 0, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 3) stop 10

  ! Change first char of message and try to get a field - won't work.
  old_val = cgrib(1)
  cgrib(1) = achar(0)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 1) stop 20
  cgrib(1) = old_val

  ! Change grib version number and try to get a field - won't work.
  old_val = cgrib(8)
  cgrib(8) = achar(0)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 2) stop 30
  cgrib(8) = old_val

  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 0) stop 10

  print *, 'SUCCESS!'

end program test_getfield
