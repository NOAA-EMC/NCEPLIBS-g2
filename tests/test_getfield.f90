! This program tests the getfield() subroutine of the NCEPLIBS-g2
! project. Link this to the _4 build of the library.
!
! Ed Hartnett 11/16/21
program test_getfield
  use grib_mod
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
  integer :: igdstmplen = 19
  integer :: idefnum
  integer :: ndpts
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml
  integer :: igds(5)
  integer :: x_igds(5) = (/ 0, 4, 0, 0, 0/)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp3-0.shtml
  integer :: igdstmpl(19)
  integer :: x_igdstmpl(19) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  integer :: ideflist(1)

  ! Sections 4-7.
  integer :: ipdsnum
  integer :: ipdstmplen = 15, numcoord = 0
  integer :: ipdstmpl(15)
  integer :: x_ipdstmpl(15) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer :: coordlist(1)
  integer :: idrsnum = 0
  integer :: idrstmplen = 5
  integer :: idrstmpl(5)
  integer :: x_idrstmpl(5) = (/ 1093664768, 1, 1, 8, 0 /)
  integer :: ngrdpts = 4, ibmap = 255
  logical :: bmap(4)
  real :: fld(4)
  real :: x_fld(4) = (/ 1.1, 1.2, 1.3, 1.4 /)
  real, parameter :: EPSILON = .2

  ! Section 8
  integer :: lengrib

  ! For reading values.
  integer :: idrslen, igdslen, ipdslen
  type(gribfield) :: gfld

  ! For changing values for tests.
  character :: old_val
  character :: old_val_arr(4)
  
  integer :: i, ierr

  print *, 'Testing gribcreate().'
  !   expected_cpack = (/ char(0), char(93), char(108), char(64) /)

  print *, 'Testing getfield(). Expect and ignore error messages.'

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

  ! Request a field that's not present.
  call getfield(cgrib, lcgrib, 2, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 6) stop 40

  ! Put an early end in the message and call getfield - will not work.
  old_val_arr(1) = cgrib(17)
  old_val_arr(2) = cgrib(18)
  old_val_arr(3) = cgrib(19)
  old_val_arr(4) = cgrib(20)
  cgrib(17) = achar(55)
  cgrib(18) = achar(55)
  cgrib(19) = achar(55)
  cgrib(20) = achar(55)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 4) stop 45
  cgrib(17) = old_val_arr(1)
  cgrib(18) = old_val_arr(2)
  cgrib(19) = old_val_arr(3)
  cgrib(20) = old_val_arr(4)

  ! Mess up section 3 and try to get a field - won't work.
  old_val = cgrib(58)
  cgrib(58) = achar(99)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 10) stop 46
  cgrib(58) = old_val

  ! Mess up section 4 and try to get a field - won't work.
  old_val = cgrib(125)
  cgrib(125) = achar(99)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 11) stop 47
  cgrib(125) = old_val
  
  ! Mess up section 5 and try to get a field - won't work.
  old_val = cgrib(161)
  cgrib(161) = achar(99)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 12) stop 48
  cgrib(161) = old_val
  
  ! Mess up section 6 and try to get a field - won't work.
  old_val = cgrib(178)
  cgrib(178) = achar(99)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 13) stop 49
  cgrib(178) = old_val
  
  ! Mess up end of message and call getfield - will not work.
  old_val = cgrib(16)
  cgrib(16) = achar(0)
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 7) stop 45
  cgrib(16) = old_val

  ! Get the field.
  call getfield(cgrib, lcgrib, 1, igds, igdstmpl, igdslen,&
       & ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist,&
       & numcoord, ndpts, idrsnum, idrstmpl, idrslen, ibmap, bmap,&
       & fld, ierr)
  if (ierr .ne. 0) stop 50

  ! Check results.
  if (igdslen .ne. 19) stop 200
  do i = 1, 5
     if (igds(i) .ne. x_igds(i)) stop 205
  end do
  do i = 1, 19
     if (igdstmpl(i) .ne. x_igdstmpl(i)) stop 210
  end do
  if (idefnum .ne. 0) stop 220
  if (ipdsnum .ne. 0) stop 230
  if (ipdslen .ne. 15) stop 240
  do i = 1, 15
     if (ipdstmpl(i) .ne. x_ipdstmpl(i)) stop 250
  end do
  if (numcoord .ne. 0) stop 260
  if (ndpts .ne. 4) stop 270
  if (idrsnum .ne. 0) stop 280
  if (idrslen .ne. 5) stop 290
  if (ibmap .ne. 255) stop 300
  do i = 1, 4
!     print *, fld(i), abs(fld(i) - x_fld(i))
     if (abs(fld(i) - x_fld(i)) .ge. EPSILON) stop 310
  end do

  print *, 'Testing gf_getfld(). Expect and ignore error messages.'

  ! Now read the same field with gf_getfld().
  call gf_getfld(cgrib, lcgrib, 1, .true., .true., gfld, ierr)  

  ! Free resources.
  call gf_free(gfld)
  
  print *, 'SUCCESS!'

end program test_getfield
