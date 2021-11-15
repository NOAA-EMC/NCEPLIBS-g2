! This program tests the gribcreate() subroutines of the NCEPLIBS-g2
! project. Link this to the _4 build of the library.
!
! Ed Hartnett 11/12/21
program test_gribcreate
  implicit none

  ! Storage for the grib2 message we are constructing.
  integer, parameter :: lcgrib = 117
  character, dimension(lcgrib) :: cgrib
  
  ! Section 0 and 1.
  integer :: listsec0(2), listsec1(13)

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

  ! This is the GRIB2 message we expect to get.
  character :: expected_cgrib(lcgrib) = (/  achar( 71), achar( 82),&
       & achar( 73), achar( 66), achar(  0), achar(  0), achar(  0),&
       & achar(  2), achar(  0), achar(  0), achar(  0), achar(  0),&
       & achar(  0), achar(  0), achar(  0), achar(117), achar(  0),&
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
       & achar(  0), achar(  0), achar(  0), achar(  5), achar(  0) /)
  
  character :: old_val
  integer :: i, ierr

  print *, 'Testing gribcreate().'
  !   expected_cpack = (/ char(0), char(93), char(108), char(64) /)

  print *, 'Testing simple call to gribcreate(). Expect and ignore error messages.'

  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml.
  listsec0(1) = 0
  listsec0(2) = 2

  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  listsec1(1) = 7 ! US National Weather Service - NCEP (WMC)
  listsec1(2) = 4 ! Environmental Modeling Center
  listsec1(3) = 2 ! GRIB master tables version number (currently 2)
  listsec1(4) = 24 ! Version Implemented on 06 November 2019
  listsec1(5) = 0 ! Local tables not used. 
  listsec1(6) = 2021 ! Year
  listsec1(7) = 11 ! Month
  listsec1(8) = 13 ! Day
  listsec1(9) = 15 ! Hour
  listsec1(10) = 59 ! Minute
  listsec1(11) = 59 ! Second
  listsec1(12) = 1 !  Operational Test Products
  listsec1(13) = 0 ! Analysis Products

  ! Change the GRIB version and try to create message - will not work.
  listsec0(2) = 1
  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  if (ierr .ne. 1) stop 10
  listsec0(2) = 2

  ! Create the GRIB2 message, with sections 0 and 1.
  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 20

  ! Change the first byte of the message, then try to add local - will
  ! not work.
  old_val = cgrib(1)
  cgrib(1) = achar(0)
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 1) stop 30
  cgrib(1) = old_val

  ! Change the section count, then try to add local - will
  ! not work.
  old_val = cgrib(16)
  cgrib(16) = achar(10)
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 3) stop 30
  cgrib(16) = old_val

  ! Add a local section.
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 0) stop 40
    
  ! Try to add a local section again - will not work.
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 4) stop 41

  ! Change the first byte of the message, then try to add grid - will
  ! not work.
  old_val = cgrib(1)
  cgrib(1) = achar(0)
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 1) stop 50
  cgrib(1) = old_val

  ! Change the section count, then try to add grid - will
  ! not work.
  old_val = cgrib(16)
  cgrib(16) = achar(10)
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 3) stop 60
  cgrib(16) = old_val

  ! Try with a bad template number.
  igds(5) = 999
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 5) stop 70
  igds(5) = 0

  ! Add a grid section.
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 0) stop 80
  
  ! Check the results.
  do i = 1, lcgrib
!     write(*, fmt='(a6i3a3)', advance="no") 'achar(', ichar(cgrib(i)), '), '
     if (cgrib(i) .ne. expected_cgrib(i)) stop 100
  enddo

  print *, 'SUCCESS!'

end program test_gribcreate
