! This is part of the test code for the NCEPLIBS-g2 project.
!
! This module provides subroutines that create a cgrib variable for other tests
! or writes a grib2 file for use in other tests.
!
! Brian Curtis 11/12/21 
! Ed Hartnett
module creategrib
  implicit none
contains

  ! Create a GIRB2 message.
  !
  ! Brian Curtis
  subroutine create_cgrib(cgrib, lengrib)
    implicit none

    ! Storage for the GRIB2 message we are constructing.
    integer, parameter :: lcgrib = 191
    character, intent(out), dimension(lcgrib) :: cgrib

    ! Section 0 and 1.
    integer :: listsec0(2), listsec1(13)

    ! Section 2.
    integer, parameter :: lcsec2 = 3
    character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

    ! Section 3.
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
    logical :: bmap(1) = .false.
    real(8) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)

    ! Section 8
    integer, intent(out) :: lengrib
    integer :: ierr

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

    ! Product definition template 0.
    ipdsnum = 0

    ! Set up product definition section template.
    ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp4-0.shtml
    ipdstmpl(1) = 0
    ipdstmpl(2) = 0
    ipdstmpl(3) = 0
    ipdstmpl(4) = 0
    ipdstmpl(5) = 0
    ipdstmpl(6) = 12
    ipdstmpl(7) = 59
    ipdstmpl(8) = 0
    ipdstmpl(9) = 0
    ipdstmpl(10) = 1
    ipdstmpl(11) = 1
    ipdstmpl(12) = 1
    ipdstmpl(13) = 2
    ipdstmpl(14) = 1
    ipdstmpl(15) = 1

    ! Set up data representation section template.
    ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-0.shtml
    idrstmpl(1) = 0
    idrstmpl(2) = 1
    idrstmpl(3) = 1
    idrstmpl(4) = 8
    idrstmpl(5) = 0

    ! Create the GRIB2 message, with sections 0 and 1.
    call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
    if (ierr .ne. 0) stop 20

    ! Add a local section.
    call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
    if (ierr .ne. 0) stop 40

    ! Add a grid section.
    call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
         ideflist, idefnum, ierr)
    if (ierr .ne. 0) stop 80

    ! Add a field.
    call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
         & coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
         & ngrdpts, ibmap, bmap, ierr)
    if (ierr .ne. 0) stop 140

    ! End the grib message by adding section 8.w
    call gribend(cgrib, lcgrib, lengrib, ierr)
    if (ierr .ne. 0) stop 150

    return
  end subroutine create_cgrib

  ! Write a GRIB2 message to a file.
  !
  ! Brian Curtis
  subroutine write_grib2_file()
    use bacio_module
    implicit none

    integer :: lengrib
    character :: cgrib(200)
    integer :: ierr

    ! Create a GRIB2 message.
    call create_cgrib(cgrib, lengrib)

    ! Open the file for writing.
    call baopenw(1, "testgrib.grb2", ierr)

    ! Write the GRIB2 message.
    call wryte(1, lengrib, cgrib)

    ! Close the file.
    call baclose(1, ierr)

  end subroutine write_grib2_file
end module creategrib
