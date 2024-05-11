! This is part of the test code for the NCEPLIBS-g2 project.
!
! This file provides subroutines that create a cgrib variable for other tests
! or writes a grib2 file for use in other tests.
!
! Brian Curtis 11/12/21
! Ed Hartnett

! Create a GIRB2 message.
!
! Brian Curtis
subroutine create_cgrib(cgrib, lengrib)
  implicit none

  ! Storage for the GRIB2 message we are constructing.
  integer, parameter :: lcgrib = 191
  character, intent(out), dimension(lcgrib) :: cgrib

  ! Section 0 and 1.
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml.
  integer :: listsec0(2) = (/ 0, 2 /)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  integer :: listsec1(13) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)

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
  integer :: ipdsnum = 0
  integer, parameter :: ipdstmplen = 15, numcoord = 0
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp4-0.shtml
  integer :: ipdstmpl(ipdstmplen) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer :: coordlist(1)
  integer :: idrsnum = 0
  integer, parameter :: idrstmplen = 5
  integer :: idrstmpl(idrstmplen) = (/ 0, 1, 1, 8, 0 /)
  integer, parameter :: ngrdpts = 4, ibmap = 255
  logical :: bmap(1) = .false.
#ifdef KIND_4  
  real(4) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)
#else
  real(8) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)
#endif

  ! Section 8
  integer, intent(out) :: lengrib
  integer :: ierr

  ! Create the GRIB2 message, with sections 0 and 1.
  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 20
  print *, 'fld(1) ', fld(1)

  ! Add a local section.
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 0) stop 40
  print *, 'fld(1) ', fld(1)

  ! Add a grid section.
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 0) stop 80
  print *, 'fld(1) ', fld(1)

  ! Add a field.
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       & coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       & ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 140
  print *, 'fld(1) ', fld(1)

  ! End the grib message by adding section 8.w
  call gribend(cgrib, lcgrib, lengrib, ierr)
  if (ierr .ne. 0) stop 150

  return
end subroutine create_cgrib

! Write a GRIB2 message to a file.
!
! Brian Curtis
subroutine write_grib2_file(filename)
  use bacio_module
  implicit none

  character(*), intent(in) :: filename
  integer :: lengrib
  character :: cgrib(200)
  integer :: ierr

  ! Create a GRIB2 message.
  call create_cgrib(cgrib, lengrib)

  ! Open the file for writing.
  call baopenw(1, filename, ierr)

  ! Write the GRIB2 message.
  call wryte(1, lengrib, cgrib)

  ! Close the file.
  call baclose(1, ierr)

end subroutine write_grib2_file
