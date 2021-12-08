!Test for the getdim source file
!Brian Curtis 2021-12-08
program test_getdim
  implicit none

  ! Storage for the grib2 message we are constructing.
  integer, parameter :: lcgrib = 191
  character, dimension(lcgrib) :: cgrib

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
  integer :: igdstmpl(igdstmplen)
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
  real(8) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)

  ! Section 8
  integer :: lengrib
  integer :: ierr

  ! getdim
  integer :: width, height, iscan

  igdstmpl = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  listsec0 = (/ 0, 2 /)
  listsec1 = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)
  ipdsnum = 0
  ipdstmpl = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  idrstmpl = (/ 0, 1, 1, 8, 0 /)

  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)

  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)

  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)

  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       & coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       & ngrdpts, ibmap, bmap, ierr)

  call gribend(cgrib, lcgrib, lengrib, ierr)

  ! NEW STUFF
  call getdim(cgrib, lcgrib, width, height, iscan)

  print *, 'width: ', width !NOT Correct
  print *, 'height: ', height !NOT Correct
  print *, 'iscan: ', iscan !Correct

end program test_getdim
