! This program tests the addfield subroutine of the NCEPLIBS-g2
! project.
!
! Andrew King 8/1/23
program test_addfield
  implicit none

  ! Using GRIB message data from util.F90

  ! Storage for the GRIB2 message we are constructing.
  integer, parameter :: lcgrib = 350
  character, dimension(lcgrib) :: cgrib, s2grib, s3grib, s7grib

  ! Section 0 and 1.
  integer :: listsec0(2) = (/ 0, 2 /)
  integer :: listsec1(13) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)

  ! Section 2.
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

  ! Section 3.
  integer, parameter :: igdstmplen = 19
  integer, parameter :: idefnum = 0
  integer, parameter :: ndata = 4
  integer :: igds(5) = (/ 0, ndata, 0, 0, 0/)
  integer :: igdstmpl(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  integer :: ideflist(idefnum)

  ! Sections 4-7.
  integer :: ipdsnum = 0
  integer, parameter :: ipdstmplen = 15, numcoord = 0
  integer :: ipdstmpl(ipdstmplen) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer :: coordlist(1)
  integer :: idrsnum = 0, idrstmplen = 5
  integer, parameter :: ngrdpts = 4
  integer :: idrstmpl(18)
  integer :: ibmap = 255
  logical :: bmap(1) = .false.
  real :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)

  ! Section 8
  integer :: ierr, lengrib

  idrstmpl = 0
  idrstmpl(2) = 1
  idrstmpl(3) = 1
  idrstmpl(4) = 8
  

  print *, 'Constructing GRIB message...'
  ! Create the GRIB2 message, with sections 0 and 1.
  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 100

  ! Add a local section.
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 0) stop 200
  s2grib = cgrib

  ! Add a grid section.
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 0) stop 300
  s3grib = cgrib

  print *, 'Testing addfield, expect and ignore error messages...'
  print *, 'Normal addfield call, error=0'
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 1
  s7grib = cgrib
  cgrib = s3grib

  print *, 'No beggining GRIB, error=1'
  cgrib(1) = char(0)
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 1) stop 2
  cgrib = s3grib
  
  print *, 'Message already complete, error=2'
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 110
  call gribend(cgrib, lcgrib, lengrib, ierr)
  if (ierr .ne. 0) stop 120
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 2) stop 3
  cgrib = s3grib
  
  print *, 'Byte count doesnt match total length, error=3'
  cgrib(46) = char(48) ! Mess up section 3 length
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 3) stop 4
  cgrib = s3grib
  
  print *, 'Last section not 3 or 7, error=4'
  cgrib = s2grib
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 4) stop 5
  
  print *, 'Section 3 not previously defined, error=6'
  cgrib = s2grib
  cgrib(42) = char(7) ! Replace section 2 section number with 7
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 6) stop 6
  cgrib = s3grib

  print *, 'Data Representation Template not yet implemented, error=5'
  idrsnum = 5
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 5) stop 7
  cgrib = s3grib

  print *, 'Testing normal addfield call with Spherical Harmonic Simple Packing, error=0'
  idrsnum = 50
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
      ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 1
  cgrib = s3grib

  print *, 'Testing normal addfield call with PNG encoding, error=0'
  idrsnum = 41
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
      ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 1
  cgrib = s3grib

  print *, 'Testing normal addfield call with JPEG encoding, error=0'
  idrstmplen = 7
  idrsnum = 40
  idrstmpl(4) = 32
  idrstmpl(7) = 1
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
      ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 1
  cgrib = s3grib

  print *, 'Testing normal addfield call with Complex Packing, error=0'
  idrstmplen = 16
  idrsnum = 2
  idrstmpl(7) = 0
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
      ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 1
  cgrib = s3grib
  
  print *, 'Spherical Harmonic Complex Packing where J, K, and M  & 
      pentagonal resolution parameters = 0, error=9'
  idrstmplen = 10
  idrsnum = 51
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
      ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 9) stop 9
  cgrib = s3grib

  if (.false.) then
    print *, 'Testing normal addfield call with Spherical Harmonic Complex Packing, error=0'
    call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
        coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
        ngrdpts, ibmap, bmap, ierr)
    if (ierr .ne. 0) stop 1
    cgrib = s3grib
  endif
  
  print *, 'SUCCESS!'

end program test_addfield
