! This is a test for the NCEPLIBS-g2 library.
!
! In this test we try out the encode() subroutine.
!
! Ed Hartnett 9/29/21
program test_g2_encode
  implicit none

  ! For gribcreate().
  integer, parameter :: MAX_MSG_LEN = 256
  character (len = MAX_MSG_LEN) :: msg
  integer :: listsec0(2)
  integer :: listsec1(13)
  integer :: msg_len

  ! For addgrid().
  integer :: idgs(5)
  integer :: idgstmpl(5)
  integer :: igdstmplen
  integer :: ideflist(5)
  integer :: idefnum

  ! For addfield().
  integer :: ipdsnum
  integer :: ipdstmpl(3)
  integer :: ipdstmplen
  integer, parameter :: numcoord = 3
  real :: coordlist(numcoord)
  integer :: idrsnum
  integer :: idrstmpl(3)
  integer :: idrstmplen
  integer, parameter :: ngrdpts = 3
  real :: fld(ngrdpts)
  integer :: ibmap
  logical*1 :: bmap(1)

  integer :: ierr

  print *, 'Testing g2 library encoding.'

!  listsec0(1) Discipline-GRIB Master Table Number (Code Table 0.0)
!  listsec0(2) GRIB Edition Number (currently 2)
  listsec0(1) = 0
  listsec0(2) = 2

! listsec1(1) Id of orginating centre (Common Code Table C-1)
! listsec1(2) Id of orginating sub-centre (local table)
! listsec1(3) GRIB Master Tables Version Number (Code Table 1.0)
! listsec1(4) GRIB Local Tables Version Number (Code Table 1.1)
! listsec1(5) Significance of Reference Time (Code Table 1.2)
! listsec1(6) Reference Time - Year (4 digits)
! listsec1(7) Reference Time - Month
! listsec1(8) Reference Time - Day
! listsec1(9) Reference Time - Hour
! listsec1(10) Reference Time - Minute
! listsec1(11) Reference Time - Second
! listsec1(12) Production status of data (Code Table 1.3)
! listsec1(13) Type of processed data (Code Table 1.4)
  listsec1(1) = 0
  listsec1(2) = 0
  listsec1(3) = 0
  listsec1(4) = 0
  listsec1(5) = 0
  listsec1(6) = 2021
  listsec1(7) = 1
  listsec1(8) = 31
  listsec1(9) = 12
  listsec1(10) = 59
  listsec1(11) = 59
  listsec1(12) = 0
  listsec1(13) = 0

  ! Create the GRIB2 message.
  call gribcreate(msg, MAX_MSG_LEN, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 2

  ! igds(1)=Source of grid definition (see Code Table 3.0)
  ! igds(2)=Number of grid points in the defined grid.
  ! igds(3)=Number of octets needed for each additional grid points
  ! definition.Used to define number of points in each row (or column)
  ! for non-regular grids. = 0, if using regular grid.
  ! igds(4)=Interpretation of list for optional points definition. (Code Table 3.11)
  ! igds(5)=Grid Definition Template Number (Code Table 3.1)
  idgs(1) = 0
  idgs(2) = 0
  idgs(3) = 0
  idgs(4) = 0
  idgs(5) = 0

  ! Contains the data values for the specified Grid Definition
  ! Template ( NN=igds(5) ). Each element of this integer array
  ! contains an entry (in the order specified) of Grid Defintion
  ! Template 3.NN.
  idgstmpl(1) = 0
  idgstmpl(2) = 0
  idgstmpl(3) = 0
  idgstmpl(4) = 0
  idgstmpl(5) = 0

  ! Length of idgstmpl.
  igdstmplen = 5

  ! What's this?
  idefnum = 0
  
  ! Add a grid to the GRIB2 message.
  call addgrid(msg, MAX_MSG_LEN, idgs, idgstmpl, igdstmplen, ideflist, idefnum, ierr)
  if (ierr .ne. 0) then
     print *, 'ierr = ', ierr
     stop 2
  endif

  ! Product Definition Template Number ( see Code Table 4.0).
  ipdsnum = 8

  ! Contains the data values for the specified Product Definition
  ! Template (N=ipdsnum). Each element of this integer array contains
  ! an entry (in the order specified) of Product Defintion Template
  ! 4.N.
  ipdstmpl(1) = 42
  ipdstmpl(2) = 42
  ipdstmpl(3) = 42

  ! Max dimension of ipdstmpl coordlist Array containg floating point
  ! values intended to document the vertical discretisation associated
  ! to model data on hybrid coordinate vertical levels.
  ipdstmplen = 3

  ! Array containg floating point values intended to document the
  ! vertical discretisation associated to model data on hybrid
  ! coordinate vertical levels. (part of Section 4) The dimension of
  ! this array can be obtained in advance from maxvals(5), which is
  ! returned from subroutine gribinfo.
  coordlist(1) = 1.0
  coordlist(2) = 2.0
  coordlist(3) = 3.0

  ! idrsnum - Data Representation Template Number (see Code Table 5.0)

  ! idrstmpl Contains the data values for the specified Data
  ! Representation Template (N=idrsnum). Each element of this integer
  ! array contains an entry (in the order specified) of Data
  ! Representation Template 5.N. Note that some values in this
  ! template (eg. reference values, number of bits, etc...) may be
  ! changed by the data packing algorithms. Use this to specify
  ! scaling factors and order of spatial differencing, if desired.
  
  ! Max dimension of idrstmpl.
  idrstmplen = 3

  idrstmpl(1) = 0
  idrstmpl(2) = 0
  idrstmpl(3) = 0

  ! fld Array of data points to pack.

  ! ngrdpts Number of data points in grid. i.e. size of fld and bmap.

  ! ibmap Bitmap indicator (see Code Table 6.0).
  ibmap = 254

  ! bmap Logical*1 array containing bitmap to be added. (if ibmap=0 or
  ! ibmap=254)
  
  ! ! Add a field to the GRIB2 message.
  ! call addfield(msg, MAX_MSG_LEN, ipdsnum, ipdstmpl, ipdstmplen, &
  !      coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
  !      ngrdpts, ibmap, bmap, ierr)
  ! if (ierr .ne. 0) then
  !    print *, 'ierr = ', ierr
  !    stop 3
  ! endif

  ! Finilize the GRIB2 message.
  call gribend(msg, MAX_MSG_LEN, msg_len, ierr)
  if (ierr .ne. 0) stop 4
  print *, 'msg_len = ', msg_len
  
  print *, 'SUCESSS!'
end program test_g2_encode
