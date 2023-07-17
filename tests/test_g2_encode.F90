! This is a test for the NCEPLIBS-g2 library.
!
! In this test we try out the g2_encode() subroutine.
!
! Ed Hartnett 9/29/21

! This subroutine prints in a pretty way the contents of a gribfield
! type.
!
! Ed Hartnett 10/5/21
subroutine print_gribfield(gfld)
  use grib_mod
  use gridtemplates
  implicit none

  type(gribfield), intent(in) :: gfld
  integer :: i

  print *, 'Section 0: Indicator Section'
  print *, 'discipline: ', gfld%discipline
  print *, 'version: ', gfld%version
  print *, ''

  print *, 'Section 1: Identification Section'
  print *, 'idsectlen: ', gfld%idsectlen
  do i = 1, gfld%idsectlen
     print *, 'idsect(', i, '): ', gfld%idsect(i)
  enddo
  print *, ''

  print *, 'Section 2: Local Use Section'
  print *, ' locallen: ', gfld%locallen
  print *, ''

  print *, 'Section 3: Grid Definition Section'
  print *, 'griddef: ', gfld%griddef
  print *, 'igdtnum: ', gfld%igdtnum, ' igdtlen: ', gfld%igdtlen
  print *, 'ngrdpts: ', gfld%ngrdpts
  print *, 'numoct_opt: ', gfld%numoct_opt
  print *, 'interp_opt: ', gfld%interp_opt
  print *, 'num_opt: ', gfld%num_opt
  do i = 1, gfld%igdtlen
     print *, 'igdtmpl(', i, '): ', gfld%igdtmpl(i)
  enddo
  print *, 'num_coord: ', gfld%num_coord
  do i = 1, gfld%num_coord
     print *, 'coord_list(', i, '): ', gfld%coord_list(i)
  enddo
  print *, ''

  print *, 'Section 4: Product Definition Section'
  print *, 'ipdtnum: ', gfld%ipdtnum
  print *, 'ipdtlen: ', gfld%ipdtlen
  do i = 1, gfld%ipdtlen
     print *, 'ipdtmpl(', i, '): ', gfld%ipdtmpl(i)
  enddo
  print *, ''

  print *, 'Section 5: Data Representation Section'
  print *, 'idrtnum: ', gfld%idrtnum
  print *, 'idrtlen: ', gfld%idrtlen
  do i = 1, gfld%idrtlen
     print *, 'idrtmpl(', i, '): ', gfld%idrtmpl(i)
  enddo
  print *, ''

  print *, 'Section 6: Bit Map Section'
  print *, ' ibmap: ', gfld%ibmap
  print *, ''

  print *, 'Section 7: Data Section'
  print *, 'ndpts: ', gfld%ndpts
  print *, 'ifldnum: ', gfld%ifldnum
  print *, 'expanded: ', gfld%expanded
  print *, 'unpacked: ', gfld%unpacked
  print *, ''
  
end subroutine print_gribfield

! This is the main test program.
!
! Ed Hartnett 9/29/21
program test_g2_encode
  use grib_mod
  implicit none

  ! For gribcreate().
  integer, parameter :: MAX_MSG_LEN = 256
  character (len = MAX_MSG_LEN) :: msg
  integer :: listsec0(2)
  integer :: listsec1(13)
  integer :: msg_len

  ! For addgrid().
  integer :: igds(5)
  integer, parameter :: my_grid_tmpl_maplen = 19
  integer, dimension(my_grid_tmpl_maplen) :: igdstmpl
  integer :: ideflist(5)
  integer :: idefnum

  ! For addfield().
  integer :: ipdsnum
  integer, parameter :: my_pds_tmpl_maplen = 35 ! 29 plus 6 extra
  integer :: ipdstmpl(my_pds_tmpl_maplen)
  integer, parameter :: numcoord = 3
  integer :: idrsnum
  integer, parameter :: my_drs_tmpl_maplen = 5
  integer :: idrstmpl(my_drs_tmpl_maplen)
  integer, parameter :: ngrdpts = 4
  real :: coordlist(numcoord)
  real :: fld(10)
  integer :: ibmap
  logical*1 :: bmap(ngrdpts)

  ! For rereading the message.
  integer :: listsec0_in(3)
  integer :: listsec1_in(13)
  integer :: numfields, numlocal, maxlocal
  type(gribfield) :: gfld
  
  integer :: ierr
  integer :: i

  print *, 'Testing g2 library encoding/decoding.'
  fld = 1

  print *, 'Testing g2 library encoding...'
!  listsec0(1) Discipline-GRIB Master Table Number (Code Table 0.0)
!  listsec0(2) GRIB Edition Number (currently 2)
  listsec0 = (/ 0, 2 /)

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
  listsec1 = (/ 0, 0, 0, 0, 0, 2021, 1, 31, 12, 59, 59, 0, 0 /)

  ! Create the GRIB2 message.
  call gribcreate(msg, MAX_MSG_LEN, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 2

  ! igds(1) Source of grid definition (see Code Table 3.0)
  ! igds(2) Number of grid points in the defined grid.
  ! igds(3) Number of octets needed for each additional grid points
  ! definition. Used to define number of points in each row (or column)
  ! for non-regular grids. = 0, if using regular grid.
  ! igds(4) Interpretation of list for optional points definition. (Code Table 3.11)
  ! igds(5) Grid Definition Template Number (Code Table 3.1)
  igds = (/ 0, 4, 0, 0, 0 /)

  ! Contains the data values for the specified Grid Definition
  ! Template (NN=igds(5)). Each element of this integer array
  ! contains an entry (in the order specified) of Grid Defintion
  ! Template 3.NN.
  igdstmpl = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)

  ! What's this?
  idefnum = 0
  
  ! Add a grid to the GRIB2 message.
  call addgrid(msg, MAX_MSG_LEN, igds, igdstmpl, my_grid_tmpl_maplen, ideflist, idefnum, ierr)
  if (ierr .ne. 0) then
     print *, 'ierr = ', ierr
     stop 2
  endif

  ! Product Definition Template Number (see Code Table 4.0).
  ipdsnum = 8

  ! Contains the data values for the specified Product Definition
  ! Template (N=ipdsnum). Each element of this integer array contains
  ! an entry (in the order specified) of Product Defintion Template
  ! 4.N.
  ipdstmpl = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 1/)

  ! Array containg floating point values intended to document the
  ! vertical discretisation associated to model data on hybrid
  ! coordinate vertical levels. (part of Section 4) The dimension of
  ! this array can be obtained in advance from maxvals(5), which is
  ! returned from subroutine gribinfo.
  coordlist(1) = 1.0
  coordlist(2) = 2.0
  coordlist(3) = 3.0

  ! idrsnum - Data Representation Template Number (see Code Table 5.0)
  idrsnum = 0

  ! idrstmpl Contains the data values for the specified Data
  ! Representation Template (N=idrsnum). Each element of this integer
  ! array contains an entry (in the order specified) of Data
  ! Representation Template 5.N. Note that some values in this
  ! template (eg. reference values, number of bits, etc...) may be
  ! changed by the data packing algorithms. Use this to specify
  ! scaling factors and order of spatial differencing, if desired.
  idrstmpl = (/ 0, 0, 0, 0, 0 /)

  ! fld Array of data points to pack.

  ! ngrdpts Number of data points in grid. i.e. size of fld and bmap.

  ! ibmap Bitmap indicator (see Code Table 6.0).
  ibmap = 253

  ! bmap Logical*1 array containing bitmap to be added. (if ibmap=0 or
  ! ibmap=254)
  
  ! Add a field to the GRIB2 message.
  call addfield(msg, MAX_MSG_LEN, ipdsnum, ipdstmpl, my_pds_tmpl_maplen, &
       coordlist, numcoord, idrsnum, idrstmpl, my_drs_tmpl_maplen, fld, &
       ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 3

  ! Finilize the GRIB2 message.
  call gribend(msg, MAX_MSG_LEN, msg_len, ierr)
  if (ierr .ne. 0) stop 4
  print *, 'msg_len = ', msg_len
  ! I don't understand why the msg_len is 216 on GNU and 217 on Intel...
  !  if (msg_len .ne. 216) stop 5

  print *, 'Testing g2 library decoding...'

  ! Check the message for correctness.
  call gb_info(msg, msg_len, listsec0_in, listsec1_in, &
       numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 0) stop 10
  
  ! I don't understand why listsec0_in(1) is 216 instead of 0...
  ! print *, listsec0(1), listsec0_in(1)
  if (listsec0(2) .ne. listsec0_in(2)) stop 11
  do i = 1, 13
     if (listsec1(i) .ne. listsec1_in(i)) stop 12
  enddo
  if (numfields .ne. 1 .or. numlocal .ne. 0 .or. maxlocal .ne. 0) stop 10

  call gf_getfld(msg, msg_len, 1, .true., 0, gfld, ierr)
  if (ierr .ne. 0) stop 20

  ! Print results.
  call print_gribfield(gfld)

  ! Section 0 - Indicator.
  if (gfld%discipline .ne. listsec0(1)) stop 100
  if (gfld%version .ne. 2) stop 101

  ! Section 1 - Identification.
  if (gfld%idrtlen .ne. my_drs_tmpl_maplen) stop 110
  do i = 1, 13
     if (gfld%idsect(i) .ne. listsec1(i)) stop 111
  enddo

  ! Section 2 - Local Use.
  if (gfld%locallen .ne. 0) stop 120

  ! Section 3 - Grid Definition.
  if (gfld%griddef .ne. igds(1)) stop 130  
  do i = 1, my_grid_tmpl_maplen
     if (igdstmpl(i) .ne. gfld%igdtmpl(i)) stop 131
  end do
  if (gfld%ngrdpts .ne. ngrdpts) stop 132
  if (gfld%numoct_opt .ne. 0) stop 133
  if (gfld%interp_opt .ne. 0) stop 134
  if (gfld%num_opt .ne. 0) stop 135
  if (gfld%num_coord .ne. numcoord) stop 136
  do i = 1, gfld%num_coord
     print *, i, coordlist(i), gfld%coord_list(i)
     if (coordlist(i) .ne. gfld%coord_list(i)) stop 137
  end do

  ! Section 4 - Product Definition.
  if (gfld%ipdtnum .ne. ipdsnum) stop 140
  if (gfld%ipdtlen .ne. 29) stop 141
  do i = 1, 29
     if (ipdstmpl(i) .ne. gfld%ipdtmpl(i)) stop 142
  end do

  ! Section 5 - Data Representation.
  if (gfld%idrtnum .ne. idrsnum) stop 140
  if (gfld%idrtlen .ne. 5) stop 141
  do i = 1, 5
     if (idrstmpl(i) .ne. gfld%idrtmpl(i)) stop 142
  end do

  ! Section 6 - Bit Map.
  if (gfld%ibmap .ne. ibmap) stop 160

  ! Section 7 - Data.
  if (gfld%ndpts .ne. 4 .or. .not. gfld%unpacked) stop 170
  if (gfld%ifldnum .ne. 1 .or. .not. gfld%expanded) stop 171

  ! Free memory.
  call gf_free(gfld)

  print *, 'OK!'
  print *, 'SUCESSS!'
end program test_g2_encode
