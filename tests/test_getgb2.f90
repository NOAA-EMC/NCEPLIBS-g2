! This program creates a grib file to use in testing other source files
!
! Ed Hartnett 11/12/21 (use from test_gribcreate)
! Brian Curtis 11/26/2021 (modify to just create GRIB message)
program test_getgb2
  use grib_mod
  use bacio_module
  implicit none

  ! Define what I need
  integer, parameter :: lugb = 1
  integer :: lugi = lugb
  integer, parameter :: j = 0
  integer, parameter :: jdisc = 0
  integer :: jids(13)
  integer, parameter :: jpdtn = 0
  integer, parameter :: ipdstmplen = 15
  integer :: jpdt(ipdstmplen)
  integer, parameter :: jgdtn = 0
  integer, parameter :: igdstmplen = 19
  integer :: jgdt(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  logical, parameter :: unpack = .false.
  integer :: k
  type(gribfield) :: gfld
  integer :: iret
  integer :: i
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

  jpdt(1) = 0
  jpdt(2) = 0
  jpdt(3) = 0
  jpdt(4) = 0
  jpdt(5) = 0
  jpdt(6) = 12
  jpdt(7) = 59
  jpdt(8) = 0
  jpdt(9) = 0
  jpdt(10) = 1
  jpdt(11) = 1
  jpdt(12) = 1
  jpdt(13) = 2
  jpdt(14) = 1
  jpdt(15) = 1

  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  jids(1) = 7 ! US National Weather Service - NCEP (WMC)
  jids(2) = 4 ! Environmental Modeling Center
  jids(3) = 2 ! GRIB master tables version number (currently 2)
  jids(4) = 24 ! Version Implemented on 06 November 2019
  jids(5) = 0 ! Local tables not used.
  jids(6) = 2021 ! Year
  jids(7) = 11 ! Month
  jids(8) = 13 ! Day
  jids(9) = 15 ! Hour
  jids(10) = 59 ! Minute
  jids(11) = 59 ! Second
  jids(12) = 1 !  Operational Test Products
  jids(13) = 0 ! Analysis Products

  ! WHAT I NEED
  !#jids = listsec1
  !#jpdtn = ipdsnum
  !#jpdt = ipdstmpl
  !#jgdtn = idefnum
  !#jgdt = igdstmpl

  call baopenr(1, "testgrib.grb2", iret)
  if (iret .ne. 0) then
      print *, 'baopenr failed with iret value: ', iret
      stop 3
  end if

  call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
              unpack, k, gfld, iret)
  if (iret .ne. 0) then
      print *, 'getgb2 failed with iret value: ', iret
      stop 4
  end if

  call baclose(1, iret)
  if (iret .ne. 0) then
      print *, 'baclose failed with iret value: ', iret
      stop 5
  end if

  if (gfld%version .ne. 2) stop 10

  if (gfld%discipline .ne. 0) stop 20

  do i = 1, 13
    if (gfld%idsect(i) .ne. jids(i)) stop 30
  end do

  if (gfld%idsectlen .ne. 13) stop 40

  do i = 1, lcsec2
    if (gfld%local(i) .ne. csec2(i)) stop 50
  end do

  if (gfld%locallen .ne. lcsec2) stop 60

              !>    Field number within GRIB message.
  print *, 'ifldnum: ', gfld%ifldnum
!              integer :: ifldnum

              !>    Source of grid definition (see [Code Table 3.0]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml))
              !>     - 0 Specified in field igdtnum (as defined in [Code table 3.1]
              !>     (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml).
              !>     - 1 Predetermined grid Defined by originating centre.
  print *, 'griddef: ', gfld%griddef
!              integer :: griddef

              !>    Number of grid points in the defined grid. Note that the number of
              !>    actual data values returned from getgb2() (in ndpts) may be less
              !>    than this value if a logical bitmap is in use with grid points
              !>    that are being masked out.
!              integer :: ngrdpts

              !>    Number of octets needed for each additional grid points
              !>    definition. Used to define number of points in each row
              !>    (or column) for non-regular grids. Equal to 0, if using
              !>    regular grid.
!              integer :: numoct_opt

              !>    Interpretation of list of numbers at end of section
              !>    3 - the Grid Definiton Section. (See [Code Table 3.11]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!              integer :: interp_opt

              !>    (Used if numoct_opt .ne. 0.) The number of entries in
              !>    array ideflist. i.e. number of rows (or columns) for
              !>    which optional grid points are defined. This value is
              !>    set to zero, if umoct_opt = 0.
!              integer :: num_opt

              !>    (Used if numoct_opt .ne. 0.) This array contains the
              !>    number of grid points contained in each row (or
              !>    column) (part of Section 3), This element is actually a
              !>    pointer to an array that holds the data. This pointer is
              !>    null if numoct_opt equals 0.
!              integer, pointer, dimension(:) :: list_opt => null()

              !>    Grid Definition Template Number ([Code Table 3.1]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!              integer :: igdtnum

              !>    Number of elements in the Grid Defintion Template
              !>    specified by igdtnum.
!              integer :: igdtlen

              !>    Contains the data values for the Grid Definition
              !>    Template specified by igdtnum. This element is actually
              !>    a pointer to an array that holds the data.
!              integer, pointer, dimension(:) :: igdtmpl => null()

              !>    Product Definition Template Number ([Code Table 4.0]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!              integer :: ipdtnum

              !>    Number of elements in the Product Defintion Template
              !>    specified by ipdtnum.
!              integer :: ipdtlen

              !>    Contains the data values for the Product Definition
              !>    Template specified by ipdtnum. This element is actually
              !>    a pointer to an array that holds the data.
!              integer, pointer, dimension(:) :: ipdtmpl => null()

              !>    Number of values in array coord_list. This is part of
              !>    Section 3 - the Grid Definition Section.
!              integer :: num_coord

              !>    Real array containing floating point values intended to
              !>    document the vertical discretisation associated to model
              !>    data on hybrid coordinate vertical levels (part of
              !>    Section 3 - the Grid Definition Section). This element
              !>    is actually a pointer to an array that holds the data.
!              real, pointer, dimension(:) :: coord_list => null()

              !>    Number of data points unpacked and returned. Note that
              !>    this number may be different from the value of ngrdpts
              !>    if a logical bitmap is in use with grid points that are
              !>    being masked out.
!              integer :: ndpts

              !>    Data Representation Template Number ([Code Table 5.0]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!              integer :: idrtnum

              !>    Number of elements in the Data Representation Template
              !>    array specified by idrtnum.
!              integer :: idrtlen

              !>    Contains the data values for the Data Representation
              !>    Template specified by idrtnum. This element is actually
              !>    a pointer to an array that holds the data.
!              integer, pointer, dimension(:) :: idrtmpl => null()

              !>    Logical value indicating whether the bitmap and data
              !>    values were unpacked. If false, bmap and fld pointers
              !>    are null.
!              logical :: unpacked

              !>    Logical value indicating whether the data field was
              !>    expanded to the grid in the case where a bit-map is
              !>    present. If true, the data points in fld match the grid
              !>    points and zeros were inserted at grid points where data
              !>    was bit-mapped out. If false, the data values in fld
              !>    were not expanded to the grid and are just a consecutive
              !>    array of data points corresponding to each value of "1"
              !>    in bmap.
!              logical :: expanded

              !>    Bitmap indicator (see [Code Table 6.0]
              !>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table6-0.shtml)).
              !>     - 0 bitmap applies and is included in Section 6.
              !>     - 1-253 Predefined bitmap applies.
              !>     - 254 Previously defined bitmap applies to this field.
              !>     - 255 Bit map does not apply to this product.
!              integer :: ibmap

              !>    Logical*1 array containing decoded bitmap, if ibmap
              !>    equals 0 or 254 - otherwise null. This element is
              !>    actually a pointer to an array that holds the data.
!              logical*1, pointer, dimension(:) :: bmap => null()

              !>    Array of ndpts unpacked data points. This element is
              !>    actually a pointer to an array that holds the data.
!              real, pointer, dimension(:) :: fld => null()

  print *, 'SUCCESS!'

end program test_getgb2
