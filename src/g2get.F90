!> @file
!> @brief Get information from a GRIB2 message.
!> @author Edward Hartnett @date Mar 6, 2024

!> Find the number of gridded fields and Local Use Sections in a GRIB2
!> message. Also various checks are performed to see if the message is a
!> valid GRIB2 message.
!>
!> This function is similar to gribinfo(), but returns less information.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message in array
!> cgrib.
!> @param[out] listsec0 Contains information decoded from GRIB
!> Indicator Section 0. Must be dimensioned >= 2.
!> - listsec0(1) Discipline-GRIB Master Table Number (see [Code Table
!> - 0.0](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml))
!> - listsec0(2) GRIB Edition Number (currently 2)
!> - listsec0(3) Length of GRIB message
!> @param[out] listsec1 Contains information read from GRIB
!> Identification Section 1. Must be dimensioned >= 13.
!> - listsec1(1) Id of orginating centre (Common Code Table C-1)
!> - listsec1(2) Id of orginating sub-centre (local table)
!> - listsec1(3) GRIB Master Tables Version Number (Code Table 1.0)
!> - listsec1(4) GRIB Local Tables Version Number
!> - listsec1(5) Significance of Reference Time (Code Table 1.1)
!> - listsec1(6) Reference Time - Year (4 digits)
!> - listsec1(7) Reference Time - Month
!> - listsec1(8) Reference Time - Day
!> - listsec1(9) Reference Time - Hour
!> - listsec1(10) Reference Time - Minute
!> - listsec1(11) Reference Time - Second
!> - listsec1(12) Production status of data (Code Table 1.2)
!> - listsec1(13) Type of processed data (Code Table 1.3)
!> @param[out] numfields The number of gridded fieldse found in the
!> GRIB message.
!> @param[out] numlocal The number of Local Use Sections (Section 2)
!> found in the GRIB message.
!> @param[out] maxlocal The size of the largest Local Use Section
!> (Section 2). Can be used to ensure that the return array passed
!> to subroutine getlocal is dimensioned large enough.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 Could not find Section 1, where expected.
!> - 4 End string "7777" found, but not where expected.
!> - 5 End string "7777" not found at end of message.
!> - 6 Invalid section number found.
!>
!> @author Stephen Gilbert @date 2000-05-25
subroutine gb_info(cgrib, lcgrib, listsec0, listsec1, &
     numfields, numlocal, maxlocal, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(out) :: listsec0(3), listsec1(13)
  integer, intent(out) :: numlocal, numfields, maxlocal, ierr

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  integer, parameter :: zero = 0, one = 1
  integer, parameter :: mapsec1len = 13
  integer, parameter :: mapsec1(mapsec1len) = (/ 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1 /)
  integer :: iofst, istart
  integer :: nbits, lensec1, lensec0, lensec, lenposs, lengrib, j
  integer :: i, ipos, isecnum

  ierr = 0
  numlocal = 0
  numfields = 0
  maxlocal = 0

  ! Check for beginning of GRIB message in the first 100 bytes.
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j+3)
     if (ctemp .eq. grib ) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'gb_info:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  ! Unpack Section 0 - Indicator Section.
  iofst = 8 * (istart + 5)
  call g2_gbytec(cgrib, listsec0(1), iofst, 8)     ! Discipline
  iofst = iofst + 8
  call g2_gbytec(cgrib, listsec0(2), iofst, 8)     ! GRIB edition number
  iofst = iofst+8
  iofst = iofst + 32
  call g2_gbytec(cgrib, lengrib, iofst, 32)        ! Length of GRIB message
  iofst = iofst + 32
  listsec0(3) = lengrib
  lensec0 = 16
  ipos = istart + lensec0

  ! Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'gb_info: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  ! Unpack Section 1 - Identification Section.
  call g2_gbytec(cgrib, lensec1, iofst, 32)        ! Length of Section 1
  iofst = iofst + 32
  call g2_gbytec(cgrib, isecnum, iofst, 8)         ! Section number ( 1 )
  iofst = iofst + 8
  if (isecnum .ne. 1) then
     print *, 'gb_info: Could not find section 1.'
     ierr = 3
     return
  endif

  ! Unpack each input value in array listsec1 into the the appropriate
  ! number of octets, which are specified in corresponding entries in
  ! array mapsec1.
  do i = 1, mapsec1len
     nbits = mapsec1(i) * 8
     call g2_gbytec(cgrib, listsec1(i), iofst, nbits)
     iofst = iofst + nbits
  enddo
  ipos = ipos + lensec1

  ! Loop through the remaining sections to see if they are valid.
  ! Also count the number of times Section 2 and Section 4 appear.
  do
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // cgrib(ipos + 3)
     if (ctemp .eq. c7777 ) then
        ipos = ipos + 4
        if (ipos .ne. (istart + lengrib)) then
           print *, 'gb_info: "7777" found, but not where expected.'
           ierr = 4
           return
        endif
        exit
     endif
     iofst = (ipos - 1) * 8
     call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8)         ! Get Section number
     iofst = iofst + 8
     ipos = ipos+lensec                 ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'gb_info: "7777"  not found at end of GRIB message.'
        ierr = 5
        return
     endif
     if (isecnum .ge. 2 .AND. isecnum .le. 7) then
        if (isecnum .eq. 2) then     ! Local Section 2
           ! Increment counter for total number of local sections found.
           numlocal = numlocal + 1
           lenposs = lensec - 5
           if (lenposs .gt. maxlocal) maxlocal = lenposs
        elseif (isecnum .eq. 4) then
           ! Increment counter for total number of fields found.
           numfields = numfields + 1
        endif
     else
        print *, 'gb_info: Invalid section number found in GRIB message: ', isecnum
        ierr = 6
        return
     endif
  enddo

end subroutine gb_info

!> Find the number of Local Use Sections and gridded fields in
!> a GRIB2 message, and the maximum sizes of template arrays.
!>
!> This subroutine also performs various checks to see if the message
!> is a valid GRIB2 message. Also, a list of safe array dimensions is
!> returned for use in allocating return arrays from routines
!> getlocal(), gettemplates(), and getfields().
!>
!> Array maxvals contains the maximum possible number of values
!> that will be returned in argument arrays for routines getlocal(),
!> gettemplates() and getfields(). Users can use this info to determine
!> if their arrays are dimensioned large enough for the data that may
!> be returned from the above routines, or to dynamically allocate
!> arrays with a reasonable size.
!>
!> This function is similar to gb_info(), but returns more information.
!>
!> @note The actual number of values in these arrays will likely be
!> less than the values calculated by this routine.
!>
!> @param[in] cgrib Character that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of array cgrib.
!> @param[out] listsec0 Contains information needed for GRIB
!> Indicator Section 0. Must be dimensioned >= 2.
!> - listsec0(1) Discipline-GRIB Master Table Number ([Code Table 0.0]
!>   (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml)).
!> - listsec0(2) GRIB Edition Number (currently 2)
!> - listsec0(3) Length of GRIB message.
!> @param[out] listsec1 Contains information needed for GRIB
!> Identification Section 1. Must be dimensioned >= 13.
!> - listsec1(1) Id of orginating centre ([Table 0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html)).
!> - listsec1(2) Id of orginating sub-centre ([Table C]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html)).
!> - listsec1(3) GRIB Master Tables Version Number ([Table 1.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml)).
!> - listsec1(4) GRIB Local Tables Version Number ([Table 1.1]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml)).
!> - listsec1(5) Significance of Reference Time ([Table 1.2]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml))
!> - listsec1(6) Reference Time - Year (4 digits)
!> - listsec1(7) Reference Time - Month
!> - listsec1(8) Reference Time - Day
!> - listsec1(9) Reference Time - Hour
!> - listsec1(10) Reference Time - Minute
!> - listsec1(11) Reference Time - Second
!> - listsec1(12) Production status of data ([Table 1.3]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml)).
!> - listsec1(13) Type of processed data ([Table 1.4]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml)).
!> @param[out] numlocal The number of Local Use Sections (Section 2)
!> found in the GRIB message.
!> @param[out] numfields The number of gridded fieldse found in the
!> GRIB message.
!> @param[out] maxvals The maximum number of elements that could be
!> returned in various arrays from this GRIB2 message.
!> - maxvals(1) max length of local section 2 (for getlocal()).
!> - maxvals(2) max length of GDS Template (for gettemplates() and getfield()).
!> - maxvals(3) max length of GDS Optional list (for getfield()).
!> - maxvals(4) max length of PDS Template (for gettemplates() and getfield()).
!> - maxvals(5) max length of PDS Optional list (for getfield()).
!> - maxvals(6) max length of DRS Template (for gettemplates() and getfield()).
!> - maxvals(7) max number of gridpoints (for getfield()).
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 Could not find Section 1, where expected.
!> - 4 End string "7777" found, but not where expected.
!> - 5 End string "7777" not found at end of message.
!>
!> @author Stephen Gilbert @date 2000-05-25
subroutine gribinfo(cgrib, lcgrib, listsec0, listsec1,  &
     numlocal, numfields, maxvals, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(out) :: listsec0(3), listsec1(13), maxvals(7)
  integer, intent(out) :: numlocal, numfields, ierr

  integer :: i, ipos, isecnum, j, lengrib, lenposs, lensec, lensec0, lensec1
  integer :: maxcoordlist, maxdeflist, maxdrstmpl, maxgridpts, maxpdstmpl, maxgdstmpl
  integer :: maxsec2len, nbits, nbyte, ngdpts, numcoord
  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  integer, parameter :: zero = 0, one = 1
  integer, parameter :: mapsec1len = 13
  integer, parameter :: mapsec1(mapsec1len) = (/ 2, 2, 1, 1, 1, 2, 1, 1, &
       1, 1, 1, 1, 1 /)
  integer iofst, istart

  ierr = 0
  numlocal = 0
  numfields = 0
  maxsec2len = 1
  maxgdstmpl = 1
  maxdeflist = 1
  maxpdstmpl = 1
  maxcoordlist = 1
  maxdrstmpl = 1
  maxgridpts = 0

  !     Check for beginning of GRIB message in the first 100 bytes
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j + 3)
     if (ctemp .eq. grib) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'gribinfo:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  ! Unpack Section 0 - Indicator Section.
  iofst = 8 * (istart + 5)
  call g2_gbytec(cgrib, listsec0(1), iofst, 8) ! Discipline
  iofst = iofst + 8
  call g2_gbytec(cgrib, listsec0(2), iofst, 8) ! GRIB edition number
  iofst = iofst + 8
  iofst = iofst + 32
  call g2_gbytec(cgrib, lengrib, iofst, 32) ! Length of GRIB message
  iofst = iofst + 32
  listsec0(3) = lengrib
  lensec0 = 16
  ipos = istart + lensec0

  ! Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'gribinfo: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  ! Unpack Section 1 - Identification Section.
  call g2_gbytec(cgrib, lensec1, iofst, 32) ! Length of Section 1
  iofst = iofst + 32
  call g2_gbytec(cgrib, isecnum, iofst, 8) ! Section number (1)
  iofst = iofst + 8
  if (isecnum .ne. 1) then
     print *, 'gribinfo: Could not find section 1.'
     ierr = 3
     return
  endif

  ! Unpack each input value in array listsec1 into the the appropriate
  ! number of octets, which are specified in corresponding entries in
  ! array mapsec1.
  do i = 1, mapsec1len
     nbits = mapsec1(i) * 8
     call g2_gbytec(cgrib, listsec1(i), iofst, nbits)
     iofst = iofst + nbits
  enddo
  ipos = ipos + lensec1

  ! Loop through the remaining sections keeping track of the length of
  ! each. Also count the number of times Section 2 and Section 4
  ! appear.
  do
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // &
          cgrib(ipos + 3)
     if (ctemp .eq. c7777) then
        ipos = ipos + 4
        if (ipos .ne. (istart + lengrib)) then
           print *, 'gribinfo: "7777" found, but not where expected.'
           ierr = 4
           return
        endif
        exit
     endif
     iofst = (ipos - 1) * 8
     call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8) ! Get Section number
     iofst = iofst + 8
     ipos = ipos + lensec      ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'gribinfo: "7777"  not found at end of GRIB message.'
        ierr = 5
        return
     endif
     if (isecnum .eq. 2) then ! Local Section 2
        ! Increment counter for total number of local sections found
        ! and determine largest Section 2 in message.
        numlocal = numlocal + 1
        lenposs = lensec-5
        if (lenposs .gt. maxsec2len) maxsec2len = lenposs
     elseif (isecnum .eq. 3) then
        iofst = iofst + 8     ! skip source of grid def.
        call g2_gbytec(cgrib, ngdpts, iofst, 32) ! Get Num of Grid Points
        iofst = iofst + 32
        call g2_gbytec(cgrib, nbyte, iofst, 8) ! Get Num octets for opt. list
        iofst = iofst + 8
        if (ngdpts .gt. maxgridpts) maxgridpts = ngdpts
        lenposs = lensec - 14
        if (lenposs .gt. maxgdstmpl) maxgdstmpl = lenposs
        if (nbyte .ne. 0) then
           lenposs = lenposs / nbyte
           if (lenposs .gt. maxdeflist) maxdeflist = lenposs
        endif
     elseif (isecnum .eq. 4) then
        numfields = numfields + 1
        call g2_gbytec(cgrib, numcoord, iofst, 16) ! Get Num of Coord Values
        iofst = iofst + 16
        if (numcoord .ne. 0) then
           if (numcoord .gt. maxcoordlist) maxcoordlist = numcoord
        endif
        lenposs = lensec - 9
        if (lenposs.gt.maxpdstmpl) maxpdstmpl = lenposs
     elseif (isecnum .eq. 5) then
        lenposs = lensec-11
        if (lenposs .gt. maxdrstmpl) maxdrstmpl = lenposs
     endif
  enddo

  maxvals(1) = maxsec2len
  maxvals(2) = maxgdstmpl
  maxvals(3) = maxdeflist
  maxvals(4) = maxpdstmpl
  maxvals(5) = maxcoordlist
  maxvals(6) = maxdrstmpl
  maxvals(7) = maxgridpts
end subroutine gribinfo

!> Return the Grid Definition, Product Definition, Bit-map (if
!> applicable), and the unpacked data for a data field. Since there
!> can be multiple data fields packed into a GRIB2 message, the
!> calling routine indicates which field is being requested with the
!> ifldnum argument.
!>
!> @note Note that subroutine gribinfo() can be used to first
!> determine how many data fields exist in a given GRIB message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[in] ifldnum Specifies which field in the GRIB2 message to
!> return.
!> @param[out] igds Contains information read from the appropriate
!> GRIB Grid Definition Section 3 for the field being returned. Must
!> be dimensioned >= 5.
!> - igds(1) Source of grid definition (see [Code Table
!> 3.0](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml)).
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!> definition. Used to define number of points in each row (or
!> column) for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points definition
!> ([Code Table
!> 3.11](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!> - igds(5) Grid Definition Template Number ([Code Table
!> 3.1](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!> @param[out] igdstmpl Contains the data values for the specified
!> Grid Definition Template (NN=igds(5)). Each element of this
!> integer array contains an entry (in the order specified) of Grid
!> Defintion Template 3.NN. A safe dimension for this array can be
!> obtained in advance from maxvals(2), which is returned from
!> subroutine gribinfo().
!> @param[out] igdslen Number of elements in igdstmpl. i.e. number of
!> entries in Grid Defintion Template 3.NN (NN=igds(5)).
!> @param[out] ideflist (Used if igds(3) .ne. 0) This array contains
!> the number of grid points contained in each row (or column). (part
!> of Section 3) A safe dimension for this array can be obtained in
!> advance from maxvals(3), which is returned from subroutine
!> gribinfo().
!> @param[out] idefnum (Used if igds(3) .ne. 0) The number of entries
!> in array ideflist - i.e. number of rows (or columns) for which
!> optional grid points are defined.
!> @param[out] ipdsnum Product Definition Template Number (see [Code
!> Table 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[out] ipdstmpl Contains the data values for the specified
!> Product Definition Template (N=ipdsnum). Each element of this
!> integer array contains an entry (in the order specified) of
!> Product Defintion Template 4.N. A safe dimension for this array
!> can be obtained in advance from maxvals(4), which is returned from
!> subroutine gribinfo().
!> @param[out] ipdslen Number of elements in ipdstmpl - i.e. number
!> of entries in Product Defintion Template 4.N (N=ipdsnum).
!> @param[out] coordlist Array containg floating point values
!> intended to document the vertical discretisation associated to
!> model data on hybrid coordinate vertical levels (part of Section
!> 4). The dimension of this array can be obtained in advance from
!> maxvals(5), which is returned from subroutine gribinfo().
!> @param[out] numcoord number of values in array coordlist.
!> @param[out] ndpts Number of data points unpacked and returned.
!> @param[out] idrsnum Data Representation Template Number (see [Code
!> Table 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[out] idrstmpl Contains the data values for the specified
!> Data Representation Template (N=idrsnum). Each element of this
!> integer array contains an entry (in the order specified) of
!> Product Defintion Template 5.N A safe dimension for this array can
!> be obtained in advance from maxvals(6), which is returned from
!> subroutine gribinfo().
!> @param[out] idrslen Number of elements in idrstmpl. i.e. number of
!> entries in Data Representation Template specified by idrsnum.
!> @param[out] ibmap Bitmap indicator (see [Code Table
!> 6.0](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table6-0.shtml)).
!> - 0 bitmap applies and is included in Section 6.
!> - 1-253 Predefined bitmap applies.
!> - 254 Previously defined bitmap applies to this field.
!> - 255 Bit map does not apply to this product.
!> @param[out] bmap Logical*1 array containing decoded bitmap (if
!> ibmap=0). The dimension of this array can be obtained in advance
!> from maxvals(7), which is returned from subroutine gribinfo().
!> @param[out] fld Array of ndpts unpacked data points. A safe
!> dimension for this array can be obtained in advance from
!> maxvals(7), which is returned from subroutine gribinfo().
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 The data field request number was not positive.
!> - 4 End string "7777" found, but not where expected.
!> - 6 GRIB message did not contain the requested number of data fields.
!> - 7 End string "7777" not found at end of message.
!> - 9 Data Representation Template 5.NN not yet implemented.
!> - 10 Error unpacking Section 3.
!> - 11 Error unpacking Section 4.
!> - 12 Error unpacking Section 5.
!> - 13 Error unpacking Section 6.
!> - 14 Error unpacking Section 7.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine getfield(cgrib, lcgrib, ifldnum, igds, igdstmpl, &
     igdslen, ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, &
     coordlist, numcoord, ndpts, idrsnum, idrstmpl, idrslen,  &
     ibmap, bmap, fld, ierr)

  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ifldnum
  integer, intent(out) :: igds(*), igdstmpl(*), ideflist(*)
  integer, intent(out) :: ipdsnum, ipdstmpl(*)
  integer, intent(out) :: idrsnum, idrstmpl(*)
  integer, intent(out) :: ndpts, ibmap, idefnum, numcoord
  integer, intent(out) :: ierr
  logical*1, intent(out) :: bmap(*)
  real, intent(out) :: fld(*), coordlist(*)

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  integer :: listsec0(2)
  integer :: iofst, istart
  real (kind = 4) :: ieee(1)
  logical :: have3, have4, have5, have6, have7

  !implicit none additions
  integer, intent(out) :: igdslen, ipdslen, idrslen
  integer :: numfld, j, lengrib, lensec0, ipos
  integer :: lensec, isecnum, jerr, ier, numlocal

  have3 = .false.
  have4 = .false.
  have5 = .false.
  have6 = .false.
  have7 = .false.
  ierr = 0
  numfld = 0
  numlocal = 0

  !     Check for valid request number
  if (ifldnum .le. 0) then
     print *, 'getfield: Request for field number ' &
          ,'must be positive.'
     ierr = 3
     return
  endif

  !     Check for beginning of GRIB message in the first 100 bytes
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j + 3)
     if (ctemp .eq. grib) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'getfield:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  !     Unpack Section 0 - Indicator Section
  iofst = 8 * (istart + 5)
  call g2_gbytec(cgrib, listsec0(1), iofst, 8) ! Discipline
  iofst = iofst + 8
  call g2_gbytec(cgrib, listsec0(2), iofst, 8) ! GRIB edition number
  iofst = iofst + 8
  iofst = iofst + 32
  call g2_gbytec(cgrib, lengrib, iofst, 32) ! Length of GRIB message
  iofst = iofst + 32
  lensec0 = 16
  ipos = istart + lensec0

  !     Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'getfield: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  !     Loop through the remaining sections keeping track of the length of
  !     each. Also keep the latest Grid Definition Section info.  Unpack
  !     the requested field number.
  do
     !         Check to see if we are at end of GRIB message
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // &
          cgrib(ipos + 3)
     if (ctemp .eq. c7777) then
        ipos = ipos + 4
        !             If end of GRIB message not where expected, issue error
        if (ipos.ne.(istart + lengrib)) then
           print *, 'getfield: "7777" found, but not ' &
                ,'where expected.'
           ierr = 4
           return
        endif
        exit
     endif
     !         Get length of Section and Section number
     iofst = (ipos - 1) * 8
     call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8) ! Get Section number
     iofst = iofst + 8

     !         If found Section 3, unpack the GDS info using the appropriate
     !         template. Save in case this is the latest grid before the
     !         requested field.
     if (isecnum .eq. 3) then
        iofst = iofst - 40    ! reset offset to beginning of section
        call unpack3(cgrib, lcgrib, iofst, igds, igdstmpl,  &
             igdslen, ideflist, idefnum, jerr)
        if (jerr .eq. 0) then
           have3 = .true.
        else
           ierr = 10
           return
        endif
     endif

     !         If found Section 4, check to see if this field is the one
     !         requested.
     if (isecnum .eq. 4) then
        numfld = numfld + 1
        if (numfld .eq. ifldnum) then
           iofst = iofst - 40 ! reset offset to beginning of section
           call unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
                ipdslen, coordlist, numcoord, jerr)
           if (jerr .eq. 0) then
              have4 = .true.
           else
              ierr = 11
              return
           endif
        endif
     endif

     !         If found Section 5, check to see if this field is the one
     !         requested.
     if ((isecnum .eq. 5) .and. (numfld .eq. ifldnum)) then
        iofst = iofst - 40    ! reset offset to beginning of section
        call unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum, &
             idrstmpl, idrslen, jerr)
        if (jerr .eq. 0) then
           have5 = .true.
        else
           ierr = 12
           return
        endif
     endif

     !         If found Section 6, Unpack bitmap. Save in case this is the
     !         latest bitmap before the requested field.
     if (isecnum .eq. 6) then
        iofst = iofst - 40    ! reset offset to beginning of section
        call unpack6(cgrib, lcgrib, iofst, igds(2), ibmap, bmap, &
             jerr)
        if (jerr .eq. 0) then
           have6 = .true.
        else
           ierr = 13
           return
        endif
     endif

     !         If found Section 7, check to see if this field is the one
     !         requested.
     if ((isecnum .eq. 7) .and. (numfld .eq. ifldnum)) then
        if (idrsnum .eq. 0) then
           call simunpack(cgrib(ipos + 5), lensec - 6, idrstmpl, &
                ndpts, fld)
           have7 = .true.
        elseif (idrsnum .eq. 2 .or. idrsnum .eq. 3) then
           call comunpack(cgrib(ipos + 5), lensec - 6, lensec, &
                idrsnum,idrstmpl, ndpts, fld, ier)
           if (ier .ne. 0) then
              ierr = 14
              return
           endif
           have7 = .true.
        elseif (idrsnum .eq. 50) then
           call simunpack(cgrib(ipos + 5), lensec - 6, idrstmpl, &
                ndpts - 1, fld(2))
           ieee = transfer(idrstmpl(5), ieee, 1)
           call rdieee(ieee, fld(1), 1)
           have7 = .true.
        elseif (idrsnum .eq. 40 .or. idrsnum .eq. 40000) then
           call jpcunpack(cgrib(ipos + 5), lensec - 5, idrstmpl, &
                ndpts, fld)
           have7 = .true.
        elseif (idrsnum .eq. 41 .or. idrsnum .eq. 40010) then
           call pngunpack(cgrib(ipos + 5), lensec - 5, idrstmpl, &
                ndpts, fld)
           have7 = .true.
        else
           print *, 'getfield: Data Representation Template ', &
                idrsnum, ' not yet implemented.'
           ierr = 9
           return
        endif
     endif

     !         Check to see if we read pass the end of the GRIB message and
     !         missed the terminator string '7777'.
     ipos = ipos + lensec      ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'getfield: "7777"  not found at end' &
             ,' of GRIB message.'
        ierr = 7
        return
     endif

     if (have3 .and. have4 .and. have5 .and. have6 .and. have7) &
          return

  enddo

  !     If exited from above loop, the end of the GRIB message was reached
  !     before the requested field was found.
  print *, 'getfield: GRIB message contained ', numlocal,  &
       ' different fields.'
  print *, 'getfield: The request was for the ', ifldnum,  &
       ' field.'
  ierr = 6

end subroutine getfield

!>    This subroutine unpacks Section 3 (Grid Definition Section)
!>    starting at octet 6 of that Section.
!>
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning (in) or the end
!>    (out) of Section 3.
!>    @param[out] igds Contains information read from the appropriate
!>    GRIB Grid Definition Section 3 for the field being returned. Must
!>    be dimensioned >= 5.

!>    - igds(1) Source of grid definition (see [Code Table - 3.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml))
!>    - igds(2) Number of grid points in the defined grid.
!>    - igds(3) Number of octets needed for each additional grid points
!>    definition. Used to define number of points in each row (or
!>    column) for non-regular grids. = 0, if using regular grid.
!>    - igds(4) Interpretation of list for optional points
!>    definition. ([Code Table 3.11]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!>    - igds(5) Grid Definition Template Number ([Code Table 3.1]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!>    @param[out] igdstmpl Contains the data values for the specified
!>    Grid Definition Template (NN=igds(5)). Each element of this
!>    integer array contains an entry (in the order specified) of Grid
!>    Defintion Template 3.NN.
!>    @param[out] mapgridlen Number of elements in igdstmpl -
!>    i.e. number of entries in Grid Defintion Template 3.NN
!>    (NN=igds(5)).
!>    @param[out] ideflist (Used if igds(3) .ne. 0). This array contains
!>    the number of grid points contained in each row (or column) (part
!>    of Section 3).
!>    @param[out] idefnum (Used if igds(3) .ne. 0). The number of
!>    entries in array ideflist - i.e. number of rows (or columns) for
!>    which optional grid points are defined.
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 5 "GRIB" message contains an undefined Grid Definition Template.
!>
!>    @author Stephen Gilbert @date 2000-05-26
!>
subroutine unpack3(cgrib, lcgrib, iofst, igds, igdstmpl,  &
     mapgridlen, ideflist, idefnum, ierr)

  use gridtemplates
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, intent(out) :: igds(*), igdstmpl(*), ideflist(*)
  integer, intent(out) :: ierr, idefnum

  integer, allocatable :: mapgrid(:)
  integer :: mapgridlen, ibyttem
  logical needext

  !implicit none additions
  integer :: lensec, iret, i, nbits, isign, newmapgridlen

  ierr = 0

  call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8             ! skip section number

  call g2_gbytec(cgrib, igds(1), iofst, 8) ! Get source of Grid def.
  iofst = iofst + 8
  call g2_gbytec(cgrib, igds(2), iofst, 32) ! Get number of grid pts.
  iofst = iofst + 32
  call g2_gbytec(cgrib, igds(3), iofst, 8) ! Get num octets for opt. list
  iofst = iofst + 8
  call g2_gbytec(cgrib, igds(4), iofst, 8) ! Get interpret. for opt. list
  iofst = iofst + 8
  call g2_gbytec(cgrib, igds(5), iofst, 16) ! Get Grid Def Template num.
  iofst = iofst + 16
  if (igds(1) .eq. 0) then
     !      if (igds(1).eq.0.OR.igds(1).eq.255) then  ! FOR ECMWF TEST ONLY
     allocate(mapgrid(lensec))
     !         Get Grid Definition Template
     call getgridtemplate(igds(5), mapgridlen, mapgrid, needext,  &
          iret)
     if (iret .ne. 0) then
        ierr = 5
        return
     endif
  else
     !        igdstmpl = -1
     mapgridlen = 0
     needext = .false.
  endif

  !     Unpack each value into array igdstmpl from the the appropriate
  !     number of octets, which are specified in corresponding entries in
  !     array mapgrid.
  ibyttem = 0
  do i = 1, mapgridlen
     nbits = iabs(mapgrid(i)) * 8
     if (mapgrid(i) .ge. 0) then
        call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, igdstmpl(i), iofst + 1, nbits-1)
        if (isign .eq. 1) igdstmpl(i) = -igdstmpl(i)
     endif
     iofst = iofst + nbits
     ibyttem = ibyttem + iabs(mapgrid(i))
  enddo

  !      Check to see if the Grid Definition Template needs to be
  !      extended. The number of values in a specific template may vary
  !      depending on data specified in the "static" part of the template.
  if (needext) then
     call extgridtemplate(igds(5), igdstmpl, newmapgridlen, &
          mapgrid)
     !         Unpack the rest of the Grid Definition Template
     do i = mapgridlen + 1, newmapgridlen
        nbits = iabs(mapgrid(i)) * 8
        if (mapgrid(i) .ge. 0) then
           call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
        else
           call g2_gbytec(cgrib, isign, iofst, 1)
           call g2_gbytec(cgrib, igdstmpl(i), iofst + 1, nbits - &
                1)
           if (isign .eq. 1) igdstmpl(i) = -igdstmpl(i)
        endif
        iofst = iofst + nbits
        ibyttem = ibyttem + iabs(mapgrid(i))
     enddo
     mapgridlen = newmapgridlen
  endif

  !     Unpack optional list of numbers defining number of points in each
  !     row or column, if included. This is used for non regular grids.
  if (igds(3) .ne. 0) then
     nbits = igds(3) * 8
     idefnum = (lensec - 14 - ibyttem) / igds(3)
     call g2_gbytesc(cgrib, ideflist, iofst, nbits, 0, idefnum)
     iofst = iofst + (nbits * idefnum)
  else
     idefnum = 0
  endif
  if (allocated(mapgrid)) deallocate(mapgrid)
end subroutine unpack3

!>    This subroutine unpacks Section 4 (Product Definition Section)
!>    starting at octet 6 of that Section.
!>
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning (in) or the end
!>    (out) of Section 4.
!>    @param[out] ipdsnum Product Definition Template Number (see [Code
!>    Table 4.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!>    @param[out] ipdstmpl Contains the data values for the specified
!>    Product Definition Template (N=ipdsnum). Each element of this
!>    integer array contains an entry (in the order specified) of
!>    Product Defintion Template 4.N.
!>    @param[out] mappdslen Number of elements in ipdstmpl. i.e. number
!>    of entries in Product Defintion Template 4.N (N=ipdsnum).
!>    @param[out] coordlist- Array containg floating point values
!>    intended to document the vertical discretisation associated to
!>    model data on hybrid coordinate vertical levels (part of Section
!>    4).
!>    @param[out] numcoord number of values in array coordlist.
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 5 GRIB message contains an undefined Product Definition Template.
!>
!>    @author Stephen Gilbert @date 2000-05-26
subroutine unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
     mappdslen, coordlist, numcoord, ierr)

  use pdstemplates
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  real, intent(out) :: coordlist(*)
  integer, intent(out) :: ipdsnum, ipdstmpl(*)
  integer, intent(out) :: ierr, numcoord

  real(4), allocatable :: coordieee(:)
  integer, allocatable :: mappds(:)
  integer :: mappdslen
  logical needext

  !implicit none additions
  integer :: lensec, iret, i, nbits, isign, newmappdslen

  ierr = 0

  call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8             ! skip section number
  allocate(mappds(lensec))

  call g2_gbytec(cgrib, numcoord, iofst, 16) ! Get num of coordinate values
  iofst = iofst + 16
  call g2_gbytec(cgrib, ipdsnum, iofst, 16) ! Get Prod. Def Template num.
  iofst = iofst + 16
  !     Get Product Definition Template.
  call getpdstemplate(ipdsnum, mappdslen, mappds, needext, iret)
  if (iret.ne.0) then
     ierr = 5
     return
  endif

  !     Unpack each value into array ipdstmpl from the the appropriate
  !     number of octets, which are specified in corresponding entries in
  !     array mappds.
  do i = 1, mappdslen
     nbits = iabs(mappds(i))*8
     if (mappds(i).ge.0) then
        call g2_gbytec(cgrib, ipdstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, ipdstmpl(i), iofst + 1, nbits-1)
        if (isign.eq.1) ipdstmpl(i) = -ipdstmpl(i)
     endif
     iofst = iofst + nbits
  enddo

  !     Check to see if the Product Definition Template needs to be
  !     extended. The number of values in a specific template may vary
  !     depending on data specified in the "static" part of the template.
  if (needext) then
     call extpdstemplate(ipdsnum, ipdstmpl, newmappdslen, mappds)

     !         Unpack the rest of the Product Definition Template
     do i = mappdslen + 1, newmappdslen
        nbits = iabs(mappds(i))*8
        if (mappds(i).ge.0) then
           call g2_gbytec(cgrib, ipdstmpl(i), iofst, nbits)
        else
           call g2_gbytec(cgrib, isign, iofst, 1)
           call g2_gbytec(cgrib, ipdstmpl(i), iofst + 1, nbits-1)
           if (isign.eq.1) ipdstmpl(i) = -ipdstmpl(i)
        endif
        iofst = iofst + nbits
     enddo
     mappdslen = newmappdslen
  endif

  !     Get Optional list of vertical coordinate values after the Product
  !     Definition Template, if necessary.
  if (numcoord .ne. 0) then
     allocate (coordieee(numcoord))
     call g2_gbytescr(cgrib, coordieee, iofst, 32, 0, numcoord)
     call rdieee(coordieee, coordlist, numcoord)
     deallocate (coordieee)
     iofst = iofst + (32*numcoord)
  endif
  if (allocated(mappds)) deallocate(mappds)
end subroutine unpack4

!>    This subroutine unpacks Section 5 (Data Representation Section)
!>    starting at octet 6 of that Section.
!>
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning (in) or the
!>    end(out) of Section 5.
!>    @param[out] ndpts Number of data points unpacked and returned.
!>    @param[out] idrsnum Data Representation Template Number (see [Code
!>    Table 5.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml))
!>    @param[out] idrstmpl Contains the data values for the specified
!>    Data Representation Template (N = idrsnum). Each element of this
!>    integer array contains an entry (in the order specified) of Data
!>    Representation Template 5.N.
!>    @param[out] mapdrslen Number of elements in idrstmpl. i.e. number
!>    of entries in Data Representation Template 5.N (N = idrsnum).
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 7 GRIB message contains an undefined Data Representation Template.
!>
!>    @author Stephen Gilbert @date 2000-05-26
subroutine unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum,  &
     idrstmpl, mapdrslen, ierr)

  use drstemplates
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, intent(out) :: ndpts, idrsnum, idrstmpl(*)
  integer, intent(out) :: ierr

  !      integer, allocatable :: mapdrs(:)
  integer, allocatable :: mapdrs(:)
  integer :: mapdrslen
  logical needext

  !implicit none additions
  integer :: lensec, i, nbits, isign, newmapdrslen, iret

  ierr = 0

  call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8             ! skip section number
  allocate(mapdrs(lensec))

  call g2_gbytec(cgrib, ndpts, iofst, 32) ! Get num of data points
  iofst = iofst + 32
  call g2_gbytec(cgrib, idrsnum, iofst, 16) ! Get Data Rep Template Num.
  iofst = iofst + 16
  !     Gen Data Representation Template
  call getdrstemplate(idrsnum, mapdrslen, mapdrs, needext, iret)
  if (iret.ne.0) then
     ierr = 7
     return
  endif

  !     Unpack each value into array ipdstmpl from the the appropriate
  !     number of octets, which are specified in corresponding entries in
  !     array mappds.
  do i = 1, mapdrslen
     nbits = iabs(mapdrs(i))*8
     if (mapdrs(i).ge.0) then
        call g2_gbytec(cgrib, idrstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, idrstmpl(i), iofst + 1, nbits-1)
        if (isign.eq.1) idrstmpl(i) = -idrstmpl(i)
     endif
     iofst = iofst + nbits
  enddo

  !     Check to see if the Data Representation Template needs to be
  !     extended. The number of values in a specific template may vary
  !     depending on data specified in the "static" part of the template.
  if (needext) then
     call extdrstemplate(idrsnum, idrstmpl, newmapdrslen, mapdrs)
     !         Unpack the rest of the Data Representation Template
     do i = mapdrslen + 1, newmapdrslen
        nbits = iabs(mapdrs(i))*8
        if (mapdrs(i).ge.0) then
           call g2_gbytec(cgrib, idrstmpl(i), iofst, nbits)
        else
           call g2_gbytec(cgrib, isign, iofst, 1)
           call g2_gbytec(cgrib, idrstmpl(i), iofst + 1, nbits - 1)
           if (isign.eq.1) idrstmpl(i) = -idrstmpl(i)
        endif
        iofst = iofst + nbits
     enddo
     mapdrslen = newmapdrslen
  endif
  if (allocated(mapdrs)) deallocate(mapdrs)
end subroutine unpack5

!>    This subroutine unpacks Section 6 (Bit-Map Section) starting at
!>    octet 6 of that Section.
!>
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning (in) or the end
!>    (out) of Section 6.
!>    @param[in] ngpts Number of grid points specified in the bit-map.
!>    @param[out] ibmap Bitmap indicator (see [Code Table 6.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table6-0.shtml)).
!>    - 0 bitmap applies and is included in Section 6.
!>    - 1-253 Predefined bitmap applies.
!>    - 254 Previously defined bitmap applies to this field.
!>    - 255 Bit map does not apply to this product.
!>    @param[out] bmap Logical*1 array containing decoded bitmap (if
!>    ibmap = 0).
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 4 Unrecognized pre-defined bit-map.
!>
!>    @author Stephen Gilbert @date 2000-05-26
subroutine unpack6(cgrib, lcgrib, iofst, ngpts, ibmap, bmap, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ngpts
  integer, intent(inout) :: iofst
  integer, intent(out) :: ibmap
  integer, intent(out) :: ierr
  logical*1, intent(out) :: bmap(ngpts)

  integer :: intbmap(ngpts)

  !implicit none additions
  integer :: j

  ierr = 0

  iofst = iofst + 32            ! skip Length of Section
  iofst = iofst + 8             ! skip section number

  call g2_gbytec(cgrib, ibmap, iofst, 8) ! Get bit-map indicator
  iofst = iofst + 8

  if (ibmap.eq.0) then      ! Unpack bitmap
     call g2_gbytesc(cgrib, intbmap, iofst, 1, 0, ngpts)
     iofst = iofst + ngpts
     do j = 1, ngpts
        bmap(j) = .true.
        if (intbmap(j).eq.0) bmap(j) = .false.
     enddo
  elseif (ibmap.eq.254) then ! Use previous bitmap
     return
  elseif (ibmap.eq.255) then ! No bitmap in message
     bmap(1:ngpts) = .true.
  else
     print *, 'unpack6: Predefined bitmap ', ibmap, &
          ' not recognized.'
     ierr = 4
  endif
end subroutine unpack6

!> Return the dimensions and scanning mode of a grid
!> definition packed in the Grid Definition Section.
!>
!> @param[in] csec3 Character array that contains the packed GRIB2
!> GDS.
!> @param[in] lcsec3 Length (in octets) of section 3.
!> @param[out] width x (or i) dimension of the grid.
!> @param[out] height y (or j) dimension of the grid.
!> @param[out] iscan Scanning mode (see [Code Table 3.4]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-4.shtml)).
!>
!> @note Returns width and height set to zero, if grid template not
!> recognized.
!>
!> @author Stephen Gilbert @date 2002-12-11
subroutine getdim(csec3,lcsec3,width,height,iscan)
  implicit none

  character(len=1),intent(in) :: csec3(*)
  integer,intent(in) :: lcsec3
  integer,intent(out) :: width,height,iscan

  integer,pointer,dimension(:) :: igdstmpl,list_opt
  integer :: igds(5)
  integer iofst,igdtlen,num_opt,jerr

  interface
     subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl, &
          mapgridlen,ideflist,idefnum,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: igdstmpl,ideflist
       integer,intent(out) :: igds(5)
       integer,intent(out) :: ierr,idefnum
     end subroutine gf_unpack3
  end interface

  nullify(igdstmpl,list_opt)

  iofst=0       ! set offset to beginning of section
  call gf_unpack3(csec3,lcsec3,iofst,igds,igdstmpl, &
       igdtlen,list_opt,num_opt,jerr)
  if (jerr.eq.0) then
     selectcase( igds(5) )     !  Template number
     case (0:3)   ! Lat/Lon
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(19)
     case (10)   ! Mercator
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(16)
     case (20)   ! Polar Stereographic
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(18)
     case (30)   ! Lambert Conformal
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(18)
     case (40:43)   ! Gaussian
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(19)
     case (90)   ! Space View/Orthographic
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(17)
     case (110)   ! Equatorial Azimuthal
        width=igdstmpl(8)
        height=igdstmpl(9)
        iscan=igdstmpl(16)
     case default
        width=0
        height=0
        iscan=0
     end select
  else
     width=0
     height=0
  endif

  if (associated(igdstmpl)) deallocate(igdstmpl)
  if (associated(list_opt)) deallocate(list_opt)

  return
end subroutine getdim

!> Return the contents of Section 2 (Local Use) from a GRIB2 message.
!>
!> Since there can be multiple occurrences of Section 2 within a GRIB
!> message, the calling routine indicates which occurrence is being
!> requested with the localnum argument.
!>
!> @note Note that subroutine gb_info() can be used to first determine
!> how many Local Use sections exist in a given GRIB message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[in] localnum The nth occurrence of Section 2 requested.
!> @param[out] csec2 Character array containing information read from
!> Section 2. The dimension of this array can be obtained in advance
!> from argument maxlocal, which is returned from subroutine gb_info().
!> @param[out] lcsec2 Number of bytes of character array csec2 read
!> from Section 2.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 The data field request number was not positive.
!> - 4 End string "7777" found, but not where expected.
!> - 5 End string "7777" not found at end of message.
!> - 6 GRIB message did not contain the requested number of data fields.
!>
!> @author Stephen Gilbert @date 2000-05-25
subroutine getlocal(cgrib, lcgrib, localnum, csec2, lcsec2, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, localnum
  character(len = 1), intent(out) :: csec2(*)
  integer, intent(out) :: lcsec2, ierr

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  integer :: listsec0(2)
  integer iofst, istart, numlocal
  integer :: lengrib, lensec, lensec0, j, ipos, isecnum

  ierr = 0
  numlocal = 0

  ! Check for valid request number.
  if (localnum .le. 0) then
     print *, 'getlocal: Request for local section must be positive.'
     ierr = 3
     return
  endif

  ! Check for beginning of GRIB message in the first 100 bytes
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j)//cgrib(j+1)//cgrib(j+2)//cgrib(j+3)
     if (ctemp .eq. grib) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'getlocal:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  ! Unpack Section 0 - Indicator Section
  iofst = 8 * (istart + 5)
  call g2_gbytec(cgrib, listsec0(1), iofst, 8)     ! Discipline
  iofst = iofst + 8
  call g2_gbytec(cgrib, listsec0(2), iofst, 8)     ! GRIB edition number
  iofst = iofst + 8
  iofst = iofst + 32
  call g2_gbytec(cgrib, lengrib, iofst, 32)        ! Length of GRIB message
  iofst = iofst + 32
  lensec0 = 16
  ipos = istart + lensec0

  ! Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'getlocal: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  ! Loop through the remaining sections keeping track of the length of
  ! each. Also check to see that if the current occurrence of Section
  ! 2 is the same as the one requested.
  do
     ! Check to see if we are at end of GRIB message
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // cgrib(ipos + 3)
     if (ctemp .eq. c7777) then
        ipos = ipos + 4
        
        ! If end of GRIB message not where expected, issue error
        if (ipos .ne. (istart + lengrib)) then
           print *, 'getlocal: "7777" found, but not where expected.'
           ierr = 4
           return
        endif
        exit
     endif

     ! Get length of Section and Section number
     iofst = (ipos - 1) * 8
     call g2_gbytec(cgrib, lensec, iofst, 32)        ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8)         ! Get Section number
     iofst = iofst + 8
     
     ! If found the requested occurrence of Section 2, 
     ! return the section contents.
     if (isecnum .eq. 2) then
        numlocal = numlocal + 1
        if (numlocal.eq.localnum) then
           lcsec2 = lensec - 5
           csec2(1:lcsec2) = cgrib(ipos + 5:ipos + lensec - 1)
           return
        endif
     endif
     
     ! Check to see if we read pass the end of the GRIB
     ! message and missed the terminator string '7777'.
     ipos = ipos + lensec                 ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'getlocal: "7777"  not found at end of GRIB message.'
        ierr = 5
        return
     endif
  enddo

  ! If exited from above loop, the end of the GRIB message was reached
  ! before the requested occurrence of section 2 was found.
  print *, 'getlocal: GRIB message contained ', numlocal, ' local sections.'
  print *, 'getlocal: The request was for the ', localnum,  ' occurrence.'
  ierr = 6

end subroutine getlocal

!> Return the J, K, and M pentagonal resolution
!> parameters specified in a GRIB2 Grid Definition Section used
!> spherical harmonic coefficients using GDT 5.50 through 5.53.
!>
!> @param[in] csec3 Character array containing the packed GRIB2 GDS
!> @param[in] lcsec3 Length (in octets) of section 3
!> @param[out] JJ =J pentagonal resolution parameter
!> @param[out] KK =K pentagonal resolution parameter
!> @param[out] MM =M pentagonal resolution parameter
!>
!> @note Returns JJ, KK, and MM set to zero, if grid template not
!> recognized.
!>
!> @author Stephen Gilbert @date 2002-12-11
subroutine getpoly(csec3,lcsec3,jj,kk,mm)
  implicit none

  character(len=1),intent(in) :: csec3(*)
  integer,intent(in) :: lcsec3
  integer,intent(out) :: jj,kk,mm

  integer,pointer,dimension(:) :: igdstmpl,list_opt
  integer :: igds(5)
  integer iofst,igdtlen,num_opt,jerr

  interface
     subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl, &
          mapgridlen,ideflist,idefnum,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: igdstmpl,ideflist
       integer,intent(out) :: igds(5)
       integer,intent(out) :: ierr,idefnum
     end subroutine gf_unpack3
  end interface

  nullify(igdstmpl,list_opt)

  iofst=0       ! set offset to beginning of section
  call gf_unpack3(csec3,lcsec3,iofst,igds,igdstmpl, &
       igdtlen,list_opt,num_opt,jerr)
  if (jerr.eq.0) then
     selectcase( igds(5) )     !  Template number
     case (50:53)   ! Spherical harmonic coefficients
        jj=igdstmpl(1)
        kk=igdstmpl(2)
        mm=igdstmpl(3)
     case default
        jj=0
        kk=0
        mm=0
     end select
  else
     jj=0
     kk=0
     mm=0
  endif

  if (associated(igdstmpl)) deallocate(igdstmpl)
  if (associated(list_opt)) deallocate(list_opt)

end subroutine getpoly

!> Return the Grid Definition and Product Definition for a data field.
!> Since there can be multiple data fields packed into a GRIB2
!> message, the calling routine indicates which field is being
!> requested with the ifldnum argument.
!>
!> @note Note that subroutine gribinfo() can be used to first
!> determine how many data fields exist in a given GRIB message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[in] ifldnum Specifies which field in the GRIB2 message to return.
!> @param[out] igds Contains information read from the appropriate GRIB Grid
!> Definition Section 3 for the field being returned. Must be dimensioned >= 5.
!> - igds(1) Source of grid definition (see Code Table 3.0)
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points definition.
!> Used to define number of points in each row (or column) for non-regular
!> grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points definition. (Code Table 3.11)
!> - igds(5) Grid Definition Template Number (Code Table 3.1)
!> @param[out] igdstmpl Contains the data values for the specified Grid Definition
!> Template (NN=igds(5)). Each element of this integer array contains an entry
!> (in the order specified) of Grid Defintion Template 3.NN.
!> @param[out] igdslen Number of elements in igdstmpl. i.e. number of entries
!> in Data Representation Template 5.N (N=idrsnum).
!> @param[out] ideflist (Used if igds(3) .ne. 0) This array contains the
!> number of grid points contained in each row (or column). (part of Section 3).
!> @param[out] idefnum (Used if igds(3) .ne. 0) The number of entries in array
!> ideflist. i.e. number of rows (or columns) for which optional grid points are defined.
!> @param[out] ipdsnum Product Definition Template Number (see Code Table 4.0).
!> @param[out] ipdstmpl Contains the data values for the specified Product Definition
!> Template (N=ipdsnum). Each element of this integer array contains an entry
!> (in the order specified) of Product Defintion Template 4.N. A safe
!> dimension for this array can be obtained in advance from maxvals(4),
!> which is returned from subroutine gribinfo().
!> @param[out] ipdslen Number of elements in ipdstmpl. i.e. number of entries
!> in Product Defintion Template 4.N (N=ipdsnum).
!> @param[out] coordlist Array containg floating point values intended to
!> document the vertical discretisation associated to model data on hybrid
!> coordinate vertical levels. (part of Section 4) The dimension of this
!> array can be obtained in advance from maxvals(5), which is returned
!> from subroutine gribinfo().
!> @param[out] numcoord number of values in array coordlist.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 The data field request number was not positive.
!> - 4 End string "7777" found, but not where expected.
!> - 5 End string "7777" not found at end of message.
!> - 6 GRIB message did not contain the requested number of data fields.
!> - 7 End string "7777" not found at end of message.
!> - 9 Data Representation Template 5.NN not yet implemented.
!> - 10 Error unpacking Section 3.
!> - 11 Error unpacking Section 4.
!> - 12 Error unpacking Section 5.
!> - 13 Error unpacking Section 6.
!> - 14 Error unpacking Section 7.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gettemplates(cgrib, lcgrib, ifldnum, igds, igdstmpl, &
     igdslen, ideflist, idefnum, ipdsnum, ipdstmpl, ipdslen, coordlist, numcoord, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ifldnum
  integer, intent(out) :: igds(*), igdstmpl(*), ideflist(*)
  integer, intent(out) :: ipdsnum, ipdstmpl(*)
  integer, intent(out) :: idefnum, numcoord
  integer, intent(out) :: ierr
  real, intent(out) :: coordlist(*)

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  integer:: listsec0(2)
  integer iofst, istart
  logical have3, have4
  integer :: igdslen, ipdslen, ipos, isecnum, j, jerr, lengrib, lensec, lensec0, numfld

  interface
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
  end interface

  have3 = .false.
  have4 = .false.
  ierr = 0
  numfld = 0

  ! Check for valid request number.
  if (ifldnum .le. 0) then
     print *, 'gettemplates: Request for field number must be positive.'
     ierr = 3
     return
  endif

  ! Check for beginning of GRIB message in the first 100 bytes.
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j + 3)
     if (ctemp .eq. grib ) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'gettemplates:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  ! Unpack Section 0 - Indicator Section.
  iofst = 8 * (istart + 5)
  call g2_gbytec1(cgrib, listsec0(1), iofst, 8)     ! Discipline
  iofst = iofst + 8
  call g2_gbytec1(cgrib, listsec0(2), iofst, 8)     ! GRIB edition number
  iofst = iofst + 8
  iofst = iofst + 32
  call g2_gbytec1(cgrib, lengrib, iofst, 32)        ! Length of GRIB message
  iofst = iofst + 32
  lensec0 = 16
  ipos = istart + lensec0

  ! Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'gettemplates: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  ! Loop through the remaining sections keeping track of the length of
  ! each. Also keep the latest Grid Definition Section info. Unpack
  ! the requested field number.
  do
     ! Check to see if we are at end of GRIB message.
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // cgrib(ipos + 3)
     if (ctemp .eq. c7777 ) then
        ipos = ipos+4
        ! If end of GRIB message not where expected, issue error
        if (ipos .ne. (istart + lengrib)) then
           print *, 'gettemplates: "7777" found, but not where expected.'
           ierr = 4
           return
        endif
        exit
     endif
     ! Get length of Section and Section number.
     iofst = (ipos - 1) * 8
     call g2_gbytec1(cgrib, lensec, iofst, 32)        ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec1(cgrib, isecnum, iofst, 8)         ! Get Section number
     iofst = iofst + 8
     !print *, ' lensec =  ', lensec, '    secnum =  ', isecnum

     ! If found Section 3, unpack the GDS info using the appropriate
     ! template. Save in case this is the latest grid before the
     ! requested field.
     if (isecnum .eq. 3) then
        iofst = iofst - 40       ! reset offset to beginning of section
        call unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, igdslen, ideflist, idefnum, jerr)
        if (jerr .eq. 0) then
           have3 = .true.
        else
           ierr = 10
           return
        endif
     endif

     ! If found Section 4, check to see if this field is the one
     ! requested.
     if (isecnum .eq. 4) then
        numfld = numfld + 1
        if (numfld .eq. ifldnum) then
           iofst = iofst - 40       ! reset offset to beginning of section
           call unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, ipdslen, coordlist, numcoord, jerr)
           if (jerr .eq. 0) then
              have4 = .true.
           else
              ierr = 11
              return
           endif
        endif
     endif

     ! Check to see if we read pass the end of the GRIB message and
     ! missed the terminator string '7777'.
     ipos = ipos + lensec                 ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'gettemplates: "7777"  not found at end of GRIB message.'
        ierr = 7
        return
     endif

     if (have3.and.have4) return
  enddo

  ! If exited from above loop, the end of the GRIB message was reached
  ! before the requested field was found.
  print *, 'gettemplates: GRIB message contained ', numfld, ' different fields.'
  print *, 'gettemplates: The request was for the ', ifldnum,  ' field.'
  ierr = 6

end subroutine gettemplates
