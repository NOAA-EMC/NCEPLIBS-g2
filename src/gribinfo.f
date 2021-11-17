!>    @file
!>    @brief This subroutines searches the number of Local Use Sections
!>    and gridded fields.
!>    @author Stephen Gilbert @date 2000-05-25

!>    This subroutine searches through a GRIB2 message and returns
!>    the number of Local Use Sections and number of gridded fields
!>    found in the message. It also performs various checks to see
!>    if the message is a valid GRIB2 message. Last, a list of safe
!>    array dimensions is returned for use in allocating return
!>    arrays from routines getlocal, gettemplates, and getfields.
!>
!>    @param[in] cgrib Character that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of array cgrib.
!>    @param[out] listsec0 Contains information needed for GRIB
!>    Indicator Section 0. Must be dimensioned >= 2.
!>    - listsec0(1) Discipline-GRIB Master Table Number ([Code Table 0.0]
!>      (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml)).
!>    - listsec0(2) GRIB Edition Number (currently 2)
!>    - listsec0(3) Length of GRIB message.
!>    @param[out] listsec1 Contains information needed for GRIB
!>    Identification Section 1. Must be dimensioned >= 13.
!>    - listsec1(1) Id of orginating centre ([Table 0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html)).
!>    - listsec1(2) Id of orginating sub-centre ([Table C]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html)).
!>    - listsec1(3) GRIB Master Tables Version Number ([Table 1.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml)).
!>    - listsec1(4) GRIB Local Tables Version Number ([Table 1.1]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml)).
!>    - listsec1(5) Significance of Reference Time ([Table 1.2]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml))
!>    - listsec1(6) Reference Time - Year (4 digits)
!>    - listsec1(7) Reference Time - Month
!>    - listsec1(8) Reference Time - Day
!>    - listsec1(9) Reference Time - Hour
!>    - listsec1(10) Reference Time - Minute
!>    - listsec1(11) Reference Time - Second
!>    - listsec1(12) Production status of data ([Table 1.3]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml)).
!>    - listsec1(13) Type of processed data ([Table 1.4]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml)).
!>    @param[out] numlocal The number of Local Use Sections (Section 2)
!>    found in the GRIB message.
!>    @param[out] numfields The number of gridded fieldse found in the
!>    GRIB message.
!>    @param[out] maxvals The maximum number of elements that could be
!>    returned in various arrays from this GRIB2 message.
!>    - maxvals(1) max length of local section 2 (for getlocal()).
!>    - maxvals(2) max length of GDS Template (for gettemplates() and getfield()).
!>    - maxvals(3) max length of GDS Optional list (for getfield()).
!>    - maxvals(4) max length of PDS Template (for gettemplates() and getfield()).
!>    - maxvals(5) max length of PDS Optional list (for getfield()).
!>    - maxvals(6) max length of DRS Template (for gettemplates() and getfield()).
!>    - maxvals(7) max number of gridpoints (for getfield()).
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 1 Beginning characters "GRIB" not found.
!>    - 2 GRIB message is not Edition 2.
!>    - 3 Could not find Section 1, where expected.
!>    - 4 End string "7777" found, but not where expected.
!>    - 5 End string "7777" not found at end of message.
!>
!>    @note Array maxvals contains the maximum possible number of values
!>    that will be returned in argument arrays for routines getlocal(),
!>    gettemplates() and getfields(). Users can use this info to determine
!>    if their arrays are dimensioned large enough for the data that may
!>    be returned from the above routines, or to dynamically allocate
!>    arrays with a reasonable size. NOTE that the actual number of values
!>    in these arrays is returned from the routines and will likely be
!>    less than the values calculated by this routine.
!>
!>    @author Stephen Gilbert @date 2000-05-25
      subroutine gribinfo(cgrib, lcgrib, listsec0, listsec1, 
     &     numlocal, numfields, maxvals, ierr)

      character(len = 1), intent(in) :: cgrib(lcgrib)
      integer, intent(in) :: lcgrib
      integer, intent(out) :: listsec0(3), listsec1(13), maxvals(7)
      integer, intent(out) :: numlocal, numfields, ierr

      character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
      character(len = 4) :: ctemp
      integer, parameter :: zero = 0, one = 1
      integer, parameter :: mapsec1len = 13
      integer, parameter :: mapsec1(mapsec1len) = (/ 2, 2, 1, 1, 1, 2, 1
     $     , 1, 1, 1, 1, 1, 1 /)
      integer iofst, ibeg, istart

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
          ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j +
     $         3)
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

!     Unpack Section 0 - Indicator Section
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

!     Currently handles only GRIB Edition 2.
      if (listsec0(2) .ne. 2) then
          print *, 'gribinfo: can only decode GRIB edition 2.'
          ierr = 2
          return
      endif

!     Unpack Section 1 - Identification Section
      call g2_gbytec(cgrib, lensec1, iofst, 32) ! Length of Section 1
      iofst = iofst + 32
      call g2_gbytec(cgrib, isecnum, iofst, 8) ! Section number (1)
      iofst = iofst + 8
      if (isecnum .ne. 1) then
          print *, 'gribinfo: Could not find section 1.'
          ierr = 3
          return
      endif

!     Unpack each input value in array listsec1 into the the appropriate
!     number of octets, which are specified in corresponding entries in
!     array mapsec1.
      do i = 1, mapsec1len
          nbits = mapsec1(i) * 8
          call g2_gbytec(cgrib, listsec1(i), iofst, nbits)
          iofst = iofst + nbits
      enddo
      ipos = ipos + lensec1

!     Loop through the remaining sections keeping track of the length of
!     each. Also count the number of times Section 2 and Section 4
!     appear.
      do
          ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) //
     $         cgrib(ipos + 3)
          if (ctemp .eq. c7777) then
              ipos = ipos + 4
              if (ipos .ne. (istart + lengrib)) then
                  print *, 'gribinfo: "7777" found, but not '
     $                 ,'where expected.'
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
              print *, 'gribinfo: "7777"  not found at end of '
     $             ,'GRIB message.'
              ierr = 5
              return
          endif
          if (isecnum .eq. 2) then ! Local Section 2
!             Increment counter for total number of local sections found
!             and determine largest Section 2 in message.
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

      return
      end
