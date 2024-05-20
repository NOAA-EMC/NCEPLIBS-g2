!> @file
!> @brief Subroutines to create a GRIB2 message.
!> @author Ed Hartnett @date Mar 5, 2024

!> Initialize a new GRIB2 message and pack GRIB2 sections 0
!> (Indicator) and 1 (Identification).
!>
!> This routine is used with routines addlocal(), addgrid(), addfield(),
!> and gribend() to create a complete GRIB2 message. Subroutine
!> gribcreate() must be called first to initialize a new GRIB2 message.
!> Also, a call to gribend() is required to complete GRIB2 message
!> after all fields have been added.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum Length (in bytes) of array cgrib.
!> @param[in] listsec0 Contains information needed for GRIB Indicator
!> Section 0. Must be dimensioned >= 2.
!> - listsec0(1) Discipline-GRIB Master Table Number ([Code Table 0.0]
!>   (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml)).
!> - listsec0(2) GRIB Edition Number (currently 2)
!> @param[in] listsec1 Contains information needed for GRIB
!> [Identification Section 1](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml).
!> Must be dimensioned >= 13.
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
!> @param[out] ierr Error return code.
!> - 0 no error
!> - 1 Tried to use for version other than GRIB Edition 2
!>
!> @note This routine is intended for use with routines
!> addlocal(), addgrid(), addfield(), and gribend() to create a
!> complete GRIB2 message.
!>
!> @author Stephen Gilbert @date 2000-04-28
subroutine gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  implicit none

  character(len = 1), intent(inout) :: cgrib(lcgrib)
  integer, intent(in) :: listsec0(*), listsec1(*)
  integer, intent(in) :: lcgrib
  integer, intent(out) :: ierr

  integer :: i, lensec1, nbits
  character(len = 4), parameter :: grib = 'GRIB'
  integer, parameter :: ZERO = 0, ONE = 1
  integer, parameter :: mapsec1len = 13
  integer, parameter :: mapsec1(mapsec1len) = (/ 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1 /)
  integer lensec0, iofst, ibeg
    
  interface
     subroutine g2_sbytec(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec
     subroutine g2_sbytec1(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec1
  end interface

  ierr = 0

#ifdef LOGGING
  print *, 'gribcreate lcgrib ', lcgrib
#endif

  ! Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'gribcreate: can only code GRIB edition 2.'
     ierr = 1
     return
  endif

  !  Pack Section 0 - Indicator Section (except for total length of
  !  GRIB message).
  cgrib(1) = grib(1:1) ! Beginning of GRIB message
  cgrib(2) = grib(2:2)
  cgrib(3) = grib(3:3)
  cgrib(4) = grib(4:4)
  call g2_sbytec1(cgrib, ZERO, 32, 16)           ! reserved for future use
  call g2_sbytec(cgrib, listsec0(1), 48, 8)     ! Discipline
  call g2_sbytec(cgrib, listsec0(2), 56, 8)     ! GRIB edition number
  lensec0 = 16      ! bytes (octets)

  ! Pack Section 1 - Identification Section.
  ibeg = lensec0 * 8        !   Calculate offset for beginning of section 1
  iofst = ibeg + 32         !   leave space for length of section
  call g2_sbytec1(cgrib, ONE, iofst, 8)     ! Store section number ( 1 )
  iofst = iofst + 8

  ! Pack up each input value in array listsec1 into the the
  ! appropriate number of octets, which are specified in corresponding
  ! entries in array mapsec1.
  do i = 1, mapsec1len
     nbits = mapsec1(i) * 8
     call g2_sbytec(cgrib, listsec1(i), iofst, nbits)
     iofst = iofst + nbits
  enddo

  ! Calculate length of section 1 and store it in octets 1-4 of
  ! section 1.
  lensec1 = (iofst - ibeg) / 8
  call g2_sbytec1(cgrib, lensec1, ibeg, 32)

  ! Put current byte total of message into Section 0.
  call g2_sbytec1(cgrib, ZERO, 64, 32)
  call g2_sbytec1(cgrib, lensec0 + lensec1, 96, 32)
end subroutine gribcreate

!> Pack up Sections 4 through 7 for a field and add them to a
!> GRIB2 message.
!>
!> They are the Product Definition Section, Data Representation
!> Section, Bit-Map Section and Data Sections.
!>
!> This routine is used with routines gribcreate(), addlocal(),
!> addgrid(), and gribend() to create a complete GRIB2
!> message. Subroutine gribcreate() must be called first to initialize
!> a new GRIB2 message. Subroutine addgrid() must be called after
!> gribcreate() and before this routine to add the appropriate grid
!> description to the GRIB2 message. A call to gribend() is required
!> to complete GRIB2 message after all fields have been added.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum length (bytes) of array cgrib.
!> @param[in] ipdsnum Product Definition Template Number (see [Code
!> Table 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[in] ipdstmpl Contains the data values for the
!> Product Definition Template specified by ipdsnum.
!> @param[in] ipdstmplen Max dimension of ipdstmpl.
!> @param[out] coordlist Array containg floating point values intended to
!> document the vertical discretisation associated to model data on hybrid
!> coordinate vertical levels (part of Section 4).
!> @param[in] numcoord - number of values in array coordlist.
!> @param[in] idrsnum - Data Representation Template Number (see
!> [Code Table 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[in] idrstmpl Contains the data values for the Data
!> Representation Template specified by idrsnum. Note that some
!> values in this template (eg. reference values, number of bits,
!> etc...) may be changed by the data packing algorithms. Use this
!> to specify scaling factors and order of spatial differencing, if
!> desired.
!> @param[in] idrstmplen Max dimension of idrstmpl. This must be at
!> least as large as the length of the selected PDS template.
!> @param[in] fld Array of data points to pack.
!> @param[out] ngrdpts Number of data points in grid. i.e. size of
!> fld and bmap.
!> @param[out] ibmap Bitmap indicator (see [Code Table
!> 6.0](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table6-0.shtml)).
!> - 0 bitmap applies and is included in Section 6.
!> - 1-253 Predefined bitmap applies
!> - 254 Previously defined bitmap applies to this field
!> - 255 Bit map does not apply to this product.
!> @param[out] bmap Logical*1 array containing bitmap to be added
!> (if ibmap=0 or ibmap=254).
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 GRIB message was not initialized. Need to call
!> routine gribcreate first.
!> - 2 GRIB message already complete. Cannot add new section.
!> - 3 Sum of Section byte counts does not add to total
!> byte count.
!> - 4 Previous Section was not 3 or 7.
!> - 5 Could not find requested Product Definition Template.
!> - 6 Section 3 (GDS) not previously defined in message.
!> - 7 Tried to use unsupported Data Representationi Template.
!> - 8 Specified use of a previously defined bitmap, but one
!> does not exist in the GRIB message.
!> - 9 GDT of one of 5.50 through 5.53 required to pack
!> using DRT 5.51.
!> - 10 Error packing data field.
!>
!> @author Stephen Gilbert @date 2000-05-02
subroutine addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
     coordlist, numcoord, idrsnum, idrstmpl, &
     idrstmplen, fld, ngrdpts, ibmap, bmap, ierr)
  use pdstemplates
  use drstemplates
  implicit none

  logical :: match
  character(len=1), intent(inout) :: cgrib(lcgrib)
  integer, intent(in) :: ipdsnum, ipdstmpl(*)
  integer, intent(in) :: idrsnum, numcoord, ipdstmplen, idrstmplen
  integer, intent(in) :: lcgrib, ngrdpts, ibmap
  real, intent(in) :: coordlist(numcoord)
  real(kind = 4) :: coordlist_4(numcoord)
  real, target, intent(in) :: fld(ngrdpts)
  integer, intent(out) :: ierr
  integer, intent(inout) :: idrstmpl(*)
  logical*1, intent(in) :: bmap(ngrdpts)

  character(len=4), parameter :: grib='GRIB', c7777='7777'
  character(len=4):: ctemp
  character(len=1), allocatable :: cpack(:)
  real, pointer, dimension(:) :: pfld
  real(4) :: coordieee(numcoord), re00, tmpre00(1)
  integer(4) :: ire00, allones
  integer :: mappds(ipdstmplen), intbmap(ngrdpts), mapdrs(idrstmplen)
  integer, parameter :: zero=0, one=1, four=4, five=5, six=6, seven=7
  integer, parameter :: minsize=50000
  integer iofst, ibeg, lencurr, len, mappdslen, mapdrslen, lpos3
  integer width, height, ndpts
  integer lensec3, lensec4, lensec5, lensec6, lensec7
  logical issec3, needext, isprevbmap
  integer :: nbits, newlen, nsize, lcpack, left
  integer :: ibmprev, ilen, ioctet, iscan, isecnum, itemp
  integer :: i, jj, kk, mm
  integer :: iret, istat
  real (kind = 4) :: tmpfld(1)

  interface
     subroutine g2_gbytec(in, iout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: iout(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
     subroutine g2_gbytec81(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
       integer (kind = 8) :: iout(1)
     end subroutine g2_gbytec81
     subroutine g2_sbytec(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec
     subroutine g2_sbytec1(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec1
  end interface

  allones = int(Z'FFFFFFFF')
  ierr = 0

  ! Check to see if beginning of GRIB message exists.
  match = .true.
  do i = 1, 4
     if (cgrib(i) /= grib(i:i)) then
        match = .false.
     endif
  enddo
  if (.not. match) then
     print *, 'addfield: GRIB not found in given message.'
     print *, 'addfield: Call to routine gribcreate required to initialize GRIB messge.'
     ierr = 1
     return
  endif

  ! Get current length of GRIB message.
  call g2_gbytec1(cgrib, lencurr, 96, 32)

  ! Check to see if GRIB message is already complete.
  ctemp = cgrib(lencurr-3) // cgrib(lencurr - 2) // cgrib(lencurr - 1) // cgrib(lencurr)
  if (ctemp .eq. c7777) then
     print *, 'addfield: GRIB message already complete.  Cannot add new section.'
     ierr = 2
     return
  endif

  ! Loop through all current sections of the GRIB message to find the
  ! last section number.
  issec3 = .false.
  isprevbmap = .false.
  len = 16 ! length of Section 0
  do
     ! Get number and length of next section
     iofst = len * 8
     call g2_gbytec1(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec1(cgrib, isecnum, iofst, 8)
     iofst = iofst + 8
     
     ! Check if previous Section 3 exists and save location of
     ! the section 3 in case needed later.
     if (isecnum .eq. 3) then
        issec3 = .true.
        lpos3 = len + 1
        lensec3 = ilen
     endif
     
     ! Check if a previous defined bitmap exists
     if (isecnum .eq. 6) then
        call g2_gbytec1(cgrib, ibmprev, iofst, 8)
        iofst = iofst + 8
        if ((ibmprev .ge. 0) .and. (ibmprev .le. 253)) isprevbmap = .true.
     endif
     len = len + ilen
     
     ! Exit loop if last section reached
     if (len .eq. lencurr) exit
     
     ! If byte count for each section does not match current
     ! total length, then there is a problem.
     if (len .gt. lencurr) then
        print *, 'addfield: Section byte counts don''t add to total.'
        print *, 'addfield: Sum of section byte counts = ', len
        print *, 'addfield: Total byte count in Section 0 = ', lencurr
        ierr = 3
        return
     endif
  enddo

  ! Sections 4 through 7 can only be added after section 3 or 7.
  if ((isecnum .ne. 3) .and. (isecnum .ne. 7)) then
     print *, 'addfield: Sections 4-7 can only be added after  Section 3 or 7.'
     print *, 'addfield: Section ', isecnum, ' was the last found in', &
          ' given GRIB message.'
     ierr = 4
     return

     ! Sections 4 through 7 can only be added if section 3 was previously defined.
  elseif (.not. issec3) then
     print *, 'addfield: Sections 4-7 can only be added if Section', &
          ' 3 was previously included.'
     print *, 'addfield: Section 3 was not found in given GRIB message.'
     print *, 'addfield: Call to routine addgrid required', &
          ' to specify Grid definition.'
     ierr = 6
     return
  endif

  ! Add Section 4 - Product Definition Section.
  ibeg = lencurr * 8 ! Calculate offset for beginning of section 4
  iofst = ibeg + 32 ! leave space for length of section
  call g2_sbytec1(cgrib, four, iofst, 8) ! Store section number (4)
  iofst = iofst + 8
  call g2_sbytec1(cgrib, numcoord, iofst, 16) ! Store num of coordinate values
  iofst = iofst + 16
  call g2_sbytec1(cgrib, ipdsnum, iofst, 16) ! Store Prod Def Template num.
  iofst = iofst + 16

  ! Get Product Definition Template.
  call getpdstemplate(ipdsnum, mappdslen, mappds, needext, iret)
  if (iret .ne. 0) then
     ierr = 5
     return
  endif

  ! Extend the Product Definition Template, if necessary.
  ! The number of values in a specific template may vary
  ! depending on data specified in the "static" part of the
  ! template.
  if (needext) then
     call extpdstemplate(ipdsnum, ipdstmpl, mappdslen, mappds)
  endif

  ! Pack up each input value in array ipdstmpl into the
  ! the appropriate number of octets, which are specified in
  ! corresponding entries in array mappds.
  do i = 1, mappdslen
     nbits = iabs(mappds(i)) * 8
     if ((mappds(i) .ge. 0).or.(ipdstmpl(i) .ge. 0)) then
        call g2_sbytec(cgrib, ipdstmpl(i), iofst, nbits)
     else
        call g2_sbytec1(cgrib, one, iofst, 1)
        call g2_sbytec1(cgrib, iabs(ipdstmpl(i)), iofst + 1, nbits - 1)
     endif
     iofst = iofst+nbits
  enddo

  ! Add Optional list of vertical coordinate values
  ! after the Product Definition Template, if necessary.
  if (numcoord .ne. 0) then
     do i = 1, numcoord
        coordlist_4(i) = real(coordlist(i), 4)
     end do
     call mkieee(coordlist_4, coordieee, numcoord)
     call g2_sbytescr(cgrib, coordieee, iofst, 32, 0, numcoord)
     iofst = iofst + (32 * numcoord)
  endif

  ! Calculate length of section 4 and store it in octets
  ! 1-4 of section 4.
  lensec4 = (iofst-ibeg)/8
  call g2_sbytec1(cgrib, lensec4, ibeg, 32)

  ! Pack Data using appropriate algorithm

  ! Get Data Representation Template
  call getdrstemplate(idrsnum, mapdrslen, mapdrs, needext, iret)
  if (iret .ne. 0) then
     ierr = 5
     return
  endif

  ! contract data field, removing data at invalid grid points,
  ! if bit-map is provided with field.
  if (ibmap .eq. 0 .OR. ibmap .eq. 254) then
     allocate(pfld(max(2, ngrdpts)))
     ndpts = 0;
     do jj = 1, ngrdpts
        intbmap(jj) = 0
        if (bmap(jj)) then
           intbmap(jj) = 1
           ndpts = ndpts + 1
           pfld(ndpts) = fld(jj);
        endif
     enddo
     if (ndpts == 0 .and. ngrdpts > 0) then
        pfld(1) = 0
     endif
  else
     ndpts = ngrdpts;
     pfld=>fld;
  endif
  lcpack = 0
  nsize = ndpts*4
  if (nsize .lt. minsize) nsize = minsize
  allocate(cpack(nsize), stat = istat)
  if (idrsnum.eq.0) then ! Simple Packing
     call simpack(pfld, ndpts, idrstmpl, cpack, lcpack)
  elseif (idrsnum.eq.2.or.idrsnum.eq.3) then ! Complex Packing
     call cmplxpack(pfld, ndpts, idrsnum, idrstmpl, cpack, lcpack)
  elseif (idrsnum.eq.50) then ! Sperical Harmonic Simple Packing
     call simpack(pfld(2), ndpts-1, idrstmpl, cpack, lcpack)
     tmpfld(1) = real(pfld(1), 4)
     call mkieee(tmpfld, tmpre00, 1) ! ensure RE(0,0) value is IEEE format
     re00 = tmpre00(1)
     ire00 = transfer(re00, ire00)
     idrstmpl(5) = ire00
  elseif (idrsnum.eq.51) then ! Sperical Harmonic Complex Packing
     call getpoly(cgrib(lpos3), lensec3, jj, kk, mm)
     if (jj.ne.0 .AND. kk.ne.0 .AND. mm.ne.0) then
        call specpack(pfld, ndpts, jj, kk, mm, idrstmpl, cpack, lcpack)
     else
        print *, 'addfield: Cannot pack DRT 5.51.'
        ierr=9
        return
     endif

  elseif (idrsnum.eq.40 .OR. idrsnum.eq.40000) then ! JPEG2000 encoding
     if (ibmap.eq.255) then
        call getdim(cgrib(lpos3), lensec3, width, height, iscan)
        if (width.eq.0 .OR. height.eq.0) then
           width=ndpts
           height=1
        elseif (width.eq.allones .OR. height.eq.allones) then
           width=ndpts
           height=1
        elseif (ibits(iscan, 5, 1) .eq. 1) then ! Scanning mode: bit 3
           itemp=width
           width=height
           height=itemp
        endif
     else
        width=ndpts
        height=1
     endif
     if (width<1 .or. height<1) then
        ! Special case: bitmask off everywhere.
        write(0, *) 'Warning: bitmask off everywhere.'
        write(0, *) '   Pretend one point in jpcpack to avoid crash.'
        width=1
        height=1
     endif
     lcpack=nsize
     !print *, 'w, h=', width, height
     call jpcpack(pfld, width, height, idrstmpl, cpack, lcpack)

  elseif (idrsnum.eq.41 .OR. idrsnum.eq.40010) then ! PNG encoding
     if (ibmap.eq.255) then
        call getdim(cgrib(lpos3), lensec3, width, height, iscan)
        if (width.eq.0 .OR. height.eq.0) then
           width=ndpts
           height=1
        elseif (width.eq.allones .OR. height.eq.allones) then
           width=ndpts
           height=1
        elseif (ibits(iscan, 5, 1) .eq. 1) then ! Scanning mode: bit 3
           itemp=width
           width=height
           height=itemp
        endif
     else
        width=ndpts
        height=1
     endif
     !print *, 'png size ', width, height
     call pngpack(pfld, width, height, idrstmpl, cpack, lcpack)
     !print *, 'png packed'
  else
     print *, 'addfield: Data Representation Template 5.', idrsnum, &
          ' not yet implemented.'
     ierr=7
     return
  endif
  if (ibmap.eq.0 .OR. ibmap.eq.254) then
     deallocate(pfld)
  endif
  if (lcpack .lt. 0) then
     if (allocated(cpack))deallocate(cpack)
     ierr=10
     return
  endif

  !     Add Section 5 - Data Representation Section
  ibeg=iofst ! Calculate offset for beginning of section 5
  iofst=ibeg + 32 ! leave space for length of section
  call g2_sbytec1(cgrib, five, iofst, 8) ! Store section number (5)
  iofst=iofst + 8
  call g2_sbytec1(cgrib, ndpts, iofst, 32) ! Store num of actual data points
  iofst=iofst + 32
  call g2_sbytec1(cgrib, idrsnum, iofst, 16) ! Store Data Repr. Template num.
  iofst = iofst + 16

  ! Pack up each input value in array idrstmpl into the
  ! the appropriate number of octets, which are specified in
  ! corresponding entries in array mapdrs.
  do i=1, mapdrslen
     nbits =iabs(mapdrs(i)) * 8
     if ((mapdrs(i) .ge. 0) .or. (idrstmpl(i) .ge. 0)) then
        call g2_sbytec(cgrib, idrstmpl(i), iofst, nbits)
     else
        call g2_sbytec1(cgrib, one, iofst, 1)
        call g2_sbytec1(cgrib, iabs(idrstmpl(i)), iofst+1, nbits-1)
     endif
     iofst = iofst + nbits
  enddo

  ! Calculate length of section 5 and store it in octets
  ! 1-4 of section 5.
  lensec5 = (iofst - ibeg) / 8
  call g2_sbytec1(cgrib, lensec5, ibeg, 32)

  ! Add Section 6 - Bit-Map Section.
  ibeg = iofst ! Calculate offset for beginning of section 6
  iofst = ibeg + 32 ! leave space for length of section
  call g2_sbytec1(cgrib, six, iofst, 8) ! Store section number (6)
  iofst=iofst + 8
  call g2_sbytec1(cgrib, ibmap, iofst, 8) ! Store Bit Map indicator
  iofst = iofst + 8

  ! Store bitmap, if supplied
  if (ibmap.eq.0) then
     call g2_sbytesc(cgrib, intbmap, iofst, 1, 0, ngrdpts) ! Store BitMap
     iofst=iofst+ngrdpts
  endif

  ! If specifying a previously defined bit-map, make sure
  ! one already exists in the current GRIB message.
  if ((ibmap.eq.254).and.(.not.isprevbmap)) then
     print *, 'addfield: Requested previously defined bitmap, ', &
          ' but one does not exist in the current GRIB message.'
     ierr=8
     return
  endif

  ! Calculate length of section 6 and store it in octets
  ! 1-4 of section 6. Pad to end of octect, if necessary.
  left = 8 - mod(iofst, 8)
  if (left .ne. 8) then
     call g2_sbytec1(cgrib, zero, iofst, left) ! Pad with zeros to fill Octet
     iofst = iofst + left
  endif
  lensec6 = (iofst - ibeg) / 8
  call g2_sbytec1(cgrib, lensec6, ibeg, 32)

  ! Add Section 7 - Data Section.
  ibeg = iofst ! Calculate offset for beginning of section 7
  iofst = ibeg + 32 ! leave space for length of section
  call g2_sbytec1(cgrib, seven, iofst, 8) ! Store section number (7)
  iofst = iofst + 8
  
  ! Store Packed Binary Data values, if non-constant field
  if (lcpack .ne. 0) then
     ioctet = iofst / 8
     cgrib(ioctet + 1:ioctet + lcpack) = cpack(1:lcpack)
     iofst = iofst + (8 * lcpack)
  endif

  ! Calculate length of section 7 and store it in octets
  ! 1-4 of section 7.
  lensec7 = (iofst - ibeg) / 8
  call g2_sbytec1(cgrib, lensec7, ibeg, 32)

  if (allocated(cpack)) deallocate(cpack)

  ! Update current byte total of message in Section 0.
  newlen = lencurr + lensec4 + lensec5 + lensec6 + lensec7
  call g2_sbytec1(cgrib, newlen, 96, 32)

  return
end subroutine addfield

!> Add a [Grid Definition Section (Section
!> 3)](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml)
!> to a GRIB2 message.
!>
!> This routine is used with routines gribcreate(), addlocal(),
!> addfield(), and gribend() to create a complete GRIB2
!> message.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum length (bytes) of array cgrib.
!> @param[in] igds Contains information needed for GRIB Grid
!> Definition Section 3. Must be dimensioned >= 5.
!> - igds(1) Source of grid definition (see [Code Table
!> 3.0](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml)).
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!> definition. Used to define number of points in each row (or column)
!> for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points
!> definition. (See [Code Table
!> 3.11](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!> - igds(5) Grid Definition Template Number (See [Code Table
!> 3.1](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!> @param[in] igdstmpl Contains the data values for the Grid
!> Definition Template specified by igds(5). This should have the
!> length of mapgridlen of the selected grid template.
!> @param[in] igdstmplen Max dimension of igdstmpl. This must be at
!> least as large as the mapgridlen of the chosen template.
!> @param[in] ideflist (Used if igds(3) .ne. 0). This array contains
!> the number of grid points contained in each row (or column).
!> @param[in] idefnum The number of entries in array ideflist. This
!> is the number of rows (or columns) for which optional grid points
!> are defined.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 GRIB message was not initialized. Need to call routine
!> gribcreate first.
!> - 2 GRIB message already complete. Cannot add new section.
!> - 3 Sum of Section byte counts doesn't add to total byte count.
!> - 4 Previous Section was not 1, 2 or 7.
!> - 5 Could not find requested Grid Definition Template.
!>
!> @author Stephen Gilbert @date 2000-05-01
subroutine addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
     ideflist, idefnum, ierr)
  use gridtemplates
  implicit none

  character(len = 1), intent(inout) :: cgrib(lcgrib)
  integer, intent(in) :: igds(*), igdstmpl(*), ideflist(idefnum)
  integer, intent(in) :: lcgrib, idefnum, igdstmplen
  integer, intent(out) :: ierr

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4):: ctemp
  integer:: mapgrid(igdstmplen)
  integer, parameter :: ONE = 1, THREE = 3
  integer lensec3, iofst, ibeg, lencurr, len, mapgridlen
  logical needext
  integer :: i, ilen, iret, isecnum, nbits

  interface
     subroutine g2_gbytec(in, iout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: iout(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
     subroutine g2_gbytec81(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
       integer (kind = 8) :: iout(1)
     end subroutine g2_gbytec81
     subroutine g2_sbytec(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec
     subroutine g2_sbytec1(out, in, iskip, nbits)
       character*1, intent(inout) :: out(*)
       integer, intent(in) :: in
       integer, intent(in) :: iskip, nbits
     end subroutine g2_sbytec1
  end interface

  ierr = 0

#ifdef LOGGING
  print *, 'addgrid lcgrib ', lcgrib, ' igdstmplen ', igdstmplen, ' idefnum ', idefnum
#endif

  ! Check to see if beginning of GRIB message exists.
  do i = 1, 4
     if (cgrib(i) /= grib(i:i)) then
        print *, 'addgrid: GRIB not found in given message.'
        print *, 'addgrid: Call to routine gribcreate required', &
             ' to initialize GRIB messge.'
10      format('"', 4A1, '" /= "GRIB"')
        print 10, cgrib(1:4)
        ierr = 1
        return
     endif
  enddo

  ! Get current length of GRIB message.
  call g2_gbytec1(cgrib, lencurr, 96, 32)

  ! Check to see if GRIB message is already complete.
  ctemp = cgrib(lencurr - 3) // cgrib(lencurr - 2) // cgrib(lencurr &
       - 1) // cgrib(lencurr)
  if (ctemp .eq. c7777) then
     print *, 'addgrid: GRIB message already complete.  Cannot', &
          ' add new section.'
     ierr = 2
     return
  endif

  ! Loop through all current sections of the GRIB message to find the
  ! last section number.
  len = 16                  ! length of Section 0
  do
     ! Get length and section number of next section.
     iofst = len * 8
     call g2_gbytec1(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec1(cgrib, isecnum, iofst, 8)
     len = len + ilen

     ! Exit loop if last section reached.
     if (len .eq. lencurr) exit

     ! If byte count for each section doesn't match current
     ! total length, then there is a problem.
     if (len .gt. lencurr) then
        print *, 'addgrid: Section byte counts don''t add to total.'
        print *, 'addgrid: Sum of section byte counts = ', len
        print *, 'addgrid: Total byte count in Section 0 = ', lencurr
        ierr = 3
        return
     endif
  enddo

  ! Section 3 can only be added after sections 1, 2 and 7.
  if ((isecnum .ne. 1) .and. (isecnum .ne. 2) .and. &
       (isecnum .ne. 7)) then
     print *, 'addgrid: Section 3 can only be added after Section',  &
          ' 1, 2 or 7.'
     print *, 'addgrid: Section ', isecnum, &
          ' was the last found in given GRIB message.'
     ierr = 4
     return
  endif

  ! Add Section 3  - Grid Definition Section.
  ibeg = lencurr * 8        !   Calculate offset for beginning of section 3
  iofst = ibeg + 32         !   leave space for length of section
  call g2_sbytec1(cgrib, THREE, iofst, 8) ! Store section number (3)
  iofst = iofst + 8
  call g2_sbytec(cgrib, igds(1), iofst, 8) ! Store source of Grid def.
  iofst = iofst + 8
  call g2_sbytec(cgrib, igds(2), iofst, 32) ! Store number of data pts.
  iofst = iofst + 32
  call g2_sbytec(cgrib, igds(3), iofst, 8) ! Store number of extra octets.
  iofst = iofst + 8
  call g2_sbytec(cgrib, igds(4), iofst, 8) ! Store interp. of extra octets.
  iofst = iofst + 8

  ! If Octet 6 is not equal to zero, Grid Definition Template may not
  ! be supplied.
  if (igds(1) .eq. 0) then
     call g2_sbytec(cgrib, igds(5), iofst, 16) ! Store Grid Def Template num.
  else
     call g2_sbytec1(cgrib, 65535, iofst, 16) ! Store missing value as Grid Def Template num.
  endif
  iofst = iofst + 16

  ! Get Grid Definition Template.
  if (igds(1) .eq. 0) then
     call getgridtemplate(igds(5), mapgridlen, mapgrid, needext, &
          iret)
     if (iret .ne. 0) then
        ierr = 5
        return
     endif

     ! Extend the Grid Definition Template, if necessary. The number
     ! of values in a specific template may vary depending on data
     ! specified in the "static" part of the template.
     if (needext) then
        call extgridtemplate(igds(5), igdstmpl, mapgridlen, &
             mapgrid)
     endif
  else
     mapgridlen = 0
  endif

  ! Pack up each input value in array igdstmpl into the the
  ! appropriate number of octets, which are specified in corresponding
  ! entries in array mapgrid.
  do i = 1, mapgridlen
     nbits = iabs(mapgrid(i)) * 8
     if ((mapgrid(i) .ge. 0) .or. (igdstmpl(i) .ge. 0)) then
        call g2_sbytec(cgrib, igdstmpl(i), iofst, nbits)
     else
        call g2_sbytec1(cgrib, ONE, iofst, 1)
        call g2_sbytec1(cgrib, iabs(igdstmpl(i)), iofst + 1, nbits &
             - 1)
     endif
     iofst = iofst + nbits
  enddo

  ! If requested, insert optional list of numbers defining number of
  ! points in each row or column. This is used for non regular grids.
  if (igds(3) .ne. 0) then
     nbits = igds(3) * 8
     call g2_sbytesc(cgrib, ideflist, iofst, nbits, 0, idefnum)
     iofst = iofst + (nbits * idefnum)
  endif

  ! Calculate length of section 3 and store it in octets 1-4 of
  ! section 3.
  lensec3 = (iofst - ibeg) / 8
  call g2_sbytec1(cgrib, lensec3, ibeg, 32)


  ! Update current byte total of message in Section 0.
  call g2_sbytec1(cgrib, lencurr + lensec3, 96, 32)
end subroutine addgrid

!> Add a [Local Use Section (Section
!> 2)](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect2.shtml)
!> to a GRIB2 message. This routine is used with routines
!> gribcreate(), addlocal(), addfield(), and gribend() to create a
!> complete GRIB2 message. Subroutine gribcreate must be called first
!> to initialize a new GRIB2 message.
!>
!> The Local Use Section (Section 2) can only follow Section 1 or
!> Section 7 in a GRIB2 message.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum length (bytes) of array cgrib.
!> @param[in] csec2 Character array containing information to be
!> added to Section 2.
!> @param[in] lcsec2 Number of bytes of character array csec2 to be
!> added to Section 2.
!> @param[out] ierr Error return code.
!> - 0 no error
!> - 1 GRIB message was not initialized. Need to call routine gribcreate first.
!> - 2 GRIB message already complete. Cannot add new section.
!> - 3 Sum of Section byte counts doesn't add to total byte count.
!> - 4 Previous Section was not 1 or 7.
!>
!> @author Stephen Gilbert @date 2000-05-01
subroutine addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  implicit none
  
  character(len = 1), intent(inout) :: cgrib(lcgrib)
  character(len = 1), intent(in) :: csec2(lcsec2)
  integer, intent(in) :: lcgrib, lcsec2
  integer, intent(out) :: ierr

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4):: ctemp
  integer, parameter :: two = 2
  integer :: lensec2, iofst, ibeg, lencurr, len
  integer :: ilen, isecnum, istart

  ierr = 0

  ! Check to see if beginning of GRIB message exists.
  ctemp = cgrib(1) // cgrib(2) // cgrib(3) // cgrib(4)
  if (ctemp .ne. grib) then
     print *, 'addlocal: GRIB not found in given message.'
     print *, 'addlocal: Call to routine gribcreate required', &
          ' to initialize GRIB messge.'
     ierr = 1
     return
  endif

  ! Get current length of GRIB message.
  call g2_gbytec1(cgrib, lencurr, 96, 32)

  ! Check to see if GRIB message is already complete
  ctemp = cgrib(lencurr - 3) // cgrib(lencurr - 2) // cgrib(lencurr - 1) // cgrib(lencurr)
  if (ctemp .eq. c7777) then
     print *, 'addlocal: GRIB message already complete.  Cannot add new section.'
     ierr = 2
     return
  endif

  ! Loop through all current sections of the GRIB message to find the
  ! last section number.
  len = 16    ! length of Section 0
  do 
     ! Get section number and length of next section.
     iofst = len * 8
     call g2_gbytec1(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec1(cgrib, isecnum, iofst, 8)
     len = len + ilen
     ! Exit loop if last section reached
     if (len .eq. lencurr) exit
     
     ! If byte count for each section doesn't match current
     ! total length, then there is a problem.
     if (len .gt. lencurr) then
        print *, 'addlocal: Section byte counts don''t add to total.'
        print *, 'addlocal: Sum of section byte counts = ', len
        print *, 'addlocal: Total byte count in Section 0 = ', lencurr
        ierr = 3
        return
     endif
  enddo

  ! Section 2 can only be added after sections 1 and 7.
  if ((isecnum .ne. 1) .and. (isecnum .ne. 7)) then
     print *, 'addlocal: Section 2 can only be added after Section 1 or Section 7.'
     print *, 'addlocal: Section ', isecnum, ' was the last found in given GRIB message.'
     ierr = 4
     return
  endif

  ! Add Section 2  - Local Use Section.
  ibeg = lencurr * 8  !    Calculate offset for beginning of section 2
  iofst = ibeg + 32   !    leave space for length of section
  call g2_sbytec1(cgrib, two, iofst, 8)     ! Store section number (2)
  istart = lencurr + 5
  cgrib(istart + 1:istart + lcsec2) = csec2(1:lcsec2)

  ! Calculate length of section 2 and store it in octets 1-4 of
  ! section 2.
  lensec2 = lcsec2 + 5 !  bytes
  call g2_sbytec1(cgrib, lensec2, ibeg, 32)

  ! Update current byte total of message in Section 0.
  call g2_sbytec1(cgrib, lencurr + lensec2, 96, 32)

end subroutine addlocal

!> Finalize a GRIB2 message after all grids and fields have
!> been added.
!>
!> This subroutine adds the End Section ("7777") to the end of the
!> GRIB message and calculates the length and stores it in the
!> appropriate place in Section 0. This routine is used with routines
!> gribcreate(), addlocal(), addgrid(), and addfield() to create a
!> complete GRIB2 message.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum Length (in bytes) of array cgrib.
!> @param[out] lengrib Length of the final GRIB2 message in bytes.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 GRIB message was not initialized - call routine gribcreate() first.
!> - 2 GRIB message already complete.
!> - 3 Sum of Section byte counts doesn't add to total byte count.
!> - 4 Previous Section was not 7.
!>
!> @author Stephen Gilbert @date 2000-05-02
subroutine gribend(cgrib, lcgrib, lengrib, ierr)
  implicit none

  character(len = 1), intent(inout) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(out) :: lengrib, ierr

  integer ilen, isecnum
  character(len = 4), parameter :: grib = 'GRIB'
  character(len = 1), parameter :: c7777(4) = (/ '7', '7', '7', '7' /)
  character(len = 4):: ctemp
  integer iofst, lencurr, len

  ierr = 0

  ! Check to see if beginning of GRIB message exists.
  ctemp = cgrib(1) // cgrib(2) // cgrib(3) // cgrib(4)
  if (ctemp .ne. grib) then
     print *, 'gribend: GRIB not found in given message.'
     ierr = 1
     return
  endif

  ! Get current length of GRIB message.
  call g2_gbytec1(cgrib, lencurr, 96, 32)

  ! Loop through all current sections of the GRIB message to
  ! find the last section number.
  len = 16                    ! Length of Section 0
  do
     ! Get number and length of next section.
     iofst = len * 8
     call g2_gbytec1(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec1(cgrib, isecnum, iofst, 8)
     len = len + ilen

     ! Exit loop if last section reached.
     if (len .eq. lencurr) exit

     ! If byte count for each section doesn't match current total
     ! length, then there is a problem.
     if (len .gt. lencurr) then
        print *, 'gribend: Section byte counts don''t add ' &
             ,'to total.'
        print *, 'gribend: Sum of section byte counts = ', len
        print *, 'gribend: Total byte count in Section 0 = ', &
             lencurr
        ierr = 3
        return
     endif
  enddo

  ! Can only add End Section (Section 8) after Section 7.
  if (isecnum .ne. 7) then
     print *, 'gribend: Section 8 can only be added after Section 7.'
     print *, 'gribend: Section ', isecnum, &
          ' was the last found in',' given GRIB message.'
     ierr = 4
     return
  endif

  ! Add Section 8  - End Section.
  cgrib(lencurr + 1:lencurr + 4) = c7777

  ! Update current byte total of message in Section 0.
  lengrib = lencurr + 4
  call g2_sbytec1(cgrib, lengrib, 96, 32)
end subroutine gribend
