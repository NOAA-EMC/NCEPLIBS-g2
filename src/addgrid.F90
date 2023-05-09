!> @file
!> @brief Pack up a Grid Definition Section (Section 3) and add it to
!> a GRIB2 message.
!> @author Stephen Gilbert @date 2000-05-01

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
  call g2_gbytec(cgrib, lencurr, 96, 32)

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
     call g2_gbytec(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8)
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
  call g2_sbytec(cgrib, THREE, iofst, 8) ! Store section number (3)
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
     call g2_sbytec(cgrib, 65535, iofst, 16) ! Store missing value as Grid Def Template num.
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
        call g2_sbytec(cgrib, ONE, iofst, 1)
        call g2_sbytec(cgrib, iabs(igdstmpl(i)), iofst + 1, nbits &
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
  call g2_sbytec(cgrib, lensec3, ibeg, 32)


  ! Update current byte total of message in Section 0.
  call g2_sbytec(cgrib, lencurr + lensec3, 96, 32)
end subroutine addgrid
