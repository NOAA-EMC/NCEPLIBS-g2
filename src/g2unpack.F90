!> @file
!> @brief Unpack Sections of GRIB2 messages.
!> @author Edward Hartnett @date Jan 31, 2024

!> Unpack Section 1 ([Identification Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 1.
!> @param[out] ids Pointer to integer array containing information
!> read from Section 1, the Identification section.
!> - ids(1) Identification of originating Centre ([Table 0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html)).
!> - ids(2) Identification of originating Sub-centre.([Table C]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html)).      ).
!> - ids(3) GRIB Master Tables Version Number ([Code Table 1.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml)).
!> - ids(4) GRIB Local Tables Version Number ([Code Table 1.1]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml)).
!> - ids(5) Significance of Reference Time ([Code Table 1.2]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-2.shtml)).
!> - ids(6) Year (4 digits).
!> - ids(7) Month
!> - ids(8) Day
!> - ids(9) Hour
!> - ids(10) Minute
!> - ids(11) Second
!> - ids(12) Production status of processed data ([Code Table 1.3]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml)).
!> - ids(13) Type of processed data ([Code Table 1.4]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml)).
!> @param[out] idslen Number of elements in ids.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack1(cgrib, lcgrib, iofst, ids, idslen, ierr)
  implicit none

  character(len=1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, pointer, dimension(:) :: ids
  integer, intent(out) :: ierr, idslen
  integer, dimension(:) :: mapid(13)
  integer :: i, istat, lensec, nbits

  data mapid /2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1/

  ierr = 0
  idslen = 13
  nullify(ids)

  call g2_gbytec(cgrib, lensec, iofst, 32)        ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8     ! skip section number

  ! Unpack each value into array ids from the the appropriate number
  ! of octets, which are specified in corresponding entries in array
  ! mapid.
  istat = 0
  allocate(ids(idslen), stat = istat)
  if (istat .ne. 0) then
     ierr = 6
     nullify(ids)
     return
  endif

  do i = 1, idslen
     nbits = mapid(i) * 8
     call g2_gbytec(cgrib, ids(i), iofst, nbits)
     iofst = iofst + nbits
  enddo
end subroutine gf_unpack1

!> Unpack Section 2 ([Local Use Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect2.shtml))
!> of a GRIB2 message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end (returned) of
!> Section 2.
!> @param[out] lencsec2 Length (in octets) of Local Use data.
!> @param[out] csec2 Pointer to a character*1 array containing local
!> use data.  Caller should pass in an unassociated pointer, and must
!> free memory allocated by this subroutine by calling deallocate on
!> this pointer.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 2 Array passed is not section 2.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2002-04-09
subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, intent(out) :: lencsec2
  integer, intent(out) :: ierr
  character(len = 1), pointer, dimension(:) :: csec2
  integer :: lensec, istat, isecnum, ipos

  ierr = 0
  lencsec2 = 0
  nullify(csec2)

  call g2_gbytec(cgrib, lensec, iofst, 32)        ! Get Length of Section
  iofst = iofst + 32    
  lencsec2 = lensec-5
  call g2_gbytec(cgrib, isecnum, iofst, 8)         ! Get Section Number
  iofst = iofst + 8     
  ipos = (iofst / 8) + 1

  if (isecnum .ne. 2) then
     ierr = 6
     print *, 'gf_unpack2: Not Section 2 data. '
     return
  endif

  allocate(csec2(lencsec2), stat = istat)
  if (istat .ne. 0) then
     ierr = 6
     nullify(csec2)
     return
  endif

  csec2(1:lencsec2) = cgrib(ipos:ipos + lencsec2 - 1)
  iofst = iofst + (lencsec2 * 8)

end subroutine gf_unpack2

!> Unpack Section 3 ([Grid Definition Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 3.
!> @param[out] igds Contains information read from the appropriate
!> GRIB Grid Definition Section 3 for the field being returned.  Must
!> be dimensioned >= 5.
!> - igds(1) Source of grid definition (see [Code Table 3.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml)).
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!> definition. Used to define number of points in each row (or
!> column) for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points definition ([Code Table 3.11]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!> - igds(5) Grid Definition Template Number ([Code Table 3.1]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!> @param[out] igdstmpl Contains the data values for the specified
!> Grid Definition Template (NN=igds(5)). Each element of this
!> integer array contains an entry (in the order specified) of Grid
!> Defintion Template 3.NN. A safe dimension for this array can be
!> obtained in advance from maxvals(2), which is returned from
!> subroutine gribinfo().
!> @param[out] mapgridlen Number of elements in igdstmpl. i.e. number
!> of entries in Grid Defintion Template 3.NN (NN=igds(5)).
!> @param[out] ideflist (Used if igds(3) .ne. 0) This array contains
!> the number of grid points contained in each row (or column). (part
!> of Section 3) A safe dimension for this array can be obtained in
!> advance from maxvals(3), which is returned from subroutine
!> gribinfo().
!> @param[out] idefnum (Used if igds(3) .ne. 0) The number of entries
!> in array ideflist.  i.e. number of rows (or columns) for which
!> optional grid points are defined.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 5 "GRIB" message contains an undefined Grid Definition Template.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
     mapgridlen, ideflist, idefnum, ierr)

  use gridtemplates
  use re_alloc              !  needed for subroutine realloc
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, pointer, dimension(:) :: igdstmpl, ideflist
  integer, intent(out) :: igds(5)
  integer, intent(out) :: ierr, idefnum

  integer, allocatable :: mapgrid(:)
  integer, intent(out) :: mapgridlen
  integer :: ibyttem
  logical needext
  integer :: lensec, istat, i, nbits, isign, newmapgridlen, iret

  ierr = 0
  nullify(igdstmpl, ideflist)

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

  if (igds(1) .eq. 0 .OR. igds(1) .eq. 255) then ! FOR ECMWF TEST ONLY
     allocate(mapgrid(lensec))

     !         Get Grid Definition Template
     call getgridtemplate(igds(5), mapgridlen, mapgrid, needext, iret)
     if (iret .ne. 0) then
        ierr = 5
        if (allocated(mapgrid)) deallocate(mapgrid)
        return
     endif
  else
     mapgridlen = 0
     needext = .false.
  endif

  !     Unpack each value into array igdstmpl from the the appropriate
  !     number of octets, which are specified in corresponding entries in
  !     array mapgrid.
  istat = 0
  if (mapgridlen .gt. 0) allocate(igdstmpl(mapgridlen), stat = istat)
  if (istat .ne. 0) then
     ierr = 6
     nullify(igdstmpl)
     if (allocated(mapgrid)) deallocate(mapgrid)
     return
  endif
  ibyttem = 0
  do i = 1, mapgridlen
     nbits = iabs(mapgrid(i)) * 8
     if (mapgrid(i) .ge. 0) then
        call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, igdstmpl(i), iofst + 1, nbits - 1)
        if (isign .eq. 1) igdstmpl(i) = -igdstmpl(i)
     endif
     iofst = iofst + nbits
     ibyttem = ibyttem + iabs(mapgrid(i))
  enddo

  !     Check to see if the Grid Definition Template needs to be extended.
  !     The number of values in a specific template may vary depending on
  !     data specified in the "static" part of the template.
  if (needext) then
     call extgridtemplate(igds(5), igdstmpl, newmapgridlen, &
          mapgrid)

     ! Unpack the rest of the Grid Definition Template.
     call realloc(igdstmpl, mapgridlen, newmapgridlen, istat)
     do i = mapgridlen + 1, newmapgridlen
        nbits = iabs(mapgrid(i)) * 8
        if (mapgrid(i) .ge. 0) then
           call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
        else
           call g2_gbytec(cgrib, isign, iofst, 1)
           call g2_gbytec(cgrib, igdstmpl(i), iofst + 1, nbits - 1)
           if (isign.eq.1) igdstmpl(i) = -igdstmpl(i)
        endif
        iofst = iofst + nbits
        ibyttem = ibyttem + iabs(mapgrid(i))
     enddo
     mapgridlen = newmapgridlen
  endif
  if (allocated(mapgrid)) deallocate(mapgrid)

  ! Unpack optional list of numbers defining number of points in each
  ! row or column, if included. This is used for non regular grids.
  if (igds(3) .ne. 0) then
     nbits = igds(3) * 8
     idefnum = (lensec - 14 - ibyttem) / igds(3)
     istat = 0
     if (idefnum .gt. 0) allocate(ideflist(idefnum), stat = istat)
     if (istat .ne. 0) then
        ierr = 6
        nullify(ideflist)
        return
     endif
     call g2_gbytesc(cgrib, ideflist, iofst, nbits, 0, idefnum)
     iofst = iofst + (nbits * idefnum)
  else
     idefnum = 0
     nullify(ideflist)
  endif
end subroutine gf_unpack3

!> Unpack Section 4 ([Product Definition Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect4.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 4.
!> @param[out] ipdsnum Product Definition Template Number ([Code Table 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[out] ipdstmpl Contains the data values for the
!> Product Definition Template specified by ipdsnum. A safe dimension for this array
!> can be obtained in advance from maxvals(4), which is returned from
!> subroutine gribinfo.
!> @param[out] mappdslen Number of elements in ipdstmpl. i.e. number
!> of entries in Product Defintion Template specified by ipdsnum.
!> @param[out] coordlist Pointer to real array containing floating
!> point values intended to document the vertical discretisation
!> associated to model data on hybrid coordinate vertical
!> levels (part of Section 4). Must be deallocated by caller.
!> @param[out] numcoord Number of values in array coordlist.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 5 "GRIB" message contains an undefined Grid Definition Template.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
     mappdslen, coordlist, numcoord, ierr)
  use pdstemplates
  use re_alloc              !  needed for subroutine realloc
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  real, pointer, dimension(:) :: coordlist
  integer, pointer, dimension(:) :: ipdstmpl
  integer, intent(out) :: ipdsnum
  integer, intent(out) :: ierr, numcoord

  real(4), allocatable :: coordieee(:)
  integer, allocatable :: mappds(:)
  integer :: mappdslen
  logical needext
  integer :: lensec, nbits, newmappdslen
  integer :: istat1, istat, isign, iret, i

  ierr = 0
  nullify(ipdstmpl, coordlist)

  ! Get Length of Section.
  call g2_gbytec(cgrib, lensec, iofst, 32)
  iofst = iofst + 32
  iofst = iofst + 8     ! skip section number
  allocate(mappds(lensec))

  ! Get num of coordinate values.
  call g2_gbytec(cgrib, numcoord, iofst, 16)
  iofst = iofst + 16
  ! Get Prod. Def Template num.
  call g2_gbytec(cgrib, ipdsnum, iofst, 16)
  iofst = iofst + 16
  ! Get Product Definition Template.
  call getpdstemplate(ipdsnum, mappdslen, mappds, needext, iret)
  if (iret.ne.0) then
     ierr = 5
     if (allocated(mappds)) deallocate(mappds)
     return
  endif

  ! Unpack each value into array ipdstmpl from the the appropriate
  ! number of octets, which are specified in corresponding entries in
  ! array mappds.
  istat = 0
  if (mappdslen.gt.0) allocate(ipdstmpl(mappdslen), stat = istat)
  if (istat.ne.0) then
     ierr = 6
     nullify(ipdstmpl)
     if (allocated(mappds)) deallocate(mappds)
     return
  endif
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

  ! Check to see if the Product Definition Template needs to be
  ! extended. The number of values in a specific template may vary
  ! depending on data specified in the "static" part of the template.
  if (needext) then
     call extpdstemplate(ipdsnum, ipdstmpl, newmappdslen, mappds)
     call realloc(ipdstmpl, mappdslen, newmappdslen, istat)
     ! Unpack the rest of the Product Definition Template.
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
  if (allocated(mappds)) deallocate(mappds)

  ! Get Optional list of vertical coordinate values
  ! after the Product Definition Template, if necessary.
  nullify(coordlist)
  if (numcoord .ne. 0) then
     allocate (coordieee(numcoord), stat = istat1)
     allocate(coordlist(numcoord), stat = istat)
     if ((istat1 + istat).ne.0) then
        ierr = 6
        nullify(coordlist)
        if (allocated(coordieee)) deallocate(coordieee)
        return
     endif
     call g2_gbytescr(cgrib, coordieee, iofst, 32, 0, numcoord)
     call rdieee(coordieee, coordlist, numcoord)
     deallocate (coordieee)
     iofst = iofst + (32 * numcoord)
  endif
end subroutine gf_unpack4

!> Unpack Section 5 ([Data Representation Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect5.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 5.
!> @param[out] ndpts Number of data points unpacked and returned.
!> @param[out] idrsnum Data Representation Template Number ([Code Table 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[out] idrstmpl Contains the data values for the specified
!> Data Representation Template (N = idrsnum). Each element of this
!> integer array contains an entry (in the order specified) of
!> Product Defintion Template 5.N A safe dimension for this array can
!> be obtained in advance from maxvals(6), which is returned from
!> subroutine gribinfo.
!> @param[out] mapdrslen Number of elements in idrstmpl. i.e. number
!> of entries in Data Representation Template 5.N (N = idrsnum).
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 6 memory allocation error.
!> - 7 "GRIB" message contains an undefined Grid Definition Template.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum, idrstmpl, &
     mapdrslen, ierr)
  use drstemplates
  use re_alloc              !  needed for subroutine realloc
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, intent(out) :: ndpts, idrsnum
  integer, pointer, dimension(:) :: idrstmpl
  integer, intent(out) :: ierr

  integer, allocatable :: mapdrs(:)
  integer :: mapdrslen
  logical needext
  integer :: newmapdrslen, nbits, istat, isign, lensec, iret, i

  ierr = 0
  nullify(idrstmpl)

  call g2_gbytec(cgrib, lensec, iofst, 32)        ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8     ! skip section number
  allocate(mapdrs(lensec))

  ! Get num of data points.
  call g2_gbytec(cgrib, ndpts, iofst, 32)
  iofst = iofst + 32
  ! Get Data Rep Template Num.
  call g2_gbytec(cgrib, idrsnum, iofst, 16)
  iofst = iofst + 16
  ! Gen Data Representation Template.
  call getdrstemplate(idrsnum, mapdrslen, mapdrs, needext, iret)
  if (iret .ne. 0) then
     ierr = 7
     if (allocated(mapdrs)) deallocate(mapdrs)
     return
  endif

  ! Unpack each value into array ipdstmpl from the the appropriate
  ! number of octets, which are specified in corresponding entries in
  ! array mappds.
  istat = 0
  if (mapdrslen .gt. 0) allocate(idrstmpl(mapdrslen), stat = istat)
  if (istat .ne. 0) then
     ierr = 6
     nullify(idrstmpl)
     if (allocated(mapdrs)) deallocate(mapdrs)
     return
  endif
  do i = 1, mapdrslen
     nbits = iabs(mapdrs(i)) * 8
     if (mapdrs(i) .ge. 0) then
        call g2_gbytec(cgrib, idrstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, idrstmpl(i), iofst + 1, nbits-1)
        if (isign .eq. 1) idrstmpl(i) = -idrstmpl(i)
     endif
     iofst = iofst + nbits
  enddo

  ! Check to see if the Data Representation Template needs to be
  ! extended. The number of values in a specific template may vary
  ! depending on data specified in the "static" part of the template.
  if (needext) then
     call extdrstemplate(idrsnum, idrstmpl, newmapdrslen, mapdrs)
     call realloc(idrstmpl, mapdrslen, newmapdrslen, istat)

     ! Unpack the rest of the Data Representation Template.
     do i = mapdrslen + 1, newmapdrslen
        nbits = iabs(mapdrs(i)) * 8
        if (mapdrs(i) .ge. 0) then
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

end subroutine gf_unpack5

!> Unpack Section 6 ([Bit-Map Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect6.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of Section 6.
!> @param[in] ngpts Number of grid points specified in the bit-map.
!> @param[out] ibmap Bitmap indicator (see Code Table 6.0)
!> - 0 bitmap applies and is included in Section 6.
!> - 1-253 Predefined bitmap applies.
!> - 254 Previously defined bitmap applies to this field.
!> - 255 Bit map does not apply to this product.
!> @param[out] bmap Logical*1 array containing decoded bitmap. (if ibmap=0)
!> The dimension of this array can be obtained in advance from maxvals(7),
!> which is returned from subroutine gribinfo().
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 4 Unrecognized pre-defined bit-map.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack6(cgrib, lcgrib, iofst, ngpts, ibmap, bmap, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ngpts
  integer, intent(inout) :: iofst
  integer, intent(out) :: ibmap
  integer, intent(out) :: ierr

  logical*1, pointer, dimension(:) :: bmap
  integer :: intbmap(ngpts)
  integer :: istat, j

  ierr = 0
  nullify(bmap)

  iofst = iofst + 32    ! Skip Length of Section.
  iofst = iofst + 8     ! Skip section number.

  call g2_gbytec(cgrib, ibmap, iofst, 8)    ! Get bit-map indicator.
  iofst = iofst + 8

  if (ibmap .eq. 0) then               ! Unpack bitmap
     istat = 0
     if (ngpts .gt. 0) allocate(bmap(ngpts), stat = istat)
     if (istat .ne. 0) then
        ierr = 6
        nullify(bmap)
        return
     endif
     call g2_gbytesc(cgrib, intbmap, iofst, 1, 0, ngpts)
     iofst = iofst + ngpts
     do j = 1, ngpts
        bmap(j) = .true.
        if (intbmap(j) .eq. 0) bmap(j) = .false.
     enddo
  endif

end subroutine gf_unpack6

!> Unpack Section 7 ([Data Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect7.shtml))
!> of a GRIB2 message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 7.
!> @param[in] igdsnum Grid Definition Template Number (Code Table
!> 3.0) (Only required to unpack DRT 5.51).
!> @param[in] igdstmpl Pointer to an integer array containing the
!> data values for the specified Grid Definition. Template
!> (N = igdsnum). Each element of this integer array contains an entry
!> (in the order specified) of Grid Definition Template 3.N (Only
!> required to unpack DRT 5.51).
!> @param[in] idrsnum Data Representation Template Number ([Code Table
!> 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[in] idrstmpl Pointer to an integer array containing the data
!> values for the Data Representation Template specified by idrsnum. A
!> safe dimension for this array can be obtained in advance from
!> maxvals(6), which is returned from subroutine gribinfo.
!> @param[in] ndpts Number of data points unpacked and returned.
!> @param[out] fld Pointer to a real array containing the unpacked
!> data field.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 4 Unrecognized Data Representation Template.
!> - 5 One of GDT 3.50 through 3.53 required to unpack DRT 5.51.
!> - 6 memory allocation error.
!> - 7 corrupt section 7.
!>
!> @author Stephen Gilbert @date 2002-01-24
subroutine gf_unpack7(cgrib, lcgrib, iofst, igdsnum, igdstmpl,  &
     idrsnum, idrstmpl, ndpts, fld, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ndpts, igdsnum, idrsnum
  integer, intent(inout) :: iofst
  integer, pointer, dimension(:) :: igdstmpl, idrstmpl
  integer, intent(out) :: ierr
  real, pointer, dimension(:) :: fld
  integer :: ier,  ipos,  istat,  lensec
  real (kind = 4) :: ieee(1)
  real :: tmpfld(1)

  ierr = 0
  nullify(fld)

  call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
  iofst = iofst + 32
  iofst = iofst + 8 ! skip section number

  ipos = (iofst/8) + 1
  istat = 0
  allocate(fld(ndpts), stat = istat)
  if (istat.ne.0) then
     ierr = 6
     return
  endif

  if (idrsnum .eq. 0) then
     call simunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts, fld)
  elseif (idrsnum.eq.2.or.idrsnum.eq.3) then
     call comunpack(cgrib(ipos), lensec-5, lensec, idrsnum, idrstmpl, ndpts, fld, ier)
     if (ier .ne. 0) then
        ierr = 7
        return
     endif
  elseif (idrsnum .eq. 50) then ! Spectral simple
     call simunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts-1, fld(2))
     ieee = transfer(idrstmpl(5), ieee, 1)
     call rdieee(ieee, tmpfld, 1)
     fld(1) = tmpfld(1)
  elseif (idrsnum .eq. 51) then ! Spectral complex
     if (igdsnum.ge.50.AND.igdsnum.le.53) then
        call specunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts,  &
             igdstmpl(1), igdstmpl(2), igdstmpl(3), fld)
     else
        print *, 'gf_unpack7: Cannot use GDT 3.', igdsnum, ' to unpack Data Section 5.51.'
        ierr = 5
        nullify(fld)
        return
     endif
  elseif (idrsnum .eq. 40 .OR. idrsnum .eq. 40000) then
     call jpcunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts, fld)
  elseif (idrsnum .eq. 41 .OR. idrsnum .eq. 40010) then
     call pngunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts, fld)
#ifdef USE_AEC
  elseif (idrsnum .eq. 42) then
     call aecunpack(cgrib(ipos), lensec-5, idrstmpl, ndpts, fld)
#endif /* USE_AEC */
  else
     print *, 'gf_unpack7: Data Representation Template ', idrsnum, ' not yet implemented.'
     ierr = 4
     nullify(fld)
     return
  endif

  iofst = iofst + (8 * lensec)

end subroutine gf_unpack7
