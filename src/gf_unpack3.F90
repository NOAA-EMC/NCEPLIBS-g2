!>    @file
!>    @brief This subroutine unpacks Section 3 ([Grid Definition
!>    Section]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml)).
!>    @author Stephen Gilbert @date 2000-05-26
!>

!>    This subroutine unpacks Section 3 ([Grid Definition
!>    Section]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml))
!>    starting at octet 6 of that Section.
!>
!>    ### Program History Log
!>    Date | Programmer | Comments
!>    -----|------------|---------
!>    2000-05-26 | Stephen Gilbert | Initial development.
!>    2002-01-24 | Stephen Gilbert | Dynamically allocate arrays and pass pointers.
!>
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning/end(returned) of
!>    Section 3.
!>    @param[out] igds Contains information read from the appropriate
!>    GRIB Grid Definition Section 3 for the field being returned.  Must
!>    be dimensioned >= 5.
!>    - igds(1) Source of grid definition (see [Code Table 3.0]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-0.shtml)).
!>    - igds(2) Number of grid points in the defined grid.
!>    - igds(3) Number of octets needed for each additional grid points
!>    definition. Used to define number of points in each row (or
!>    column) for non-regular grids. = 0, if using regular grid.
!>    - igds(4) Interpretation of list for optional points definition ([Code Table 3.11]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-11.shtml)).
!>    - igds(5) Grid Definition Template Number ([Code Table 3.1]
!>    (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-1.shtml)).
!>    @param[out] igdstmpl Contains the data values for the specified
!>    Grid Definition Template (NN=igds(5)). Each element of this
!>    integer array contains an entry (in the order specified) of Grid
!>    Defintion Template 3.NN. A safe dimension for this array can be
!>    obtained in advance from maxvals(2), which is returned from
!>    subroutine gribinfo().
!>    @param[out] mapgridlen Number of elements in igdstmpl. i.e. number
!>    of entries in Grid Defintion Template 3.NN (NN=igds(5)).
!>    @param[out] ideflist (Used if igds(3) .ne. 0) This array contains
!>    the number of grid points contained in each row (or column). (part
!>    of Section 3) A safe dimension for this array can be obtained in
!>    advance from maxvals(3), which is returned from subroutine
!>    gribinfo().
!>    @param[out] idefnum (Used if igds(3) .ne. 0) The number of entries
!>    in array ideflist.  i.e. number of rows (or columns) for which
!>    optional grid points are defined.
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 5 "GRIB" message contains an undefined Grid Definition Template.
!>    - 6 memory allocation error.
!>
!>    @author Stephen Gilbert @date 2000-05-26
      subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
           mapgridlen, ideflist, idefnum, ierr)

      use gridtemplates
      use re_alloc              !  needed for subroutine realloc

      character(len=1), intent(in) :: cgrib(lcgrib)
      integer, intent(in) :: lcgrib
      integer, intent(inout) :: iofst
      integer, pointer, dimension(:) :: igdstmpl, ideflist
      integer, intent(out) :: igds(5)
      integer, intent(out) :: ierr, idefnum

      integer, allocatable :: mapgrid(:)
      integer :: mapgridlen, ibyttem
      logical needext

      ierr=0
      nullify(igdstmpl, ideflist)

      call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8             ! skip section number

      call g2_gbytec(cgrib, igds(1), iofst, 8) ! Get source of Grid def.
      iofst=iofst+8
      call g2_gbytec(cgrib, igds(2), iofst, 32) ! Get number of grid pts.
      iofst=iofst+32
      call g2_gbytec(cgrib, igds(3), iofst, 8) ! Get num octets for opt. list
      iofst=iofst+8
      call g2_gbytec(cgrib, igds(4), iofst, 8) ! Get interpret. for opt. list
      iofst=iofst+8
      call g2_gbytec(cgrib, igds(5), iofst, 16) ! Get Grid Def Template num.
      iofst=iofst+16

      if (igds(1).eq.0.OR.igds(1).eq.255) then ! FOR ECMWF TEST ONLY
          allocate(mapgrid(lensec))

!         Get Grid Definition Template
          call getgridtemplate(igds(5), mapgridlen, mapgrid, needext, iret)
          if (iret.ne.0) then
              ierr=5
              if (allocated(mapgrid) ) deallocate(mapgrid)
              return
          endif
      else
          mapgridlen=0
          needext=.false.
      endif

!     Unpack each value into array igdstmpl from the the appropriate
!     number of octets, which are specified in corresponding entries in
!     array mapgrid.
      istat=0
      if (mapgridlen.gt.0) allocate(igdstmpl(mapgridlen), stat=istat)
      if (istat.ne.0) then
          ierr=6
          nullify(igdstmpl)
          if (allocated(mapgrid) ) deallocate(mapgrid)
          return
      endif
      ibyttem=0
      do i=1, mapgridlen
          nbits=iabs(mapgrid(i))*8
          if (mapgrid(i).ge.0 ) then
              call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
          else
              call g2_gbytec(cgrib, isign, iofst, 1)
              call g2_gbytec(cgrib, igdstmpl(i), iofst+1, nbits-1)
              if (isign.eq.1) igdstmpl(i)=-igdstmpl(i)
          endif
          iofst=iofst+nbits
          ibyttem=ibyttem+iabs(mapgrid(i))
      enddo

!     Check to see if the Grid Definition Template needs to be extended.
!     The number of values in a specific template may vary depending on
!     data specified in the "static" part of the template.
      if (needext ) then
          call extgridtemplate(igds(5), igdstmpl, newmapgridlen, &
               mapgrid)

!         Unpack the rest of the Grid Definition Template
          call realloc(igdstmpl, mapgridlen, newmapgridlen, istat)
          do i=mapgridlen+1, newmapgridlen
              nbits=iabs(mapgrid(i))*8
              if (mapgrid(i).ge.0 ) then
                  call g2_gbytec(cgrib, igdstmpl(i), iofst, nbits)
              else
                  call g2_gbytec(cgrib, isign, iofst, 1)
                  call g2_gbytec(cgrib, igdstmpl(i), iofst+1, nbits-1)
                  if (isign.eq.1) igdstmpl(i)=-igdstmpl(i)
              endif
              iofst=iofst+nbits
              ibyttem=ibyttem+iabs(mapgrid(i))
          enddo
          mapgridlen=newmapgridlen
      endif
      if (allocated(mapgrid) ) deallocate(mapgrid)

!     Unpack optional list of numbers defining number of points in each
!     row or column, if included. This is used for non regular grids.
      if (igds(3).ne.0 ) then
          nbits=igds(3)*8
          idefnum=(lensec-14-ibyttem)/igds(3)
          istat=0
          if (idefnum.gt.0) allocate(ideflist(idefnum), stat=istat)
          if (istat.ne.0) then
              ierr=6
              nullify(ideflist)
              return
          endif
          call g2_gbytesc(cgrib, ideflist, iofst, nbits, 0, idefnum)
          iofst=iofst+(nbits*idefnum)
      else
          idefnum=0
          nullify(ideflist)
      endif

      return
      end
