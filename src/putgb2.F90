!>    @file
!>    @brief This subroutine read and unpack sections 6 and 7 from ah
!>    grib2 message.
!>    @author Stephen Gilbert @date 2002-01-11
!>

!>    This subroutine packs a single field into a grib2 message and
!>    writes out that message to the file associated with unit lugb.
!>    note that file/unit lugb should be opened woth a call to
!>    subroutine baopenw before this routine is called.
!>    The information to be packed into the grib field is stored in a
!>    derived type variable, gfld. gfld is of type gribfield, which is
!>    defined in module grib_mod, so users of this routine will need to
!>    include the line "use grib_mod" in their calling routine. Each
!>    component of the gribfield type is described in the input argument
!>    list section below.
!>
!>    PROGRAM HISTORY LOG:
!>    - 2002-04-22 Stephen Gilbert
!>    - 2005-02-28 Stephen Gilbert Changed dimension of array cgrib to be
!>    a multiple of gfld%ngrdpts instead of gfld%ndpts.
!>    - 2009-03-10Boi Vuong Initialize variable coordlist.
!>    - 2011-06-09Boi Vuong Initialize variable gfld%list_opt.
!>    - 2012-02-28Boi Vuong Initialize variable ilistopt.
!>
!>    @param[in] LUGB integer unit of the unblocked grib data file.
!>    file must be opened with baopen or baopenw before calling this
!>    routine.
!>    @param[in] GFLD derived type gribfield (defined in module grib_mod)
!>    (NOTE: See Remarks Section)
!>    - gfld\%version GRIB edition number (currently 2)
!>    - gfld\%discipline Message Discipline (see Code Table 0.0)
!>    - gfld\%idsect Contains the entries in the Identification Section
!>    (Section 1) This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%idsect(1) Identification of originating Centre
!>    (see Common Code Table C-1) 7 US National Weather Service
!>    - gfld\%idsect(2) Identification of originating Sub-centre
!>    - gfld\%idsect(3) GRIB Master Tables Version Number
!>    (see Code Table 1.0) 0 Experimental; 1 Initial operational version number
!>    - gfld\%idsect(4) GRIB Local Tables Version Number (see Code Table 1.1)
!>     - 0 Local tables not used
!>     - 0 1-254 Number of local tables version used
!>    - gfld\%idsect(5) Significance of Reference Time (Code Table 1.2)
!>     - 0 Analysis
!>     - 1 Start of forecast
!>     - 2 Verifying time of forecast
!>     - 3 Observation time.
!>    - gfld\%idsect(6) Year (4 digits)
!>    - gfld\%idsect(7) Month
!>    - gfld\%idsect(8) Day
!>    - gfld\%idsect(9) Hour
!>    - gfld\%idsect(10) Minute
!>    - gfld\%idsect(11) Second
!>    - gfld\%idsect(12) Production status of processed data (see Code
!>    Table 1.3)
!>     - 0 Operational products
!>     - 1 Operational test products
!>     - 2 Research products
!>     - 3 Re-analysis products
!>    - gfld\%idsect(13) Type of processed data (see Code Table 1.4)
!>     - 0 Analysis products
!>     - 1 Forecast products
!>     - 2 Analysis and forecast products
!>     - 3 Control forecast products
!>     - 4 Perturbed forecast products
!>     - 5 Control and perturbed forecast products
!>     - 6 Processed satellite observations
!>     - 7 Processed radar observations
!>    - gfld\%idsectlen Number of elements in gfld\%idsect
!>    - gfld\%local Pointer to character array containing contents
!>    of Local Section 2, if included
!>    - gfld\%locallen length of array gfld\%local
!>    - gfld\%ifldnum field number within GRIB message
!>    - gfld\%griddef Source of grid definition (see Code Table 3.0)
!>     - 0 Specified in Code table 3.1
!>     - 1 Predetermined grid Defined by originating centre
!>    - gfld\%ngrdpts Number of grid points in the defined grid.
!>    Note that the number of actual data values returned from getgb2
!>    (in gfld\%ndpts) may be less than this value if a logical bitmap
!>    is in use with grid points that are being masked out.
!>    - gfld\%numoct_opt Number of octets needed for each additional grid
!>    points definition. Used to define number of points in each row (or
!>    column) for non-regular grids. = 0, if using regular grid.
!>    - gfld\%interp_opt Interpretation of list for optional points
!>    definition.(Code Table 3.11)
!>    - gfld\%igdtnum Grid Definition Template Number (Code Table 3.1)
!>    - gfld\%igdtmpl Contains the data values for the specified Grid
!>    Definition Template (NN=gfld\%igdtnum). Each element of this
!>    integer array contains an entry (in the order specified) of Grid
!>    Defintion Template 3.NN This element is actually a pointer to an
!>    array that holds the data.
!>    - gfld\%igdtlen Number of elements in gfld\%igdtmpl. i.e. number
!>    of entries in Grid Defintion Template 3.NN (NN=gfld\%igdtnum).
!>    - gfld\%list_opt (Used if gfld\%numoct_opt .ne. 0) This array
!>    contains the number of grid points contained in each row (or
!>    column). (part of Section 3) This element is actually a pointer
!>    to an array that holds the data. This pointer is nullified
!>    if gfld\%numoct_opt=0.
!>    - gfld\%num_opt (Used if gfld\%numoct_opt .ne. 0) The number of
!>    entries in array ideflist. i.e. number of rows (or columns) for which
!>    optional grid points are defined. This value is set to zero,
!>    if gfld\%numoct_opt=0.
!>    - gfdl\%ipdtnum Product Definition Template Number (Code Table 4.0)
!>    - gfld\%ipdtmpl Contains the data values for the specified Product
!>    Definition Template (N=gfdl\%ipdtnum). Each element of this integer
!>    array contains an entry (in the order specified) of Product Defintion
!>    Template 4.N. This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%ipdtlen Number of elements in gfld\%ipdtmpl. i.e. number of
!>    entries in Product Defintion Template 4.N (N=gfdl\%ipdtnum).
!>    - gfld\%coord_list Real array containing floating point values
!>    intended to document the vertical discretisation associated to
!>    model data on hybrid coordinate vertical levels.(part of Section 4)
!>    This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%num_coord number of values in array gfld\%coord_list.
!>    - gfld\%ndpts Number of data points unpacked and returned.
!>    Note that this number may be different from the value of
!>    - gfld\%ngrdpts if a logical bitmap is in use with grid points
!>    that are being masked out.
!>    - gfld\%idrtnum Data Representation Template Number (Code Table 5.0)
!>    - gfld\%idrtmpl Contains the data values for the specified Data
!>    Representation Template (N=gfld\%idrtnum). Each element of this
!>    integer array contains an entry (in the order specified) of
!>    Product Defintion Template 5.N. This element is actually a
!>    pointer to an array that holds the data.
!>    - gfld\%idrtlen Number of elements in gfld\%idrtmpl. i.e. number
!>    of entries in Data Representation Template 5.N (N=gfld\%idrtnum).
!>    - gfld\%unpacked logical value indicating whether the bitmap and
!>    data values were unpacked. If false, gfld\%bmap and gfld\%fld
!>    pointers are nullified.
!>    - gfld\%expanded Logical value indicating whether the data field
!>    was expanded to the grid in the case where a bit-map is present.
!>    If true, the data points in gfld\%fld match the grid points and
!>    zeros were inserted at grid points where data was bit-mapped out.
!>    If false, the data values in gfld\%fld were not expanded to the
!>    grid and are just a consecutive array of data points corresponding
!>    to each value of "1" in gfld\%bmap.
!>    - gfld\%ibmap Bitmap indicator (see Code Table 6.0)
!>     - 0 bitmap applies and is included in Section 6.
!>     - 1-253 Predefined bitmap applies
!>     - 254 Previously defined bitmap applies to this field
!>     - 255 Bit map does not apply to this product.
!>    - gfld\%bmap Logical*1 array containing decoded bitmap, if ibmap=0
!>    or ibap=254. Otherwise nullified. This element is actually a
!>    pointer to an array that holds the data.
!>    - gfld\%fld Array of gfld\%ndpts unpacked data points. This element
!>    is actually a pointer to an array that holds the data.
!>    @param[out] IRET integer return code
!>    - 0 all ok.
!>    - 2 Memory allocation error.
!>    - 10 No Section 1 info available.
!>    - 11 No Grid Definition Template info available.
!>    - 12 Missing some required data field info.
!>
!>    @note That derived type gribfield contains pointers to
!>    many arrays of data. The memory for these arrays is allocated
!>    when the values in the arrays are set, to help minimize problems
!>    with array overloading. Because of this users are encouraged to
!>    free up this memory, when it is no longer needed, by an explicit
!>    call to subroutine gf_free().
!>
!>    @author Stephen Gilbert @date 2002-04-22
SUBROUTINE PUTGB2(LUGB,GFLD,IRET)

  USE GRIB_MOD

  INTEGER,INTENT(IN) :: LUGB
  TYPE(GRIBFIELD),INTENT(IN) :: GFLD
  INTEGER,INTENT(OUT) :: IRET

  CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CGRIB
  integer :: listsec0(2)
  integer :: igds(5)
  real    :: coordlist
  integer :: ilistopt

  listsec0=(/0,2/)
  igds=(/0,0,0,0,0/)
  coordlist=0.0
  ilistopt=0

  !  ALLOCATE ARRAY FOR GRIB2 FIELD
  lcgrib=gfld%ngrdpts*4
  allocate(cgrib(lcgrib),stat=is)
  if ( is.ne.0 ) then
     print *,'putgb2: cannot allocate memory. ',is
     iret=2
  endif

  !  CREATE NEW MESSAGE
  listsec0(1)=gfld%discipline
  listsec0(2)=gfld%version
  if ( associated(gfld%idsect) ) then
     call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,ierr)
     if (ierr.ne.0) then
        write(6,*) 'putgb2: ERROR creating new GRIB2 field = ',ierr
     endif
  else
     print *,'putgb2: No Section 1 info available. '
     iret=10
     deallocate(cgrib)
     return
  endif

  !  ADD LOCAL USE SECTION TO GRIB2 MESSAGE
  if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
     call addlocal(cgrib,lcgrib,gfld%local,gfld%locallen,ierr)
     if (ierr.ne.0) then
        write(6,*) 'putgb2: ERROR adding local info = ',ierr
     endif
  endif

  !  ADD GRID TO GRIB2 MESSAGE
  igds(1)=gfld%griddef
  igds(2)=gfld%ngrdpts
  igds(3)=gfld%numoct_opt
  igds(4)=gfld%interp_opt
  igds(5)=gfld%igdtnum
  if ( associated(gfld%igdtmpl) ) then
     call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen, &
          ilistopt,gfld%num_opt,ierr)
     if (ierr.ne.0) then
        write(6,*) 'putgb2: ERROR adding grid info = ',ierr
     endif
  else
     print *,'putgb2: No GDT info available. '
     iret=11
     deallocate(cgrib)
     return
  endif

  !  ADD DATA FIELD TO GRIB2 MESSAGE
  if ( associated(gfld%ipdtmpl).AND. &
       associated(gfld%idrtmpl).AND. &
       associated(gfld%fld) ) then
     call addfield(cgrib,lcgrib,gfld%ipdtnum,gfld%ipdtmpl, &
          gfld%ipdtlen,coordlist,gfld%num_coord, &
          gfld%idrtnum,gfld%idrtmpl,gfld%idrtlen, &
          gfld%fld,gfld%ngrdpts,gfld%ibmap,gfld%bmap, &
          ierr)
     if (ierr.ne.0) then
        write(6,*) 'putgb2: ERROR adding data field = ',ierr
     endif
  else
     print *,'putgb2: Missing some field info. '
     iret=12
     deallocate(cgrib)
     return
  endif

  !  CLOSE GRIB2 MESSAGE AND WRITE TO FILE
  call gribend(cgrib,lcgrib,lengrib,ierr)
  call wryte(lugb,lengrib,cgrib)

  deallocate(cgrib)
  RETURN
END SUBROUTINE PUTGB2
