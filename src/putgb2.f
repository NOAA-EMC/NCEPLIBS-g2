C>    @file
C>    @brief This subroutine read and unpack sections 6 and 7 from ah
C>    grib2 message.
C>    @author Stephen Gilbert @date 2002-01-11
C>

C>    This subroutine packs a single field into a grib2 message and
C>    writes out that message to the file associated with unit lugb.
C>    note that file/unit lugb should be opened woth a call to
C>    subroutine baopenw before this routine is called.
C>    The information to be packed into the grib field is stored in a
C>    derived type variable, gfld. gfld is of type gribfield, which is
C>    defined in module grib_mod, so users of this routine will need to
C>    include the line "use grib_mod" in their calling routine. Each
C>    component of the gribfield type is described in the input argument
C>    list section below.
C>    
C>    PROGRAM HISTORY LOG:
C>    - 2002-04-22 Stephen Gilbert
C>    - 2005-02-28 Stephen Gilbert Changed dimension of array cgrib to be
C>    a multiple of gfld%ngrdpts instead of gfld%ndpts.
C>    - 2009-03-10Boi Vuong Initialize variable coordlist.
C>    - 2011-06-09Boi Vuong Initialize variable gfld%list_opt.
C>    - 2012-02-28Boi Vuong Initialize variable ilistopt.
C>    
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    file must be opened with baopen or baopenw before calling this
C>    routine.
C>    @param[in] GFLD derived type gribfield (defined in module grib_mod)
C>    (NOTE: See Remarks Section)
C>    - gfld\%version GRIB edition number (currently 2)
C>    - gfld\%discipline Message Discipline (see Code Table 0.0)
C>    - gfld\%idsect Contains the entries in the Identification Section
C>    (Section 1) This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%idsect(1) Identification of originating Centre
C>    (see Common Code Table C-1) 7 US National Weather Service
C>    - gfld\%idsect(2) Identification of originating Sub-centre
C>    - gfld\%idsect(3) GRIB Master Tables Version Number
C>    (see Code Table 1.0) 0 Experimental; 1 Initial operational version number
C>    - gfld\%idsect(4) GRIB Local Tables Version Number (see Code Table 1.1)
C>     - 0 Local tables not used
C>     - 0 1-254 Number of local tables version used
C>    - gfld\%idsect(5) Significance of Reference Time (Code Table 1.2)
C>     - 0 Analysis
C>     - 1 Start of forecast
C>     - 2 Verifying time of forecast
C>     - 3 Observation time.
C>    - gfld\%idsect(6) Year (4 digits)
C>    - gfld\%idsect(7) Month
C>    - gfld\%idsect(8) Day
C>    - gfld\%idsect(9) Hour
C>    - gfld\%idsect(10) Minute
C>    - gfld\%idsect(11) Second
C>    - gfld\%idsect(12) Production status of processed data (see Code
C>    Table 1.3)
C>     - 0 Operational products
C>     - 1 Operational test products
C>     - 2 Research products
C>     - 3 Re-analysis products
C>    - gfld\%idsect(13) Type of processed data (see Code Table 1.4)
C>     - 0 Analysis products
C>     - 1 Forecast products
C>     - 2 Analysis and forecast products
C>     - 3 Control forecast products
C>     - 4 Perturbed forecast products
C>     - 5 Control and perturbed forecast products
C>     - 6 Processed satellite observations
C>     - 7 Processed radar observations
C>    - gfld\%idsectlen Number of elements in gfld\%idsect
C>    - gfld\%local Pointer to character array containing contents
C>    of Local Section 2, if included
C>    - gfld\%locallen length of array gfld\%local
C>    - gfld\%ifldnum field number within GRIB message
C>    - gfld\%griddef Source of grid definition (see Code Table 3.0)
C>     - 0 Specified in Code table 3.1
C>     - 1 Predetermined grid Defined by originating centre
C>    - gfld\%ngrdpts Number of grid points in the defined grid.
C>    Note that the number of actual data values returned from getgb2
C>    (in gfld\%ndpts) may be less than this value if a logical bitmap
C>    is in use with grid points that are being masked out.
C>    - gfld\%numoct_opt Number of octets needed for each additional grid
C>    points definition. Used to define number of points in each row (or
C>    column) for non-regular grids. = 0, if using regular grid.
C>    - gfld\%interp_opt Interpretation of list for optional points
C>    definition.(Code Table 3.11)
C>    - gfld\%igdtnum Grid Definition Template Number (Code Table 3.1)
C>    - gfld\%igdtmpl Contains the data values for the specified Grid
C>    Definition Template (NN=gfld\%igdtnum). Each element of this
C>    integer array contains an entry (in the order specified) of Grid
C>    Defintion Template 3.NN This element is actually a pointer to an
C>    array that holds the data.
C>    - gfld\%igdtlen Number of elements in gfld\%igdtmpl. i.e. number
C>    of entries in Grid Defintion Template 3.NN (NN=gfld\%igdtnum).
C>    - gfld\%list_opt (Used if gfld\%numoct_opt .ne. 0) This array
C>    contains the number of grid points contained in each row (or
C>    column). (part of Section 3) This element is actually a pointer
C>    to an array that holds the data. This pointer is nullified
C>    if gfld\%numoct_opt=0.
C>    - gfld\%num_opt (Used if gfld\%numoct_opt .ne. 0) The number of
C>    entries in array ideflist. i.e. number of rows (or columns) for which
C>    optional grid points are defined. This value is set to zero,
C>    if gfld\%numoct_opt=0.
C>    - gfdl\%ipdtnum Product Definition Template Number (Code Table 4.0)
C>    - gfld\%ipdtmpl Contains the data values for the specified Product
C>    Definition Template (N=gfdl\%ipdtnum). Each element of this integer
C>    array contains an entry (in the order specified) of Product Defintion
C>    Template 4.N. This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%ipdtlen Number of elements in gfld\%ipdtmpl. i.e. number of
C>    entries in Product Defintion Template 4.N (N=gfdl\%ipdtnum).
C>    - gfld\%coord_list Real array containing floating point values
C>    intended to document the vertical discretisation associated to
C>    model data on hybrid coordinate vertical levels.(part of Section 4)
C>    This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%num_coord number of values in array gfld\%coord_list.
C>    - gfld\%ndpts Number of data points unpacked and returned.
C>    Note that this number may be different from the value of
C>    - gfld\%ngrdpts if a logical bitmap is in use with grid points
C>    that are being masked out.
C>    - gfld\%idrtnum Data Representation Template Number (Code Table 5.0)
C>    - gfld\%idrtmpl Contains the data values for the specified Data
C>    Representation Template (N=gfld\%idrtnum). Each element of this
C>    integer array contains an entry (in the order specified) of
C>    Product Defintion Template 5.N. This element is actually a
C>    pointer to an array that holds the data.
C>    - gfld\%idrtlen Number of elements in gfld\%idrtmpl. i.e. number
C>    of entries in Data Representation Template 5.N (N=gfld\%idrtnum).
C>    - gfld\%unpacked logical value indicating whether the bitmap and
C>    data values were unpacked. If false, gfld\%bmap and gfld\%fld
C>    pointers are nullified.
C>    - gfld\%expanded Logical value indicating whether the data field
C>    was expanded to the grid in the case where a bit-map is present.
C>    If true, the data points in gfld\%fld match the grid points and
C>    zeros were inserted at grid points where data was bit-mapped out.
C>    If false, the data values in gfld\%fld were not expanded to the
C>    grid and are just a consecutive array of data points corresponding
C>    to each value of "1" in gfld\%bmap.
C>    - gfld\%ibmap Bitmap indicator (see Code Table 6.0)
C>     - 0 bitmap applies and is included in Section 6.
C>     - 1-253 Predefined bitmap applies
C>     - 254 Previously defined bitmap applies to this field
C>     - 255 Bit map does not apply to this product.
C>    - gfld\%bmap Logical*1 array containing decoded bitmap, if ibmap=0
C>    or ibap=254. Otherwise nullified. This element is actually a
C>    pointer to an array that holds the data.
C>    - gfld\%fld Array of gfld\%ndpts unpacked data points. This element
C>    is actually a pointer to an array that holds the data.
C>    @param[out] IRET integer return code
C>    - 0 all ok.
C>    - 2 Memory allocation error.
C>    - 10 No Section 1 info available.
C>    - 11 No Grid Definition Template info available.
C>    - 12 Missing some required data field info.
C>
C>    @note That derived type gribfield contains pointers to 
C>    many arrays of data. The memory for these arrays is allocated
C>    when the values in the arrays are set, to help minimize problems
C>    with array overloading. Because of this users are encouraged to
C>    free up this memory, when it is no longer needed, by an explicit
C>    call to subroutine gf_free().
C>
C>    @author Stephen Gilbert @date 2002-04-22
C>

C-----------------------------------------------------------------------
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ALLOCATE ARRAY FOR GRIB2 FIELD
      lcgrib=gfld%ngrdpts*4
      allocate(cgrib(lcgrib),stat=is)
      if ( is.ne.0 ) then
         print *,'putgb2: cannot allocate memory. ',is
         iret=2
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE NEW MESSAGE
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD LOCAL USE SECTION TO GRIB2 MESSAGE
      if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
         call addlocal(cgrib,lcgrib,gfld%local,gfld%locallen,ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding local info = ',ierr
         endif
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD GRID TO GRIB2 MESSAGE
      igds(1)=gfld%griddef
      igds(2)=gfld%ngrdpts
      igds(3)=gfld%numoct_opt
      igds(4)=gfld%interp_opt
      igds(5)=gfld%igdtnum
      if ( associated(gfld%igdtmpl) ) then
         call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                ilistopt,gfld%num_opt,ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding grid info = ',ierr
         endif
      else
         print *,'putgb2: No GDT info available. '
         iret=11
         deallocate(cgrib)
         return
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD DATA FIELD TO GRIB2 MESSAGE
      if ( associated(gfld%ipdtmpl).AND.
     &     associated(gfld%idrtmpl).AND.
     &     associated(gfld%fld) ) then
         call addfield(cgrib,lcgrib,gfld%ipdtnum,gfld%ipdtmpl,
     &                 gfld%ipdtlen,coordlist,gfld%num_coord,
     &                 gfld%idrtnum,gfld%idrtmpl,gfld%idrtlen,
     &                 gfld%fld,gfld%ngrdpts,gfld%ibmap,gfld%bmap,
     &                 ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding data field = ',ierr
         endif
      else
         print *,'putgb2: Missing some field info. '
         iret=12
         deallocate(cgrib)
         return
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CLOSE GRIB2 MESSAGE AND WRITE TO FILE
      call gribend(cgrib,lcgrib,lengrib,ierr)
      call wryte(lugb,lengrib,cgrib)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      deallocate(cgrib)
      RETURN
      END
