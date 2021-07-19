C>    @file
C>    @brief This subroutine read and unpack sections 6 and 7 from ah
C>    grib2 message.
C>    @author Stephen Gilbert @date 2002-01-11
C>

C>    This subroutine read and unpack sections 6 and 7 from a grib2
C>    message. It assumes that the "metadata" for this field already
C>    exists in derived type gribfield. Specifically, it requires
C>    gfld\%ibmap, gfld\%ngrdpts, gfld\%idrtnum, gfld\%idrtmpl, and
C>    gfld\%ndpts. It decoded information for the selected grib field and
C>    returned it in a derived type variable, gfld. gfld is of type
C>    gribfield, which is defined in module grib_mod, so users of this
C>    routine will need to include the line "use grib_mod" in their
C>    calling routine. Each component of the gribfield type is described
C>    in the output argument list section below.
C>
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    @param[in] CINDEX index record of the grib field (see docblock of
C>    subroutine ixgb2 for description of an index record.)
C>    @param[out] GFLD derived type gribfield (defined in module grib_mod)
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
C>    0 Local tables not used; 1-254 Number of local tables version used
C>    - gfld\%idsect(5) Significance of Reference Time (Code Table 1.2)
C>    0 Analysis; 1 Start of forecast; 2 Verifying time of forecast;
C>    3 Observation time.
C>    - gfld\%idsect(6) Year (4 digits)
C>    - gfld\%idsect(7) Month
C>    - gfld\%idsect(8) Day
C>    - gfld\%idsect(9) Hour
C>    - gfld\%idsect(10) Minute
C>    - gfld\%idsect(11) Second
C>    - gfld\%idsect(12) Production status of processed data (see Code
C>    Table 1.3)
C>    0 Operational products; 1 Operational test products;
C>    2 Research products; 3 Re-analysis products.
C>    - gfld\%idsect(13) Type of processed data (see Code Table 1.4)
C>    0 Analysis products
C>    1 Forecast products
C>    2 Analysis and forecast products
C>    3 Control forecast products
C>    4 Perturbed forecast products
C>    5 Control and perturbed forecast products
C>    6 Processed satellite observations
C>    7 Processed radar observations
C>    - gfld\%idsectlen Number of elements in gfld\%idsect
C>    - gfld\%local Pointer to character array containing contents
C>    of Local Section 2, if included
C>    - gfld\%locallen length of array gfld\%local
C>    - gfld\%ifldnum field number within GRIB message
C>    - gfld\%griddef Source of grid definition (see Code Table 3.0)
C>    0 Specified in Code table 3.1
C>    1 Predetermined grid Defined by originating centre
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
C>    0 bitmap applies and is included in Section 6.
C>    1-253 Predefined bitmap applies
C>    254 Previously defined bitmap applies to this field
C>    255 Bit map does not apply to this product.
C>    - gfld\%bmap Logical*1 array containing decoded bitmap, if ibmap=0
C>    or ibap=254. Otherwise nullified. This element is actually a
C>    pointer to an array that holds the data.
C>    - gfld\%fld Array of gfld\%ndpts unpacked data points. This element
C>    is actually a pointer to an array that holds the data.
C>    @param[out] IRET integer return code
C>    - 0 all ok
C>    - 97 error reading grib file
C>    - other gf_getfld grib2 unpacker return code
C>
C>    @note do not engage the same logical unit from more than one
C>    processor. This subprogram is intended for private use by getgb2
C>    routines only. Note that derived type gribfield contains pointers
C>    to many arrays of data. The memory for these arrays is allocated
C>    when the values in the arrays are set, to help minimize problems
C>    with array overloading. Because of this users are encouraged to
C>    free up this memory, when it is no longer needed, by an explicit
C>    call to subroutine gf_free.
C>
C>    @author Stephen Gilbert @date 2002-01-11
C>

C-----------------------------------------------------------------------
      SUBROUTINE GETGB2R(LUGB,CINDEX,GFLD,IRET)

      USE GRIB_MOD

      INTEGER,INTENT(IN) :: LUGB
      CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
      INTEGER,INTENT(OUT) :: IRET
      TYPE(GRIBFIELD) :: GFLD

      INTEGER :: LSKIP,SKIP6,SKIP7
      CHARACTER(LEN=1):: CSIZE(4)
      CHARACTER(LEN=1),ALLOCATABLE :: CTEMP(:)
      real,pointer,dimension(:) :: newfld

      interface
         subroutine gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,
     &                         bmap,ierr)
           character(len=1),intent(in) :: cgrib(lcgrib)
           integer,intent(in) :: lcgrib,ngpts
           integer,intent(inout) :: iofst
           integer,intent(out) :: ibmap
           integer,intent(out) :: ierr
           logical*1,pointer,dimension(:) :: bmap
         end subroutine gf_unpack6
         subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl,
     &                         idrsnum,idrstmpl,ndpts,fld,ierr)
           character(len=1),intent(in) :: cgrib(lcgrib)
           integer,intent(in) :: lcgrib,ndpts,idrsnum,igdsnum
           integer,intent(inout) :: iofst
           integer,pointer,dimension(:) :: idrstmpl,igdstmpl
           integer,intent(out) :: ierr
           real,pointer,dimension(:) :: fld
         end subroutine gf_unpack7
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET INFO
      NULLIFY(gfld%bmap,gfld%fld)
      IRET=0
      CALL G2_GBYTEC(CINDEX,LSKIP,4*8,4*8)
      CALL G2_GBYTEC(CINDEX,SKIP6,24*8,4*8)
      CALL G2_GBYTEC(CINDEX,SKIP7,28*8,4*8)

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK BIT_MAP, IF PRESENT
      IF ( gfld%ibmap.eq.0.OR.gfld%ibmap.eq.254 ) THEN
         ISKIP=LSKIP+SKIP6
         CALL BAREAD(LUGB,ISKIP,4,LREAD,CSIZE)    ! GET LENGTH OF SECTION
         CALL G2_GBYTEC(CSIZE,ILEN,0,32)
         ALLOCATE(CTEMP(ILEN))
         CALL BAREAD(LUGB,ISKIP,ILEN,LREAD,CTEMP)  ! READ IN SECTION
         IF (ILEN.NE.LREAD) THEN
            IRET=97
            DEALLOCATE(CTEMP)
            RETURN
         ENDIF
         IOFST=0
         CALL GF_UNPACK6(CTEMP,ILEN,IOFST,gfld%ngrdpts,idum,
     &                   gfld%bmap,ierr)
         IF (IERR.NE.0) THEN
            IRET=98
            DEALLOCATE(CTEMP)
            RETURN
         ENDIF
         DEALLOCATE(CTEMP)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK DATA FIELD 
      ISKIP=LSKIP+SKIP7
      CALL BAREAD(LUGB,ISKIP,4,LREAD,CSIZE)    ! GET LENGTH OF SECTION
      CALL G2_GBYTEC(CSIZE,ILEN,0,32)
      if (ilen.lt.6) ilen=6
      ALLOCATE(CTEMP(ILEN))
      CALL BAREAD(LUGB,ISKIP,ILEN,LREAD,CTEMP)  ! READ IN SECTION
      IF (ILEN.NE.LREAD) THEN
         IRET=97
         DEALLOCATE(CTEMP)
         RETURN
      ENDIF
      IOFST=0
      CALL GF_UNPACK7(CTEMP,ILEN,IOFST,gfld%igdtnum,gfld%igdtmpl,
     &                   gfld%idrtnum,gfld%idrtmpl,gfld%ndpts,
     &                   gfld%fld,ierr)
      IF (IERR.NE.0) THEN
         IRET=98
         DEALLOCATE(CTEMP)
         RETURN
      ENDIF
      DEALLOCATE(CTEMP)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !  If bitmap is used with this field, expand data field
      !  to grid, if possible.
      if ( gfld%ibmap .ne. 255 .AND. associated(gfld%bmap) ) then
            allocate(newfld(gfld%ngrdpts))
            !newfld=0.0
            !newfld=unpack(lgfld%fld,lgfld%bmap,newfld)
            n=1
            do j=1,gfld%ngrdpts
                if ( gfld%bmap(j) ) then
                  newfld(j)=gfld%fld(n)
                  n=n+1
                else
                  newfld(j)=0.0
                endif
            enddo
            deallocate(gfld%fld);
            gfld%fld=>newfld;
            gfld%expanded=.true.
      else
         gfld%expanded=.true.
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
