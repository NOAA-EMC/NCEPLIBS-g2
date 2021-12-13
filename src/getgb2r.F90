!>    @file
!>    @brief This subroutine read and unpack sections 6 and 7 from ah
!>    grib2 message.
!>    @author Stephen Gilbert @date 2002-01-11
!>

!>    This subroutine read and unpack sections 6 and 7 from a grib2
!>    message. It assumes that the "metadata" for this field already
!>    exists in derived type gribfield. Specifically, it requires
!>    gfld\%ibmap, gfld\%ngrdpts, gfld\%idrtnum, gfld\%idrtmpl, and
!>    gfld\%ndpts. It decoded information for the selected grib field and
!>    returned it in a derived type variable, gfld. gfld is of type
!>    gribfield, which is defined in module grib_mod, so users of this
!>    routine will need to include the line "use grib_mod" in their
!>    calling routine. Each component of the gribfield type is described
!>    in the output argument list section below.
!>
!>    @param[in] LUGB integer unit of the unblocked grib data file.
!>    @param[in] CINDEX index record of the grib field (see docblock of
!>    subroutine ixgb2() for description of an index record.)
!>    @param[out] GFLD derived type gribfield (defined in module grib_mod)
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
!>    - 0 all ok
!>    - 97 error reading grib file
!>    - other gf_getfld grib2 unpacker return code
!>
!>    @note do not engage the same logical unit from more than one
!>    processor. This subprogram is intended for private use by getgb2
!>    routines only. Note that derived type gribfield contains pointers
!>    to many arrays of data. The memory for these arrays is allocated
!>    when the values in the arrays are set, to help minimize problems
!>    with array overloading. Because of this users are encouraged to
!>    free up this memory, when it is no longer needed, by an explicit
!>    call to subroutine gf_free().
!>
!>    @author Stephen Gilbert @date 2002-01-11
!>
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
     subroutine gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap, &
          bmap,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib,ngpts
       integer,intent(inout) :: iofst
       integer,intent(out) :: ibmap
       integer,intent(out) :: ierr
       logical*1,pointer,dimension(:) :: bmap
     end subroutine gf_unpack6
     subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl, &
          idrsnum,idrstmpl,ndpts,fld,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib,ndpts,idrsnum,igdsnum
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: idrstmpl,igdstmpl
       integer,intent(out) :: ierr
       real,pointer,dimension(:) :: fld
     end subroutine gf_unpack7
  end interface

  !  GET INFO
  NULLIFY(gfld%bmap,gfld%fld)
  IRET=0
  CALL G2_GBYTEC(CINDEX,LSKIP,4*8,4*8)
  CALL G2_GBYTEC(CINDEX,SKIP6,24*8,4*8)
  CALL G2_GBYTEC(CINDEX,SKIP7,28*8,4*8)


  !  READ AND UNPACK BIT_MAP, IF PRESENT
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
     CALL GF_UNPACK6(CTEMP,ILEN,IOFST,gfld%ngrdpts,idum, &
          gfld%bmap,ierr)
     IF (IERR.NE.0) THEN
        IRET=98
        DEALLOCATE(CTEMP)
        RETURN
     ENDIF
     DEALLOCATE(CTEMP)
  ENDIF

  !  READ AND UNPACK DATA FIELD
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
  CALL GF_UNPACK7(CTEMP,ILEN,IOFST,gfld%igdtnum,gfld%igdtmpl, &
       gfld%idrtnum,gfld%idrtmpl,gfld%ndpts, &
       gfld%fld,ierr)
  IF (IERR.NE.0) THEN
     IRET=98
     DEALLOCATE(CTEMP)
     RETURN
  ENDIF
  DEALLOCATE(CTEMP)

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

  RETURN
END SUBROUTINE GETGB2R
