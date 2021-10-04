C>    @file
C>    @brief This subroutine finds and unpacks a grib file.
C>    @author Mark Iredell @date 1994-04-01
C>

C>    This subroutine finds and unpacks a grib message. It reads
C>    a grib index file (or optionally the grib file itself) to
C>    get the index buffer (i.e. table of contents) for the grib file.
C>    find in the index buffer a reference to the grib field requested.
C>    the grib field request specifies the number of fields to skip
C>    and the unpacked identification section, grid definition template
C>    and product defintion section parameters. (a requested parameter
C>    of -9999 means to allow any value of this parameter to be found.)
C>    if the requested grib field is found, then it is read from the
C>    grib file and unpacked. Its number is returned along with
C>    the associated unpacked parameters. the bitmap (if any), and
C>    the data values are unpacked only if argument "unpack" is set to
C>    true. if the grib field is not found, then the return code
C>    will be nonzero.
C>
C>    The decoded information for the selected GRIB field is returned
C>    in a derived type variable, gfld. Gfld is of type gribfield, 
C>    which is defined in module grib_mod, so users of this routine 
C>    will need to include the line "USE GRIB_MOD" in their calling 
C>    routine. Each component of the gribfield type is described in 
C>    the OUTPUT ARGUMENT LIST section below.
C>
C>    ### Program History Log
C>    Date | Programmer | Comments
C>    -----|------------|--------- 
C>    1994-04-01 | Mark Iredell |
C>    1995-10-31 | Mark Iredell |  modularized code into subprograms, allowed for unspecified index file,
C>    2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with grib2
C>    2015-11-10 | Boi Vuong | modified doc block for gfld\%ngrdpts and gfld\%ndpts
C>
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    file must be opened with baopen or baopenr before calling
C>    this routine.
C>    @param[in] LUGI integer unit of the unblocked grib index file.
C>    if nonzero, file must be opened with baopen baopenr before
C>    calling this routine.
C>    - >0 read index from index file lugi, if index doesn"t already exist.
C>    - =0 to get index buffer from the grib file, if index
C>    doesn"t already exist.
C>    - <0 force reread of index from index file abs(lugi).
C>    - =lugb force regeneration of index from grib2 file lugb.
C>    @param[in] J integer number of fields to skip
C>    (=0 to search from beginning)
C>    @param[in] JDISC grib2 discipline number of requested field
C>    (if = -1, accept any discipline see code table 0.0)
C>    - 0 meteorological products
C>    - 1 hydrological products
C>    - 2 land surface products
C>    - 3 space products
C>    - 10 oceanographic products
C>    @param[in] JIDS integer array of values in the identification section
C>    (=-9999 for wildcard)
C>    - JIDS(1) identification of originating centre
C>    (see common code table c-1)
C>    - JIDS(2) identification of originating sub-centre
C>    - JIDS(3) grib master tables version number
C>    (see code table 1.0) 0 experimental;1 initial operational version number.
C>    - JIDS(4) grib local tables version number (see code table 1.1)
C>    0 local tables not used; 1-254 number of local tables version used.
C>    - JIDS(5) significance of reference time (code table 1.2)
C>    0 analysis; 1 start of forecast; 2 verifying time of forecast; 3 observation time
C>    - JIDS(6) year (4 digits)
C>    - JIDS(7) month
C>    - JIDS(8) day
C>    - JIDS(9) hour
C>    - JIDS(10) minute
C>    - JIDS(11) second
C>    - JIDS(12) production status of processed data (see code table 1.3)
C>    0 operational products; 1 operational test products;
C>    2 research products; 3 re-analysis products.
C>    - JIDS(13) type of processed data (see code table 1.4)
C>    0 analysis products; 1 forecast products; 2 analysis and forecast
C>    products; 3 control forecast products; 4 perturbed forecast products;
C>    5 control and perturbed forecast products; 6 processed satellite
C>    observations; 7 processed radar observations.
C>    @param[in] JPDTN integer product definition template number (n)
C>    (if = -1, don't bother matching pdt - accept any)
C>    @param[in] JPDT integer array of values defining the product definition
C>    template 4.n of the field for which to search (=-9999 for wildcard)
C>    @param[in] JGDTN integer grid definition template number (m)
C>    (if = -1, don't bother matching gdt - accept any )
C>    @param[in] JGDT integer array of values defining the grid definition
C>    template 3.m of the field for which to search (=-9999 for wildcard)
C>    @param[in] UNPACK logical value indicating whether to unpack bitmap/data
C>    - .TRUE. unpack bitmap and data values
C>    - .FALSE. do not unpack bitmap and data values
C>    @param[out] K integer field number unpacked
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
C>    - gfld\%idsect(12) Production status of processed data (see Code Table 1.3)
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
C>    points definition. Used to define number of points in each row (or column)
C>    for non-regular grids. = 0, if using regular grid.
C>    - gfld\%interp_opt Interpretation of list for optional points definition.(Code Table 3.11)
C>    - gfld\%igdtnum Grid Definition Template Number (Code Table 3.1)
C>    - gfld\%igdtmpl Contains the data values for the specified Grid
C>    Definition Template (NN=gfld\%igdtnum). Each element of this integer array
C>    contains an entry (in the order specified) of Grid Defintion
C>    Template 3.NN This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%igdtlen Number of elements in gfld\%igdtmpl. i.e. number of
C>    entries in Grid Defintion Template 3.NN (NN=gfld\%igdtnum).
C>    - gfld\%list_opt (Used if gfld\%numoct_opt .ne. 0) This array contains the
C>    number of grid points contained in each row (or column).
C>    (part of Section 3) This element is actually a pointer to
C>    an array that holds the data. This pointer is nullified
C>    if gfld\%numoct_opt=0.
C>    - gfld\%num_opt (Used if gfld\%numoct_opt .ne. 0) The number of entries in
C>    array ideflist. i.e. number of rows (or columns) for which
C>    optional grid points are defined. This value is set to zero,
C>    if gfld\%numoct_opt=0.
C>    - gfdl\%ipdtnum Product Definition Template Number (see Code Table 4.0)
C>    - gfld\%ipdtmpl Contains the data values for the specified Product Definition
C>    Template (N=gfdl\%ipdtnum). Each element of this integer array
C>    contains an entry (in the order specified) of Product Defintion
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
C>    - gfld\%idrtnum Data Representation Template Number (see Code Table 5.0)
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
C>    - 96 error reading index
C>    - 97 error reading grib file
C>    - 99 request not found
C>    - other gf_getfld grib2 unpacker return code
C>
C>    @note specify an index file if feasible to increase speed.
C>    do not engage the same logical unit from more than one processor.
C>    Note that derived type gribfield contains pointers to many
C>    arrays of data. The memory for these arrays is allocated
C>    when the values in the arrays are set, to help minimize
C>    problems with array overloading. Because of this users
C>    are encouraged to free up this memory, when it is no longer
C>    needed, by an explicit call to subroutine gf_free.
C>
C>    @author Mark Iredell @date 1994-04-01
C>

      SUBROUTINE GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &                  UNPACK,K,GFLD,IRET)
      USE GRIB_MOD

      INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
      LOGICAL,INTENT(IN) :: UNPACK
      INTEGER,INTENT(OUT) :: K,IRET
      TYPE(GRIBFIELD),INTENT(OUT) :: GFLD

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)
      INTERFACE
         SUBROUTINE GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGB,LUGI
            INTEGER,INTENT(OUT) :: NLEN,NNUM,IRGI
         END SUBROUTINE GETIDX
      END INTERFACE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      IRGI=0
      CALL GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
      IF(IRGI.GT.1) THEN
        IRET=96
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH INDEX BUFFER
      CALL GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &             JK,GFLD,LPOS,IRGS)
      IF(IRGS.NE.0) THEN
        IRET=99
        CALL GF_FREE(GFLD)
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ LOCAL USE SECTION, IF AVAILABLE
      CALL GETGB2L(LUGB,CBUF(LPOS),GFLD,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK GRIB RECORD
      IF (UNPACK) THEN
    !    NUMFLD=GFLD%IFLDNUM
    !    CALL GF_FREE(GFLD)
        CALL GETGB2R(LUGB,CBUF(LPOS),GFLD,IRET)
      ENDIF
      K=JK
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
