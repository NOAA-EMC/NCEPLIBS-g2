C>    @file
C>    @brief This subroutine finds and unpacks a grib file.
C>    @author Mark Iredell @date 1994-04-01
C>

C>    This subroutine finds and unpacks a grib message. It reads
C>    a grib index file (or optionally the grib file itself) to
C>    get the index buffer (i.e. table of contents) for the grib file.
C>      
C>    Find in the index buffer a reference to the grib field requested.
C>      
C>    The grib field request specifies the number of fields to skip
C>    and the unpacked identification section, grid definition template
C>    and product defintion section parameters. (A requested parameter
C>    of -9999 means to allow any value of this parameter to be found.)
C>    If the requested grib field is found, then it is read from the
C>    grib file and unpacked. Its number is returned along with
C>    the associated unpacked parameters. the bitmap (if any), and
C>    the data values are unpacked only if argument "unpack" is set to
C>    true. If the grib field is not found, then the return code
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
C>    1994-04-01 | Mark Iredell | Initial.
C>    1995-10-31 | Mark Iredell | modularized code into subprograms, allowed for unspecified index file,
C>    2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with grib2
C>    2015-11-10 | Boi Vuong | modified doc block for gfld\%ngrdpts and gfld\%ndpts
C>
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    File must be opened with [baopen() or baopenr()]
C>    (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
C>    this routine.
C>    @param[in] LUGI integer unit of the unblocked grib index file.
C>    If nonzero, file must be opened with [baopen() or baopenr()]
C>    (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
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
C>    @param[out] IRET integer return code
C>    - 0 all ok
C>    - 96 error reading index
C>    - 97 error reading grib file
C>    - 99 request not found
C>    - other gf_getfld grib2 unpacker return code
C>
C>    @note Specify an index file if feasible to increase speed.  Do not
C>    engage the same logical unit from more than one processor.  Note
C>    that derived type gribfield contains pointers to many arrays of
C>    data. The memory for these arrays is allocated when the values in
C>    the arrays are set, to help minimize problems with array
C>    overloading. Because of this users are encouraged to free up this
C>    memory, when it is no longer needed, by an explicit call to
C>    subroutine gf_free().
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
