C>    @file
C>    @brief This subroutine find and extracts a grib message from a file.
C>    @author Mark Iredell @date 1994-04-01
C>

C>    This subroutine find and extracts a grib message from a file.
C>    It reads a grib index file (or optionally the grib file itself) to
C>    get the index buffer (i.e. table of contents) for the grib file.
C>    find in the index buffer a reference to the grib field requested.
C>    the grib field request specifies the number of fields to skip
C>    and the unpacked identification section, grid definition template
C>    and product defintion section parameters. (a requested parameter
C>    of -9999 means to allow any value of this parameter to be found.)
C>    if the requested grib field is found, then it is read from the
C>    grib file and unpacked. If the grib field is not found, then the
C>    return code will be nonzero.
C>
C>    PROGRAM HISTORY LOG:
C>    - 1994-04-01 Mark Iredell
C>    - 1995-10-31 Mark Iredell modularized portions of code into subprograms
C>    and allowed for unspecified index file
C>    - 2002-01-11 Stephen Gilbert modified from getgb and getgbm to work with grib2
C>    - 2003-12-17 Stephen Gilbert modified from getgb2 to return packed grib2 message
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    file must be opened with baopen or baopenr before calling
C>    this routine.
C>    @param[in] LUGI integer unit of the unblocked grib index file.
C>    if nonzero, file must be opened with baopen baopenr before
C>    calling this routine. (=0 to get index buffer from the grib file)
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
C>    @param[in] EXTRACT logical value indicating whether to return a
C>    grib2 message with just the requested field, or the entire
C>    grib2 message containing the requested field.
C>    - .true. = return grib2 message containing only the requested field.
C>    - .false. = return entire grib2 message containing the requested field.
C>    @param[out] K integer field number unpacked.
C>    @param[out] GRIBM returned grib message.
C>    @param[out] LENG length of returned grib message in bytes.
C>    @param[out] IRET integer return code
C>    - 0 all ok
C>    - 96 error reading index
C>    - 97 error reading grib file
C>    - 99 request not found
C>    @note specify an index file if feasible to increase speed.
C>    do not engage the same logical unit from more than one processor.
C>    Note that derived type gribfield contains pointers to many
C>    arrays of data. The memory for these arrays is allocated
C>    when the values in the arrays are set, to help minimize
C>    problems with array overloading. Because of this users are 
C>    encouraged to free up this memory, when it is no longer
C>    needed, by an explicit call to subroutine gf_free.
C>
C>    @author Mark Iredell @date 1994-04-01
C>

C-----------------------------------------------------------------------
      SUBROUTINE GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &                   EXTRACT,K,GRIBM,LENG,IRET)

      USE GRIB_MOD

      INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
      LOGICAL,INTENT(IN) :: EXTRACT
      INTEGER,INTENT(OUT) :: K,IRET,LENG
      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM

      TYPE(GRIBFIELD) :: GFLD

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
      PARAMETER(MSK1=32000,MSK2=4000)

      SAVE CBUF,NLEN,NNUM
      DATA LUX/0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)
      INTERFACE
         SUBROUTINE GETG2I(LUGI,CBUF,NLEN,NNUM,IRET)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGI
            INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
         END SUBROUTINE GETG2I
         SUBROUTINE GETG2IR(LUGB,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM,
     &                      NMESS,IRET)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGB,MSK1,MSK2,MNUM
            INTEGER,INTENT(OUT) :: NLEN,NNUM,NMESS,IRET
         END SUBROUTINE GETG2IR
         SUBROUTINE GETGB2RP(LUGB,CINDEX,EXTRACT,GRIBM,LENG,IRET)
            INTEGER,INTENT(IN) :: LUGB
            CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
            LOGICAL,INTENT(IN) :: EXTRACT
            INTEGER,INTENT(OUT) :: LENG,IRET
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
         END SUBROUTINE GETGB2RP
      END INTERFACE

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      IRGI=0
      IF(LUGI.GT.0.AND.LUGI.NE.LUX) THEN
        CALL GETG2I(LUGI,CBUF,NLEN,NNUM,IRGI)
        LUX=LUGI
      ELSEIF(LUGI.LE.0.AND.LUGB.NE.LUX) THEN
        MSKP=0
        CALL GETG2IR(LUGB,MSK1,MSK2,MSKP,CBUF,NLEN,NNUM,NMESS,IRGI)
        LUX=LUGB
      ENDIF
      IF(IRGI.GT.1) THEN
        IRET=96
        LUX=0
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
C  EXTRACT GRIB MESSAGE FROM FILE
      CALL GETGB2RP(LUGB,CBUF(LPOS:),EXTRACT,GRIBM,LENG,IRET)
!      IF ( EXTRACT ) THEN
!         PRINT *,'NOT SUPPOSED TO BE HERE.'
!      ELSE
!         IPOS=(LPOS+3)*8
!         CALL G2_GBYTEC(CBUF,ISKIP,IPOS,32)     ! BYTES TO SKIP IN FILE
!         IPOS=IPOS+(32*8)
!         CALL G2_GBYTEC(CBUF,LENG,IPOS,32)      ! LENGTH OF GRIB MESSAGE
!         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))
!         CALL BAREAD(LUGB,ISKIP,LENG,LREAD,GRIBM)
!         IF ( LENG .NE. LREAD ) THEN
!            IRET=97
!            CALL GF_FREE(GFLD)
!            RETURN
!         ENDIF
!      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      K=JK
      CALL GF_FREE(GFLD)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
