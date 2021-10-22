C>    @file
C>    @brief This subroutine read a GRIB2 index file and return its content.
C>    @author Mark Iredell @date 1995-10-31
C>

C>    Read a grib2 index file and return its contents.
C>    Version 1 of the index file has the following format:
C>    81-byte s.lord header with 'gb2ix1' in columns 42-47 followed by
C>    81-byte header with number of bytes to skip before index records,
C>    total length in bytes of the index records, number of index records,
C>    and grib file basename written in format ('ix1form:',3i10,2x,a40).
C>    Each following index record corresponds to a grib message
C>    and has the internal format:
C>    - byte 001 - 004 length of index record
C>    - byte 005 - 008 bytes to skip in data file before grib message
C>    - byte 009 - 012 bytes to skip in message before lus (local use)
C>    set = 0, if no local use section in grib2 message.
C>    - byte 013 - 016 bytes to skip in message before gds
C>    - byte 017 - 020 bytes to skip in message before pds
C>    - byte 021 - 024 bytes to skip in message before drs
C>    - byte 025 - 028 bytes to skip in message before bms
C>    - byte 029 - 032 bytes to skip in message before data section
C>    - byte 033 - 040 bytes total in the message
C>    - byte 041 - 041 grib version number (currently 2)
C>    - byte 042 - 042 message discipline
C>    - byte 043 - 044 field number within grib2 message
C>    - byte 045 -  ii identification section (ids)
C>    - byte ii+1-  jj grid definition section (gds)
C>    - byte jj+1-  kk product definition section (pds)
C>    - byte kk+1-  ll the data representation section (drs)
C>    - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
C>
C>    ### Program History Log
C>    Date | Programmer | Comments
C>    -----|------------|--------- 
C>    1995-10-31 | Mark Iredell | Initial.
C>    1996-10-31 | Mark Iredell | Augmented optional definitions to byte 320.
C>    2002-01-03 | Stephen Gilbert | Modified from getgi to work with grib2.
C>
C>    @param[in] lugi Integer unit of the unblocked grib index file. Must
C>     be opened by [baopen() or baopenr()]
C>    (https://noaa-emc.github.io/NCEPLIBS-bacio/).
C>    @param[out] cbuf Pointer to a buffer that contains index
C>    records. Users should free memory that cbuf points to, using
C>    deallocate(cbuf) when cbuf is no longer needed.
C>    @param[out] nlen Total length of all index records.
C>    @param[out] nnum Number of index records.
C>    @param[out] iret Return code.
C>    - 0 all ok
C>    - 2 not enough memory to hold index buffer
C>    - 3 error reading index file buffer
C>    - 4 error reading index file header
C>
C>    @note Subprogram can be called from a multiprocessing environment.
C>    Do not engage the same logical unit from more than one processor.
C>
C>    @author Mark Iredell @date 2000-05-26
C>

      SUBROUTINE GETG2I(LUGI,CBUF,NLEN,NNUM,IRET)

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
      INTEGER,INTENT(IN) :: LUGI
      INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
      CHARACTER CHEAD*162
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF (ASSOCIATED(CBUF)) NULLIFY(CBUF)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NLEN=0
      NNUM=0
      IRET=4
      CALL BAREAD(LUGI,0,162,LHEAD,CHEAD)
      IF(LHEAD.EQ.162.AND.CHEAD(42:47).EQ.'GB2IX1') THEN
        READ(CHEAD(82:162),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
        IF(IOS.EQ.0) THEN
          
          ALLOCATE(CBUF(NLEN),STAT=ISTAT)    ! ALLOCATE SPACE FOR CBUF
          IF (ISTAT.NE.0) THEN
             IRET=2
             RETURN
          ENDIF
          IRET=0
          CALL BAREAD(LUGI,NSKP,NLEN,LBUF,CBUF)
          IF(LBUF.NE.NLEN) IRET=3

        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
