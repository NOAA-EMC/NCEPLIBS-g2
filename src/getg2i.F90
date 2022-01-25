!> @file
!> @brief This subroutine read a GRIB2 index file and return its
!> content.
!> @author Mark Iredell @date 1995-10-31

!> Read a grib2 index file and return its contents.
!>
!> Version 1 of the index file has the following format:
!> 81-byte s.lord header with 'gb2ix1' in columns 42-47 followed by
!> 81-byte header with number of bytes to skip before index records,
!> total length in bytes of the index records, number of index records,
!> and grib file basename written in format ('ix1form:',3i10,2x,a40).
!> Each following index record corresponds to a grib message
!> and has the internal format:
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before grib message
!> - byte 009 - 012 bytes to skip in message before lus (local use)
!> set = 0, if no local use section in grib2 message.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 grib version number (currently 2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within grib2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii+1-  jj grid definition section (gds)
!> - byte jj+1-  kk product definition section (pds)
!> - byte kk+1-  ll the data representation section (drs)
!> - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial.
!> 1996-10-31 | Mark Iredell | Augmented optional definitions to byte 320.
!> 2002-01-03 | Stephen Gilbert | Modified from getgi to work with grib2.
!>
!> @param[in] lugi Integer unit of the unblocked grib index file. Must
!>  be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[out] cbuf Pointer to a buffer that contains index
!> records. Users should free memory that cbuf points to, using
!> deallocate(cbuf) when cbuf is no longer needed.
!> @param[out] nlen Total length of all index records.
!> @param[out] nnum Number of index records.
!> @param[out] iret Return code.
!> - 0 all ok
!> - 2 not enough memory to hold index buffer
!> - 3 error reading index file buffer
!> - 4 error reading index file header
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 2000-05-26
SUBROUTINE GETG2I(LUGI,CBUF,NLEN,NNUM,IRET)

  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
  INTEGER,INTENT(IN) :: LUGI
  INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
  CHARACTER CHEAD*162

  IF (ASSOCIATED(CBUF)) NULLIFY(CBUF)

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

  RETURN
END SUBROUTINE GETG2I
