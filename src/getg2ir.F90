!>    @file
!>    @brief This subroutine read a GRIB file and return its index content.
!>    @author Mark Iredell @date 1995-10-31
!>

!>    This subroutine read a GRIB file and return its index content.
!>    The index buffer returned contains index records with the internal format:
!>    - byte 001 - 004 length of index record
!>    - byte 005 - 008 bytes to skip in data file before grib message
!>    - byte 009 - 012 bytes to skip in message before lus (local use)
!>    set = 0, if no local use section in grib2 message.
!>    - byte 013 - 016 bytes to skip in message before gds
!>    - byte 017 - 020 bytes to skip in message before pds
!>    - byte 021 - 024 bytes to skip in message before drs
!>    - byte 025 - 028 bytes to skip in message before bms
!>    - byte 029 - 032 bytes to skip in message before data section
!>    - byte 033 - 040 bytes total in the message
!>    - byte 041 - 041 grib version number (currently 2)
!>    - byte 042 - 042 message discipline
!>    - byte 043 - 044 field number within grib2 message
!>    - byte 045 -  ii identification section (ids)
!>    - byte ii+1-  jj grid definition section (gds)
!>    - byte jj+1-  kk product definition section (pds)
!>    - byte kk+1-  ll the data representation section (drs)
!>    - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>
!>    ### Program History Log
!>    Date | Programmer | Comments
!>    -----|------------|---------
!>    1995-10-31 | Mark Iredell
!>    1996-10-31 | Mark Iredell | augmented optional definitions to byte 320
!>    2002-01-02 | Stephen Gilbert | modified from getgir to create grib2 indexes
!>
!>    @param[in] lugb Unit of the unblocked grib file. Must
!>     be opened by [baopen() or baopenr()]
!>    (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!>    @param[in] msk1 Number of bytes to search for first message.
!>    @param[in] msk2 Number of bytes to search for other messages.
!>    @param[in] mnum Number of grib messages to skip (usually 0).
!>    @param[out] cbuf Pointer to a buffer that contains index
!>    records. Users should free memory that cbuf points to, using
!>    deallocate(cbuf) when cbuf is no longer needed.
!>    @param[out] nlen Total length of index record buffer in bytes.
!>    @param[out] nnum Number of index records, =0 if no grib
!>    messages are found).
!>    @param[out] nmess Last grib message in file successfully processed
!>    @param[out] iret Return code.
!>    - 0 all ok
!>    - 1 not enough memory available to hold full index buffer
!>    - 2 not enough memory to allocate initial index buffer
!>
!>    @note Subprogram can be called from a multiprocessing environment.
!>    Do not engage the same logical unit from more than one processor.
!>
!>    @author Mark Iredell @date 1995-10-31
!>

SUBROUTINE GETG2IR(LUGB,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM,NMESS,IRET)

  USE RE_ALLOC          ! NEEDED FOR SUBROUTINE REALLOC
  PARAMETER(INIT=50000,NEXT=10000)
  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
  INTEGER,INTENT(IN) :: LUGB,MSK1,MSK2,MNUM
  INTEGER,INTENT(OUT) :: NLEN,NNUM,NMESS,IRET
  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUFTMP
  INTERFACE      ! REQUIRED FOR CBUF POINTER
     SUBROUTINE IXGB2(LUGB,LSKIP,LGRIB,CBUF,NUMFLD,MLEN,IRET)
       INTEGER,INTENT(IN) :: LUGB,LSKIP,LGRIB
       CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
       INTEGER,INTENT(OUT) :: NUMFLD,MLEN,IRET
     END SUBROUTINE IXGB2
  END INTERFACE

  !  INITIALIZE
  IRET=0
  IF (ASSOCIATED(CBUF)) NULLIFY(CBUF)
  MBUF=INIT
  ALLOCATE(CBUF(MBUF),STAT=ISTAT)    ! ALLOCATE INITIAL SPACE FOR CBUF
  IF (ISTAT.NE.0) THEN
     IRET=2
     RETURN
  ENDIF

  !  SEARCH FOR FIRST GRIB MESSAGE
  ISEEK=0
  CALL SKGB(LUGB,ISEEK,MSK1,LSKIP,LGRIB)
  DO M=1,MNUM
     IF(LGRIB.GT.0) THEN
        ISEEK=LSKIP+LGRIB
        CALL SKGB(LUGB,ISEEK,MSK2,LSKIP,LGRIB)
     ENDIF
  ENDDO

  !  GET INDEX RECORDS FOR EVERY GRIB MESSAGE FOUND
  NLEN=0
  NNUM=0
  NMESS=MNUM
  DO WHILE(IRET.EQ.0.AND.LGRIB.GT.0)
     CALL IXGB2(LUGB,LSKIP,LGRIB,CBUFTMP,NUMFLD,NBYTES,IRET1)
     IF (IRET1.NE.0) PRINT *,' SAGT ',NUMFLD,NBYTES,IRET1
     IF((NBYTES+NLEN).GT.MBUF) THEN             ! ALLOCATE MORE SPACE, IF
        ! NECESSARY
        NEWSIZE=MAX(MBUF+NEXT,MBUF+NBYTES)
        CALL REALLOC(CBUF,NLEN,NEWSIZE,ISTAT)
        IF ( ISTAT .NE. 0 ) THEN
           IRET=1
           RETURN
        ENDIF
        MBUF=NEWSIZE
     ENDIF
     !
     !  IF INDEX RECORDS WERE RETURNED IN CBUFTMP FROM IXGB2,
     !  COPY CBUFTMP INTO CBUF, THEN DEALLOCATE CBUFTMP WHEN DONE
     !
     IF ( ASSOCIATED(CBUFTMP) ) THEN
        CBUF(NLEN+1:NLEN+NBYTES)=CBUFTMP(1:NBYTES)
        DEALLOCATE(CBUFTMP,STAT=ISTAT)
        IF (ISTAT.NE.0) THEN
           PRINT *,' deallocating cbuftmp ... ',istat
           stop 99
        ENDIF
        NULLIFY(CBUFTMP)
        NNUM=NNUM+NUMFLD
        NLEN=NLEN+NBYTES
        NMESS=NMESS+1
     ENDIF
     !      LOOK FOR NEXT GRIB MESSAGE
     ISEEK=LSKIP+LGRIB
     CALL SKGB(LUGB,ISEEK,MSK2,LSKIP,LGRIB)
  ENDDO

  RETURN
END SUBROUTINE GETG2IR
