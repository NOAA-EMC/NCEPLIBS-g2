C>    @file
C>    @brief This subroutine finds, reads or generates a grib2 index for
C>    the grib2 file associated with unit lugb.
C>    @author Stephen Gilbert @date 2005-03-15
C>

C>    This subroutine finds, reads or generates a grib2 index for
C>    the grib2 file associated with unit lugb. If the index already
C>    exists, it is returned. otherwise, the index is (1) read from an
C>    existing indexfile associated with unit LUGI. or (2) generated
C>    from the grib2file LUGI. Users can force a regeneration of an
C>    index. If LUGI equals LUGB, the index will be regenerated from 
C>    the data in file LUGB. If LUGI is less than zero, then the index
C>    is re read from index file abs(lugi).
C>
C>    PROGRAM HISTORY LOG:
C>    - 2005-03-15 Stephen Gilbert Initial Programming
C>    - 2009-07-09 Boi Vuong Fixed bug for checking (LUGB) unit index file
C>    - 2016-03-29 Boi Vuong Restore original getidx.f from version
C>    1.2.3 modified getidex to allow to open range of unit file number
C>    up to 9999 added new parameters and new product definition
C>    template numbers: 4.60, 4.61
C>
C>    @param[in] LUGB integer unit of the unblocked grib data file.
C>    file must be opened with baopen or baopenr before calling
C>    this routine.
C>    @param[in] LUGI integer unit of the unblocked grib index file.
C>    if nonzero, file must be opened with baopen baopenr before
C>    calling this routine. (=0 to get index buffer from the grib file)
C>    @param[out] CINDEX character*1 pointer to a buffer that contains
C>    index records.
C>    @param[out] NLEN integer total length of all index records
C>    @param[out] NNUM integer number of index records
C>    @param[out] IRET integer return code
C>    - 0 all ok
C>    - 90 unit number out of range
C>    - 96 error reading/creating index file
C>
C>    @note allow file unit numbers in range 0 - 9999
C>    the grib index will automatically generate the index file. 
C>  
C>    @author Stephen Gilbert @date 2005-03-15
C>

C-----------------------------------------------------------------------
      SUBROUTINE GETIDX(LUGB,LUGI,CINDEX,NLEN,NNUM,IRET)

      INTEGER,INTENT(IN) :: LUGB,LUGI
      INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CINDEX

      INTEGER,PARAMETER :: MAXIDX=10000
      INTEGER,PARAMETER :: MSK1=32000,MSK2=4000
 
      TYPE GINDEX
         integer :: nlen
         integer :: nnum
         character(len=1),pointer,dimension(:) :: cbuf
      END TYPE GINDEX
     
      TYPE(GINDEX), save :: IDXLIST(10000)

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
      END INTERFACE

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      LUX=0
      IRET=0
      IF ( LUGB.LE.0 .OR. LUGB.GT.9999 ) THEN
         PRINT*,' '
         PRINT *,' FILE UNIT NUMBER OUT OF RANGE'
         PRINT *,' USE UNIT NUMBERS IN RANGE: 0 - 9999 '
         PRINT*,' '
         IRET=90
         RETURN
      ENDIF
      IF (LUGI.EQ.LUGB) THEN      ! Force regeneration of index from GRIB2 File
         IF ( ASSOCIATED( IDXLIST(LUGB)%CBUF ) ) 
     &                  DEALLOCATE(IDXLIST(LUGB)%CBUF)
         NULLIFY(IDXLIST(LUGB)%CBUF)
         IDXLIST(LUGB)%NLEN=0
         IDXLIST(LUGB)%NNUM=0
         LUX=0
      ENDIF

      IF (LUGI.LT.0) THEN      ! Force re-read of index from indexfile
                               ! associated with unit abs(lugi)
         IF ( ASSOCIATED( IDXLIST(LUGB)%CBUF ) ) 
     &                  DEALLOCATE(IDXLIST(LUGB)%CBUF)
         NULLIFY(IDXLIST(LUGB)%CBUF)
         IDXLIST(LUGB)%NLEN=0
         IDXLIST(LUGB)%NNUM=0
         LUX=ABS(LUGI)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  Check if index already exists in memory
      IF ( ASSOCIATED( IDXLIST(LUGB)%CBUF ) ) THEN
         CINDEX => IDXLIST(LUGB)%CBUF
         NLEN = IDXLIST(LUGB)%NLEN
         NNUM = IDXLIST(LUGB)%NNUM
         RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRGI=0
      IF(LUX.GT.0) THEN
        CALL GETG2I(LUX,IDXLIST(LUGB)%CBUF,NLEN,NNUM,IRGI)
      ELSEIF(LUX.LE.0) THEN
        MSKP=0
        CALL GETG2IR(LUGB,MSK1,MSK2,MSKP,IDXLIST(LUGB)%CBUF,
     &               NLEN,NNUM,NMESS,IRGI)
      ENDIF
      IF(IRGI.EQ.0) THEN
         CINDEX => IDXLIST(LUGB)%CBUF
         IDXLIST(LUGB)%NLEN = NLEN
         IDXLIST(LUGB)%NNUM = NNUM
      ELSE
         NLEN = 0
         NNUM = 0
         PRINT*,' '
         PRINT *,' ERROR READING INDEX FILE '
         PRINT*,' '
         IRET=96
         RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
