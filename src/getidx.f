C-----------------------------------------------------------------------
      SUBROUTINE GETIDX(LUGB,LUGI,CINDEX,NLEN,NNUM,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETIDX         FINDS, READS OR GENERATES A GRIB2 INDEX 
C   PRGMMR: GILBERT          ORG: W/NP11     DATE: 2005-03-15
C
C ABSTRACT: FINDS, READS OR GENERATES A GRIB2 INDEX FOR THE GRIB2 FILE
C  ASSOCIATED WITH UNIT LUGB.  IF THE INDEX ALREADY EXISTS, IT IS RETURNED.
C  OTHERWISE, THE INDEX IS (1) READ FROM AN EXISTING INDEXFILE ASSOCIATED WITH
C  UNIT LUGI. OR (2) GENERATED FROM THE GRIB2FILE LUGB ( IF LUGI=0 ). 
C  USERS CAN FORCE A REGENERATION OF AN INDEX.  IF LUGI EQUALS LUGB, THE INDEX
C  WILL BE REGENERATED FROM THE DATA IN FILE LUGB.  IF LUGI IS LESS THAN
C  ZERO, THEN THE INDEX IS RE READ FROM INDEX FILE ABS(LUGI).  
C
C PROGRAM HISTORY LOG:
C 2005-03-15  GILBERT
C 2009-07-09  VUONG      Fixed bug for checking (LUGB) unit index file
C 2016-03-29  VUONG      Restore original getidx.f from version 1.2.3
C                        Modified GETIDEX to allow to open range of unit file number up to 9999
C                        Added new parameters and new Product Definition Template
C                        numbers: 4.60, 4.61
C
C USAGE:    CALL GETIDX(LUGB,LUGI,CINDEX,NLEN,NNUM,IRET)
C
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE.
C                  FILE MUST BE OPENED WITH BAOPEN OR BAOPENR BEFORE CALLING 
C                  THIS ROUTINE.
C     LUGI         INTEGER UNIT OF THE UNBLOCKED GRIB INDEX FILE.
C                  IF NONZERO, FILE MUST BE OPENED WITH BAOPEN BAOPENR BEFORE 
C                  CALLING THIS ROUTINE.
C                  >0 - READ INDEX FROM INDEX FILE LUGI, IF INDEX DOESN"T 
C                       ALREADY EXIST.
C                  =0 - TO GET INDEX BUFFER FROM THE GRIB FILE, IF INDEX 
C                       DOESN"T ALREADY EXIST.
C                  <0 - FORCE REREAD OF INDEX FROM INDEX FILE ABS(LUGI).
C                  =LUGB - FORCE REGENERATION OF INDEX FROM GRIB2 FILE LUGB.
C
C   OUTPUT ARGUMENTS:
C     CINDEX       CHARACTER*1 POINTER TO A BUFFER THAT CONTAINS INDEX RECORDS.
C     NLEN         INTEGER TOTAL LENGTH OF ALL INDEX RECORDS
C     NNUM         INTEGER NUMBER OF INDEX RECORDS
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    90     UNIT NUMBER OUT OF RANGE
C                    96     ERROR READING/CREATING INDEX FILE
C
C SUBPROGRAMS CALLED:
C   GETG2I          READ INDEX FILE
C   GETG2IR         READ INDEX BUFFER FROM GRIB FILE
C
C REMARKS: 
C        -  Allow file unit numbers in range 0 - 9999
C           the grib index will automatically generate the index file.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$

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
     
      TYPE(GINDEX) :: IDXLIST(10000)

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

      IF (ASSOCIATED(IDXLIST(LUGB)%CBUF)) then
         DEALLOCATE(IDXLIST(LUGB)%CBUF)
      end if

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
