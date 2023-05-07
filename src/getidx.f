!> @file
!> @brief This subroutine finds, reads or generates a GRIB2 index for
!> the GRIB2 file associated with unit lugb.
!> @author Stephen Gilbert @date 2005-03-15

!> This subroutine finds, reads or generates a GRIB2 index for
!> the GRIB2 file associated with unit lugb. If the index already
!> exists, it is returned, otherwise, the index is (1) read from an
!> existing indexfile associated with unit LUGI or (2) generated
!> from the grib2file lugi.
!>
!> Users can force a regeneration of an index: if lugi equals lugb,
!> the index will be regenerated from the data in file LUGB. If LUGI
!> is less than zero, then the index is re read from index file
!> abs(lugi).
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2005-03-15 | Stephen Gilbert | Initial Programming
!> 2009-07-09 | Boi Vuong | Fixed bug for checking (LUGB) unit index file
!> 2016-03-29 | Boi Vuong | Restore getidx.f from 1.2.3; file num up to 9999; added templates: 4.60, 4.61
!>
!> @param[in] lugb integer unit of the unblocked GRIB2 data file.
!> File must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] lugi integer unit of the unblocked GRIB2 index file.
!> If nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. Set to 0 to get index buffer from the grib file.
!> @param[out] cindex character*1 pointer to a buffer that will get
!> index records.
!> @param[out] nlen integer total length of all index records
!> @param[out] nnum integer number of index records
!> @param[out] iret integer return code
!> - 0 No error.
!> - 90 Unit number out of range.
!> - 96 Error reading/creating index file.
!>
!> @note Allow file unit numbers in range 0 - 9999.
!>
!> @author Stephen Gilbert @date 2005-03-15
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
