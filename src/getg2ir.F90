!> @file
!> @brief Generate an index record for a message in a GRIB2 file.
!> @author Mark Iredell @date 1995-10-31

!> Generate an index record for a message in a GRIB2 file.
!>
!> The index record contains byte offsets to the message, it's length,
!> and byte offsets within the message to each section. The index file
!> record format is documented in subroutine ixgb2().
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @param[in] lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] msk1 Number of bytes to search for first message.
!> @param[in] msk2 Number of bytes to search for other messages.
!> @param[in] mnum Number of GRIB messages to skip (usually 0).
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param[out] nlen Total length of index record buffer in bytes.
!> @param[out] nnum Number of index records, zero if no GRIB
!> messages are found.
!> @param[out] nmess Last GRIB message in file successfully processed
!> @param[out] iret Return code.
!> - 0 No error.
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 Not enough memory to allocate initial index buffer.
!>
!> @author Mark Iredell @date 1995-10-31
SUBROUTINE GETG2IR(LUGB, MSK1, MSK2, MNUM, CBUF, NLEN, NNUM, NMESS, IRET)
  USE RE_ALLOC              ! NEEDED FOR SUBROUTINE REALLOC
  implicit none

  CHARACTER(LEN = 1), POINTER, DIMENSION(:) :: CBUF
  INTEGER, INTENT(IN) :: LUGB, MSK1, MSK2, MNUM
  INTEGER, INTENT(OUT) :: NLEN, NNUM, NMESS, IRET
  CHARACTER(LEN = 1), POINTER, DIMENSION(:) :: CBUFTMP
  integer :: nbytes, newsize, next, numfld, m, mbuf, lskip, lgrib
  integer :: istat, iseek, init, iret1
  PARAMETER(INIT = 50000, NEXT = 10000)

  INTERFACE      ! REQUIRED FOR CBUF POINTER
     SUBROUTINE IXGB2(LUGB, LSKIP, LGRIB, CBUF, NUMFLD, MLEN, IRET)
       INTEGER, INTENT(IN) :: LUGB, LSKIP, LGRIB
       CHARACTER(LEN = 1), POINTER, DIMENSION(:) :: CBUF
       INTEGER, INTENT(OUT) :: NUMFLD, MLEN, IRET
     END SUBROUTINE IXGB2
  END INTERFACE

  ! Initialize.
  IRET = 0
  NULLIFY(CBUF)
  MBUF = INIT
  ALLOCATE(CBUF(MBUF), STAT = ISTAT)    ! Allocate initial space for cbuf.
  IF (ISTAT .NE. 0) THEN
     IRET = 2
     RETURN
  ENDIF

  ! Search for first grib message.
  ISEEK = 0
  CALL SKGB(LUGB, ISEEK, MSK1, LSKIP, LGRIB)
  DO M = 1, MNUM
     IF(LGRIB.GT.0) THEN
        ISEEK = LSKIP + LGRIB
        CALL SKGB(LUGB, ISEEK, MSK2, LSKIP, LGRIB)
     ENDIF
  ENDDO

  ! Get index records for every grib message found.
  NLEN = 0
  NNUM = 0
  NMESS = MNUM
  DO WHILE(IRET .EQ. 0 .AND. LGRIB .GT. 0)
     CALL IXGB2(LUGB, LSKIP, LGRIB, CBUFTMP, NUMFLD, NBYTES, IRET1)
     IF (IRET1 .NE. 0) PRINT *, ' SAGT ', NUMFLD, NBYTES, IRET1
     IF((NBYTES + NLEN) .GT. MBUF) THEN             ! Allocate more space, if necessary.
        NEWSIZE = MAX(MBUF + NEXT, MBUF + NBYTES)
        CALL REALLOC(CBUF, NLEN, NEWSIZE, ISTAT)
        IF (ISTAT .NE. 0) THEN
           IRET = 1
           RETURN
        ENDIF
        MBUF = NEWSIZE
     ENDIF

     ! If index records were returned in cbuftmp from ixgb2, 
     ! copy cbuftmp into cbuf, then deallocate cbuftmp when done.
     IF (ASSOCIATED(CBUFTMP)) THEN
        CBUF(NLEN + 1 : NLEN + NBYTES) = CBUFTMP(1 : NBYTES)
        DEALLOCATE(CBUFTMP, STAT = ISTAT)
        IF (ISTAT.NE.0) THEN
           PRINT *, ' deallocating cbuftmp ... ', istat
           stop 99
        ENDIF
        NULLIFY(CBUFTMP)
        NNUM = NNUM + NUMFLD
        NLEN = NLEN + NBYTES
        NMESS = NMESS + 1
     ENDIF

     ! Look for next grib message.
     ISEEK = LSKIP + LGRIB
     CALL SKGB(LUGB, ISEEK, MSK2, LSKIP, LGRIB)
  ENDDO
END SUBROUTINE GETG2IR
