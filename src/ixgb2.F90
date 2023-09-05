!> @file
!> @brief Generate an index record for each field in a GRIB2 message.
!> @author Mark Iredell @date 1995-10-31

!> Generate an index record for each field in a GRIB2 message. The index
!> records are written to index buffer pointed to by cbuf. All integers
!> in the index are in big-endian format.
!>
!> This subroutine is called by getg2ir(), which packages the index
!> records into an index file.
!>
!> The index buffer returned contains index records with the
!> format:
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before GRIB message
!> - byte 009 - 012 bytes to skip in message before lus (local use) set = 0, if no local section.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 GRIB version number (2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within GRIB2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii + 1-  jj grid definition section (gds)
!> - byte jj + 1-  kk product definition section (pds)
!> - byte kk + 1-  ll the data representation section (drs)
!> - byte ll + 1-ll + 6 first 6 bytes of the bit map section (bms)
!>
!> @param[in] lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] lskip Number of bytes to skip before GRIB message.
!> @param[in] lgrib Number of bytes in GRIB message. When subroutine is
!> called, this must be set to the size of the cbuf buffer.
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param[out] numfld Number of index records created.
!> @param[out] mlen Total length of all index records.
!> @param[out] iret Return code
!> - 0 No error
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 I/O error in read.
!> - 3 GRIB message is not edition 2.
!> - 4 Not enough memory to allocate extent to index buffer.
!> - 5 Unidentified GRIB section encountered.
!>
!> @author Mark Iredell @date 1995-10-31
SUBROUTINE IXGB2(LUGB, LSKIP, LGRIB, CBUF, NUMFLD, MLEN, IRET)
  USE RE_ALLOC              ! NEEDED FOR SUBROUTINE REALLOC
  implicit none

  CHARACTER(LEN = 1), POINTER, DIMENSION(:) :: CBUF
  CHARACTER CVER, CDISC
  CHARACTER(LEN = 4) :: CTEMP
  INTEGER LOCLUS, LOCGDS, LENGDS, LOCBMS
  integer :: indbmp, numsec, next, newsize, mova2i, mbuf, lindex
  integer :: linmax, ixskp
  integer :: mxspd, mxskp, mxsgd, mxsdr, mxsbm, mxlus
  integer :: mxlen, mxds, mxfld, mxbms
  integer :: init, ixlus, lugb, lskip, lgrib, numfld, mlen, iret
  integer :: ixsgd, ibread, ibskip, ilndrs, ilnpds, istat, ixds
  integer :: ixspd, ixfld, ixids, ixlen, ixsbm, ixsdr
  integer :: lbread, lensec, lensec1
  PARAMETER(LINMAX = 5000, INIT = 50000, NEXT = 10000)
  PARAMETER(IXSKP = 4, IXLUS = 8, IXSGD = 12, IXSPD = 16, IXSDR = 20, IXSBM = 24, &
       IXDS = 28, IXLEN = 36, IXFLD = 42, IXIDS = 44)
  PARAMETER(MXSKP = 4, MXLUS = 4, MXSGD = 4, MXSPD = 4, MXSDR = 4, MXSBM = 4, &
       MXDS = 4, MXLEN = 4, MXFLD = 2, MXBMS = 6)
  CHARACTER CBREAD(LINMAX), CINDEX(LINMAX)
  CHARACTER CIDS(LINMAX), CGDS(LINMAX)

  LOCLUS = 0
  IRET = 0
  MLEN = 0
  NUMFLD = 0
  NULLIFY(CBUF)
  MBUF = INIT
  ALLOCATE(CBUF(MBUF), STAT = ISTAT)    ! ALLOCATE INITIAL SPACE FOR CBUF
  IF (ISTAT .NE. 0) THEN
     IRET = 1
     RETURN
  ENDIF

  ! Read sections 0 and 1 for versin number and discipline.
  IBREAD = MIN(LGRIB, LINMAX)
  CALL BAREAD(LUGB, LSKIP, IBREAD, LBREAD, CBREAD)
  IF(LBREAD .NE. IBREAD) THEN
     IRET = 2
     RETURN
  ENDIF
  IF(CBREAD(8) .NE. CHAR(2)) THEN          !  NOT GRIB EDITION 2
     IRET = 3
     RETURN
  ENDIF
  CVER = CBREAD(8)
  CDISC = CBREAD(7)
  CALL G2_GBYTEC(CBREAD, LENSEC1, 16 * 8, 4 * 8)
  LENSEC1 = MIN(LENSEC1, IBREAD)
  CIDS(1:LENSEC1) = CBREAD(17:16 + LENSEC1)
  IBSKIP = LSKIP + 16 + LENSEC1

  ! Loop through remaining sections creating an index for each field.
  IBREAD = MAX(5, MXBMS)
  DO
     CALL BAREAD(LUGB, IBSKIP, IBREAD, LBREAD, CBREAD)
     CTEMP = CBREAD(1)//CBREAD(2)//CBREAD(3)//CBREAD(4)
     IF (CTEMP .EQ. '7777') RETURN        ! END OF MESSAGE FOUND
     IF(LBREAD .NE. IBREAD) THEN
        IRET = 2
        RETURN
     ENDIF
     CALL G2_GBYTEC(CBREAD, LENSEC, 0 * 8, 4 * 8)
     CALL G2_GBYTEC(CBREAD, NUMSEC, 4 * 8, 1 * 8)

     IF (NUMSEC .EQ. 2) THEN                 ! SAVE LOCAL USE LOCATION
        LOCLUS = IBSKIP-LSKIP
     ELSEIF (NUMSEC .EQ. 3) THEN                 ! SAVE GDS INFO
        LENGDS = LENSEC
        CGDS = CHAR(0)
        CALL BAREAD(LUGB, IBSKIP, LENGDS, LBREAD, CGDS)
        IF (LBREAD .NE. LENGDS) THEN
           IRET = 2
           RETURN
        ENDIF
        LOCGDS = IBSKIP-LSKIP
     ELSEIF (NUMSEC .EQ. 4) THEN                 ! FOUND PDS
        CINDEX = CHAR(0)
        CALL G2_SBYTEC(CINDEX, LSKIP, 8 * IXSKP, 8 * MXSKP)    ! BYTES TO SKIP
        CALL G2_SBYTEC(CINDEX, LOCLUS, 8 * IXLUS, 8 * MXLUS)   ! LOCATION OF LOCAL USE
        CALL G2_SBYTEC(CINDEX, LOCGDS, 8 * IXSGD, 8 * MXSGD)   ! LOCATION OF GDS
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSPD, 8 * MXSPD)  ! LOCATION OF PDS
        CALL G2_SBYTEC(CINDEX, LGRIB, 8 * IXLEN, 8 * MXLEN)    ! LEN OF GRIB2
        CINDEX(41) = CVER
        CINDEX(42) = CDISC
        CALL G2_SBYTEC(CINDEX, NUMFLD + 1, 8 * IXFLD, 8 * MXFLD)   ! FIELD NUM
        CINDEX(IXIDS + 1:IXIDS + LENSEC1) = CIDS(1:LENSEC1)
        LINDEX = IXIDS + LENSEC1
        CINDEX(LINDEX + 1:LINDEX + LENGDS) = CGDS(1:LENGDS)
        LINDEX = LINDEX + LENGDS
        ILNPDS = LENSEC
        CALL BAREAD(LUGB, IBSKIP, ILNPDS, LBREAD, CINDEX(LINDEX + 1))
        IF (LBREAD .NE. ILNPDS) THEN
           IRET = 2
           RETURN
        ENDIF
        LINDEX = LINDEX + ILNPDS
     ELSEIF (NUMSEC .EQ. 5) THEN                 ! FOUND DRS
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSDR, 8 * MXSDR)  ! LOCATION OF DRS
        ILNDRS = LENSEC
        CALL BAREAD(LUGB, IBSKIP, ILNDRS, LBREAD, CINDEX(LINDEX + 1))
        IF (LBREAD .NE. ILNDRS) THEN
           IRET = 2
           RETURN
        ENDIF
        LINDEX = LINDEX + ILNDRS
     ELSEIF (NUMSEC .EQ. 6) THEN                 ! FOUND BMS
        INDBMP = MOVA2I(CBREAD(6))
        IF (INDBMP.LT.254) THEN
           LOCBMS = IBSKIP-LSKIP
           CALL G2_SBYTEC(CINDEX, LOCBMS, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ELSEIF (INDBMP.EQ.254) THEN
           CALL G2_SBYTEC(CINDEX, LOCBMS, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ELSEIF (INDBMP.EQ.255) THEN
           CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ENDIF
        CINDEX(LINDEX + 1:LINDEX + MXBMS) = CBREAD(1:MXBMS)
        LINDEX = LINDEX + MXBMS
        CALL G2_SBYTEC(CINDEX, LINDEX, 0, 8 * 4)    ! NUM BYTES IN INDEX RECORD
     ELSEIF (NUMSEC .EQ. 7) THEN                 ! FOUND DATA SECTION
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXDS, 8 * MXDS)   ! LOC. OF DATA SEC.
        NUMFLD = NUMFLD + 1
        IF ((LINDEX + MLEN) .GT. MBUF) THEN ! ALLOCATE MORE SPACE IF NECESSARY
           NEWSIZE = MAX(MBUF + NEXT, MBUF + LINDEX)
           CALL REALLOC(CBUF, MLEN, NEWSIZE, ISTAT)
           IF (ISTAT .NE. 0) THEN
              NUMFLD = NUMFLD-1
              IRET = 4
              RETURN
           ENDIF
           MBUF = NEWSIZE
        ENDIF
        CBUF(MLEN + 1:MLEN + LINDEX) = CINDEX(1:LINDEX)
        MLEN = MLEN + LINDEX
     ELSE                           ! UNRECOGNIZED SECTION
        IRET = 5
        RETURN
     ENDIF
     IBSKIP = IBSKIP + LENSEC
  ENDDO
END SUBROUTINE IXGB2
