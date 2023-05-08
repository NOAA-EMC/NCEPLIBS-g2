!> @file
!> @brief Find and extract a GRIB2 message from a file.
!> @author Mark Iredell @date 1994-04-01

!> Find and extract a GRIB2 message from a file.
!>
!> This subroutine reads a GRIB index file (or optionally the GRIB
!> file itself) to get the index buffer (i.e. table of contents) for
!> the GRIB file. It finds in the index buffer a reference to the
!> GRIB field requested.
!>
!> The GRIB field request specifies the number of fields to skip and
!> the unpacked identification section, grid definition template and
!> product defintion section parameters. (A requested parameter of
!> -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested GRIB field is found, then it is read from the GRIB
!> file and unpacked. If the GRIB field is not found, then the return
!> code will be nonzero.
!>
!> Note that derived type @ref grib_mod::gribfield contains pointers
!> to many arrays of data. The memory for these arrays is allocated
!> when the values in the arrays are set, to help minimize problems
!> with array overloading. Because of this users are should free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1994-04-01 | Mark Iredell | Initial
!> 1995-10-31 | Mark Iredell | modularized portions of code into subprograms and allowed for unspecified index file
!> 2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with GRIB2
!> 2003-12-17 | Stephen Gilbert | modified from getgb2 to return packed GRIB2 message
!>
!> @param[in] lugb Unit of the unblocked GRIB data file. The
!> file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] lugi Unit of the unblocked GRIB index file. If
!> nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> subroutine. Set to 0 to get index buffer from the GRIB file.
!> @param[in] J Number of fields to skip (set to 0 to search
!> from beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field. See
!> [GRIB2 - TABLE 0.0 -
!> DISCIPLINE](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml).
!> Use -1 to accept any discipline.
!> @param[in] jids Array of values in the identification
!> section. (Set to -9999 for wildcard.)
!> - jids(1) Identification of originating centre. See [TABLE 0 -
!>   NATIONAL/INTERNATIONAL ORIGINATING
!>   CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html).
!> - jids(2) Identification of originating sub-centre. See [TABLE C -
!>   NATIONAL
!>   SUB-CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html).
!> - jids(3) GRIB master tables version number. See [GRIB2 - TABLE 1.0
!>   - GRIB Master Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml).
!> - jids(4) GRIB local tables version number. See [GRIB2 - TABLE 1.1
!>   - GRIB Local Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml).
!> - jids(5) Significance of reference time. See [GRIB2 - TABLE 1.2 -
!>   Significance of Reference
!>   Time](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-2.shtml).
!> - jids(6) year (4 digits)
!> - jids(7) month
!> - jids(8) day
!> - jids(9) hour
!> - jids(10) minute
!> - jids(11) second
!> - jids(12) Production status of processed data. See [GRIB2 - TABLE
!>   1.3 - Production Status of
!>   Data](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml).
!> - jids(13) Type of processed data. See [GRIB2 - TABLE 1.4 - TYPE OF
!>   DATA](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml).
!> @param[in] jpdtn Product Definition Template (PDT) number (n)
!> (if = -1, don't bother matching PDT - accept any)
!> @param[in] jpdt Array of values defining the Product Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] extract value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. return GRIB2 message containing only the requested field.
!> - .false. return entire GRIB2 message containing the requested field.
!> @param[out] k field number unpacked.
!> @param[out] gribm returned GRIB message.
!> @param[out] leng length of returned GRIB message in bytes.
!> @param[out] iret integer return code
!> - 0 No error.
!> - 96 Error reading index.
!> - 97 Error reading GRIB file.
!> - 99 Request not found.
!>
!> @note Specify an index file if feasible to increase speed.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 1994-04-01
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
