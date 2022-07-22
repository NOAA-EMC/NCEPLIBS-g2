!> @file
!> @brief This subroutine finds and extracts a GRIB2 message from a
!> file.
!> @author Mark Iredell @date 1994-04-01

!> This subroutine finds and extracts a GRIB2 message from a file.
!>
!> This subroutine reads a grib index file (or optionally the grib
!> file itself) to get the index buffer (i.e. table of contents) for
!> the grib file.  It finds in the index buffer a reference to the
!> grib field requested.
!>
!> The grib field request specifies the number of fields to skip and
!> the unpacked identification section, grid definition template and
!> product defintion section parameters. (A requested parameter of
!> -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested grib field is found, then it is read from the grib
!> file and unpacked. If the grib field is not found, then the return
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
!> 2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with grib2
!> 2003-12-17 | Stephen Gilbert | modified from getgb2 to return packed grib2 message
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] LUGI integer unit of the unblocked grib index file.  if
!> nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine. (set to 0 to get index buffer from the grib file),
!> @param[in] J integer number of fields to skip (set to 0 to search
!> from beginning)
!> @param[in] JDISC grib2 discipline number of requested field (if =
!> -1, accept any discipline see code table 0.0)
!> - 0 meteorological products
!> - 1 hydrological products
!> - 2 land surface products
!> - 3 space products
!> - 10 oceanographic products
!> @param[in] JIDS integer array of values in the identification
!> section (set to -9999 for wildcard).
!> - JIDS(1) identification of originating centre
!> (see common code table c-1)
!> - JIDS(2) identification of originating sub-centre
!> - JIDS(3) grib master tables version number
!> (see code table 1.0) 0 experimental;1 initial operational version number.
!> - JIDS(4) grib local tables version number (see code table 1.1)
!> 0 local tables not used; 1-254 number of local tables version used.
!> - JIDS(5) significance of reference time (code table 1.2)
!> 0 analysis; 1 start of forecast; 2 verifying time of forecast; 3 observation time
!> - JIDS(6) year (4 digits)
!> - JIDS(7) month
!> - JIDS(8) day
!> - JIDS(9) hour
!> - JIDS(10) minute
!> - JIDS(11) second
!> - JIDS(12) production status of processed data (see code table 1.3)
!> 0 operational products; 1 operational test products;
!> 2 research products; 3 re-analysis products.
!> - JIDS(13) type of processed data (see code table 1.4)
!> 0 analysis products; 1 forecast products; 2 analysis and forecast
!> products; 3 control forecast products; 4 perturbed forecast products;
!> 5 control and perturbed forecast products; 6 processed satellite
!> observations; 7 processed radar observations.
!> @param[in] JPDTN integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] JGDTN integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[in] EXTRACT logical value indicating whether to return a
!> grib2 message with just the requested field, or the entire
!> grib2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] K integer field number unpacked.
!> @param[out] GRIBM returned grib message.
!> @param[out] LENG length of returned grib message in bytes.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!>
!> @note Specify an index file if feasible to increase speed.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 1994-04-01
SUBROUTINE GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
     EXTRACT,K,GRIBM,LENG,IRET)

  USE GRIB_MOD
  implicit none

  INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
  INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
  LOGICAL,INTENT(IN) :: EXTRACT
  INTEGER,INTENT(OUT) :: K,IRET,LENG
  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM

  TYPE(GRIBFIELD) :: GFLD
  integer :: msk1, irgi, irgs, jk, lpos, lux, msk2, mskp, nlen, nmess, nnum

  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
  PARAMETER(MSK1=32000,MSK2=4000)


  SAVE CBUF,NLEN,NNUM
  DATA LUX/0/

  !  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)
  INTERFACE
     SUBROUTINE GETG2I(LUGI,CBUF,NLEN,NNUM,IRET)
       CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
       INTEGER,INTENT(IN) :: LUGI
       INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
     END SUBROUTINE GETG2I
     SUBROUTINE GETG2IR(LUGB,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM, &
          NMESS,IRET)
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

  !  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
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

  !  SEARCH INDEX BUFFER
  CALL GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
       JK,GFLD,LPOS,IRGS)
  IF(IRGS.NE.0) THEN
     IRET=99
     CALL GF_FREE(GFLD)
     RETURN
  ENDIF
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !  EXTRACT GRIB MESSAGE FROM FILE
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

  K=JK
  CALL GF_FREE(GFLD)

  RETURN
END SUBROUTINE GETGB2P
