!> @file
!> @brief Find information about a GRIB field from the index.
!> file and fill a @ref grib_mod::gribfield.
!> @author Stephen Gilbert @date 2002-01-15

!> Find information about a GRIB field from the index and fill a @ref
!> grib_mod::gribfield.
!>
!> For a description of the index record see getg2i().
!>
!> Users of this routine will need to include the line "use grib_mod"
!> in their calling routine.
!>
!> The unpacked bitmap and bitmap data field are the only components
!> of the @ref grib_mod::gribfield type not set by this routine.
!>
!> @note This subprogram is intended for private use by getgb2()
!> routines only. Note that derived type @ref grib_mod::gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set. Users must free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @param[in] cbuf Buffer (of size nlen bytes) containing index data.
!> @param[in] nlen Total length of all index records.
!> @param[in] nnum Number of index records.
!> @param[in] j Number of fields to skip (0 to search from beginning).
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
!> (if = -1, don't bother matching PDT - accept any).
!> @param[in] jpdt Array of values defining the Product Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[out] k Field number unpacked.
!> @param[out] gfld Derived type @ref grib_mod::gribfield.
!> @param[out] lpos Starting position of the found index record
!> within the complete index buffer, CBUF. = 0, if request not found.
!> @param[out] iret integer return code:
!> - 0 No error.
!> - 97 Error reading GRIB file.
!> - other gf_getfld GRIB2 unpacker return code.
!>
!> @author Stephen Gilbert @date 2002-01-15
SUBROUTINE GETGB2S(CBUF, NLEN, NNUM, J, JDISC, JIDS, JPDTN, JPDT, JGDTN, &
     JGDT, K, GFLD, LPOS, IRET)
  USE GRIB_MOD

  CHARACTER(LEN = 1), INTENT(IN) :: CBUF(NLEN)
  INTEGER, INTENT(IN) :: NLEN, NNUM, J, JDISC, JPDTN, JGDTN
  INTEGER, DIMENSION(:) :: JIDS(*), JPDT(*), JGDT(*)
  INTEGER, INTENT(OUT) :: K, LPOS, IRET
  TYPE(GRIBFIELD), INTENT(OUT) :: GFLD

  INTEGER :: KGDS(5)
  LOGICAL :: MATCH1, MATCH3, MATCH4

  interface
     subroutine gf_unpack1(cgrib, lcgrib, iofst, ids, idslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: ids
       integer, intent(out) :: ierr, idslen
     end subroutine gf_unpack1
     subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
          mapgridlen, ideflist, idefnum, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: igdstmpl, ideflist
       integer, intent(out) :: igds(5)
       integer, intent(out) :: ierr, idefnum
     end subroutine gf_unpack3
     subroutine gf_unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
          mappdslen, coordlist, numcoord, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       real, pointer, dimension(:) :: coordlist
       integer, pointer, dimension(:) :: ipdstmpl
       integer, intent(out) :: ipdsnum
       integer, intent(out) :: ierr, numcoord
     end subroutine gf_unpack4
     subroutine gf_unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum, &
          idrstmpl, mapdrslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: ndpts, idrsnum
       integer, pointer, dimension(:) :: idrstmpl
       integer, intent(out) :: ierr
     end subroutine gf_unpack5
  end interface

  !     INITIALIZE
  K = 0
  LPOS = 0
  IRET = 1
  IPOS = 0
  nullify(gfld%idsect, gfld%local)
  nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
  nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)

  !     SEARCH FOR REQUEST
  DO WHILE(IRET.NE.0 .and. K.LT.NNUM)
     K = K + 1
     CALL G2_GBYTEC(CBUF, INLEN, IPOS * 8, 4 * 8)    ! GET LENGTH OF CURRENT
     ! INDEX RECORD
     IF (K.LE.J) THEN           ! SKIP THIS INDEX
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF GRIB2 DISCIPLINE IS A MATCH
     CALL G2_GBYTEC(CBUF, GFLD%DISCIPLINE, (IPOS + 41)*8, 1*8)
     IF ((JDISC.NE.-1) .and. (JDISC.NE.GFLD%DISCIPLINE)) THEN
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF IDENTIFICATION SECTION IS A MATCH
     MATCH1 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC1, (IPOS + 44) * 8, 4 * 8)  ! GET LENGTH OF IDS 
     IOF = 0
     CALL GF_UNPACK1(CBUF(IPOS + 45), LSEC1, IOF, GFLD%IDSECT, GFLD%IDSECTLEN, ICND)
     IF (ICND .eq. 0) THEN
        MATCH1 = .TRUE.
        DO I = 1, GFLD%IDSECTLEN
           IF ((JIDS(I).NE.-9999) .and.  (JIDS(I).NE.GFLD%IDSECT(I))) THEN
              MATCH1 = .FALSE.
              EXIT
           ENDIF
        ENDDO
     ENDIF
     IF (.NOT. MATCH1) THEN
        DEALLOCATE(GFLD%IDSECT)
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF GRID DEFINITION TEMPLATE IS A MATCH
     JPOS = IPOS + 44 + LSEC1
     MATCH3 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC3, JPOS * 8, 4 * 8)  ! GET LENGTH OF GDS 
     IF (JGDTN .eq. -1) THEN
        MATCH3 = .TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF, NUMGDT, (JPOS + 12) * 8, 2 * 8)  ! GET GDT TEMPLATE NO.
        IF (JGDTN .eq. NUMGDT) THEN
           IOF = 0
           CALL GF_UNPACK3(CBUF(JPOS + 1), LSEC3, IOF, KGDS, GFLD%IGDTMPL, &
                GFLD%IGDTLEN, GFLD%LIST_OPT, GFLD%NUM_OPT, ICND)
           IF (ICND .eq. 0) THEN
              MATCH3 = .TRUE.
              DO I = 1, GFLD%IGDTLEN
                 IF ((JGDT(I).NE.-9999) .and.  (JGDT(I).NE.GFLD%IGDTMPL(I))) THEN
                    MATCH3 = .FALSE.
                    EXIT
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     IF (.NOT. MATCH3) THEN
        IF (ASSOCIATED(GFLD%IDSECT)) DEALLOCATE(GFLD%IDSECT)
        IF (ASSOCIATED(GFLD%IGDTMPL)) DEALLOCATE(GFLD%IGDTMPL)
        IF (ASSOCIATED(GFLD%LIST_OPT)) DEALLOCATE(GFLD%LIST_OPT)
        IPOS = IPOS + INLEN
        CYCLE
     ELSE
        GFLD%GRIDDEF = KGDS(1)
        GFLD%NGRDPTS = KGDS(2)
        GFLD%NUMOCT_OPT = KGDS(3)
        GFLD%INTERP_OPT = KGDS(4)
        GFLD%IGDTNUM = KGDS(5)
     ENDIF

     !     CHECK IF PRODUCT DEFINITION TEMPLATE IS A MATCH
     JPOS = JPOS + LSEC3
     MATCH4 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC4, JPOS * 8, 4 * 8)  ! GET LENGTH OF PDS 
     IF (JPDTN .eq. -1) THEN
        MATCH4 = .TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF, NUMPDT, (JPOS + 7) * 8, 2 * 8)  ! GET PDT TEMPLATE NO.
        IF (JPDTN .eq. NUMPDT) THEN
           IOF = 0
           CALL GF_UNPACK4(CBUF(JPOS + 1), LSEC4, IOF, GFLD%IPDTNUM, &
                GFLD%IPDTMPL, GFLD%IPDTLEN, GFLD%COORD_LIST, GFLD%NUM_COORD, ICND)
           IF (ICND .eq. 0) THEN
              MATCH4 = .TRUE.
              DO I = 1, GFLD%IPDTLEN
                 IF ((JPDT(I).NE.-9999) .and.  (JPDT(I).NE.GFLD%IPDTMPL(I))) THEN
                    MATCH4 = .FALSE.
                    EXIT
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     IF (.NOT. MATCH4) THEN
        IF (ASSOCIATED(GFLD%IDSECT)) DEALLOCATE(GFLD%IDSECT)
        IF (ASSOCIATED(GFLD%IPDTMPL)) DEALLOCATE(GFLD%IPDTMPL)
        IF (ASSOCIATED(GFLD%COORD_LIST)) DEALLOCATE(GFLD%COORD_LIST)
     ENDIF

     !     IF REQUEST IS FOUND
     !     SET VALUES FOR DERIVED TYPE GFLD AND RETURN
     IF(MATCH1 .and. MATCH3 .and. MATCH4) THEN
        LPOS = IPOS + 1
        CALL G2_GBYTEC(CBUF, GFLD%VERSION, (IPOS + 40) * 8, 1 * 8)
        CALL G2_GBYTEC(CBUF, GFLD%IFLDNUM, (IPOS + 42) * 8, 2 * 8)
        GFLD%UNPACKED = .FALSE.
        JPOS = IPOS + 44 + LSEC1
        IF (JGDTN.EQ.-1) THEN     ! UNPACK GDS, IF NOT DONE BEFORE
           IOF = 0
           CALL GF_UNPACK3(CBUF(JPOS + 1), LSEC3, IOF, KGDS, GFLD%IGDTMPL, &
                GFLD%IGDTLEN, GFLD%LIST_OPT, GFLD%NUM_OPT, ICND)
           GFLD%GRIDDEF = KGDS(1)
           GFLD%NGRDPTS = KGDS(2)
           GFLD%NUMOCT_OPT = KGDS(3)
           GFLD%INTERP_OPT = KGDS(4)
           GFLD%IGDTNUM = KGDS(5)
        ENDIF
        JPOS = JPOS + LSEC3
        IF (JPDTN.EQ.-1 ) THEN     ! UNPACK PDS, IF NOT DONE BEFORE
           IOF = 0
           CALL GF_UNPACK4(CBUF(JPOS + 1), LSEC4, IOF, GFLD%IPDTNUM, &
                GFLD%IPDTMPL, GFLD%IPDTLEN, GFLD%COORD_LIST, GFLD%NUM_COORD, ICND)
        ENDIF
        JPOS = JPOS + LSEC4
        CALL G2_GBYTEC(CBUF, LSEC5, JPOS * 8, 4 * 8)  ! GET LENGTH OF DRS 
        IOF = 0
        CALL GF_UNPACK5(CBUF(JPOS + 1), LSEC5, IOF, GFLD%NDPTS, &
             GFLD%IDRTNUM, GFLD%IDRTMPL, GFLD%IDRTLEN, ICND)
        JPOS = JPOS + LSEC5
        CALL G2_GBYTEC(CBUF, GFLD%IBMAP, (JPOS + 5)*8, 1 * 8)  ! GET IBMAP
        IRET = 0
     ELSE      ! PDT DID NOT MATCH
        IPOS = IPOS+INLEN
     ENDIF
  ENDDO
END SUBROUTINE GETGB2S
