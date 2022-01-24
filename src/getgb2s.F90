!>    @file
!>    @brief This subroutine find a reference to the grib field
!>    requested in the index file.
!>    @author Stephen Gilbert @date 2002-01-15
!>

!>    This subroutine find in the index file for a reference to the grib
!>    field requested. The grib field request specifies the number of
!>    messages to skip and the unpacked identification section, grid
!>    definition template and product defintion section parameters.
!>    (a requested parameter of -9999 means to allow any value of this
!>    parameter to be found.)
!>    Each index record has the following form:
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
!>    - byte 041 - 041 grib version number ( currently 2 )
!>    - byte 042 - 042 message discipline
!>    - byte 043 - 044 field number within grib2 message
!>    - byte 045 -  ii identification section (ids)
!>    - byte ii+1-  jj grid definition section (gds)
!>    - byte jj+1-  kk product definition section (pds)
!>    - byte kk+1-  ll the data representation section (drs)
!>    - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>    Most of the decoded information for the selected grib field is
!>    returned in a derived type variable, GFLD. GFLD is of type
!>    gribfield, which is defined in module grib_mod, so users of this
!>    routine will need to include the line "use grib_mod" in their
!>    calling routine. Each component of the gribfield type is described
!>    in the output argument list section below. Only the unpacked
!>    bitmap and data field components are not set by this routine.
!>
!>    Program History log:
!>    - 1995-10-31  Mark Iredell Initial development
!>    - 2002-01-02  Stephen Gilbert Modified from getg1s to work with grib2
!>    - 2011-06-24  Boi Vuong initialize variable gfld%idsect and gfld%local
!>
!>    @param[in] CBUF character*1 (nlen) buffer containing index data.
!>    @param[in] NLEN integer total length of all index records.
!>    @param[in] NNUM integer number of index records.
!>    @param[in] J integer number of fields to skip
!>    (=0 to search from beginning)
!>    @param[in] JDISC grib2 discipline number of requested field
!>    (if = -1, accept any discipline see code table 0.0)
!>    - 0 meteorological products
!>    - 1 hydrological products
!>    - 2 land surface products
!>    - 3 space products
!>    - 10 oceanographic products
!>    @param[in] JIDS integer array of values in the identification section
!>    (=-9999 for wildcard)
!>    - JIDS(1) identification of originating centre
!>    (see common code table c-1)
!>    - JIDS(2) identification of originating sub-centre
!>    - JIDS(3) grib master tables version number
!>    (see code table 1.0)
!>     - 0 experimental
!>     - 1 initial operational version number.
!>    - JIDS(4) grib local tables version number (see code table 1.1)
!>     - 0 local tables not used
!>     - 1-254 number of local tables version used.
!>    - JIDS(5) significance of reference time (code table 1.2)
!>     - 0 analysis
!>     - 1 start of forecast
!>     - 2 verifying time of forecast
!>     - 3 observation time
!>    - JIDS(6) year (4 digits)
!>    - JIDS(7) month
!>    - JIDS(8) day
!>    - JIDS(9) hour
!>    - JIDS(10) minute
!>    - JIDS(11) second
!>    - JIDS(12) production status of processed data (see code table 1.3)
!>     - 0 operational products
!>     - 1 operational test products;
!>     - 2 research products
!>     - 3 re-analysis products.
!>    - JIDS(13) type of processed data (see code table 1.4)
!>     - 0 analysis products
!>     - 1 forecast products
!>     - 2 analysis and forecast products
!>     - 3 control forecast products
!>     - 4 perturbed forecast products
!>     - 5 control and perturbed forecast products
!>     - 6 processed satellite observations
!>     - 7 processed radar observations.
!>    @param[in] JPDTN integer product definition template number (n)
!>    (if = -1, don't bother matching pdt - accept any)
!>    @param[in] JPDT integer array of values defining the product definition
!>    template 4.n of the field for which to search (=-9999 for wildcard)
!>    @param[in] JGDTN integer grid definition template number (m)
!>    (if = -1, don't bother matching gdt - accept any )
!>    @param[in] JGDT integer array of values defining the grid definition
!>    template 3.m of the field for which to search (=-9999 for wildcard)
!>    @param[out] K integer field number unpacked.
!>    @param[out] GFLD derived type @ref grib_mod::gribfield.
!>    @param[out] LPOS starting position of the found index record
!>    within the complete index buffer, CBUF. = 0, if request not found.
!>    @param[out] IRET integer return code
!>    - 0 all ok
!>    - 97 error reading grib file
!>    - other gf_getfld grib2 unpacker return code
!>
!>    @note This subprogram is intended for private use by getgb2
!>    routines only. Note that derived type gribfield contains
!>    pointers to many arrays of data. The memory for these arrays is
!>    allocated when the values in the arrays are set, to help
!>    minimize problems with array overloading. Users should free this
!>    memory, when it is no longer needed, by a call to subroutine
!>    gf_free().
!>
!>    @author Stephen Gilbert @date 2002-01-15
SUBROUTINE GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
     JGDT,K,GFLD,LPOS,IRET)

  USE GRIB_MOD

  !      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
  CHARACTER(LEN=1),INTENT(IN) :: CBUF(NLEN)
  INTEGER,INTENT(IN) :: NLEN,NNUM,J,JDISC,JPDTN,JGDTN
  INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
  INTEGER,INTENT(OUT) :: K,LPOS,IRET
  TYPE(GRIBFIELD),INTENT(OUT) :: GFLD

  INTEGER :: KGDS(5)
  LOGICAL :: MATCH1,MATCH3,MATCH4
  !      INTEGER,POINTER,DIMENSION(:) :: KIDS,KPDT,KGDT
  !      INTEGER,POINTER,DIMENSION(:) :: IDEF
  !      REAL,POINTER,DIMENSION(:) :: COORD

  interface
     subroutine gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: ids
       integer,intent(out) :: ierr,idslen
     end subroutine gf_unpack1
     subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl, &
          mapgridlen,ideflist,idefnum,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: igdstmpl,ideflist
       integer,intent(out) :: igds(5)
       integer,intent(out) :: ierr,idefnum
     end subroutine gf_unpack3
     subroutine gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl, &
          mappdslen,coordlist,numcoord,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       real,pointer,dimension(:) :: coordlist
       integer,pointer,dimension(:) :: ipdstmpl
       integer,intent(out) :: ipdsnum
       integer,intent(out) :: ierr,numcoord
     end subroutine gf_unpack4
     subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum, &
          idrstmpl,mapdrslen,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,intent(out) :: ndpts,idrsnum
       integer,pointer,dimension(:) :: idrstmpl
       integer,intent(out) :: ierr
     end subroutine gf_unpack5
  end interface

  !  INITIALIZE
  K=0
  LPOS=0
  IRET=1
  IPOS=0
  nullify(gfld%idsect,gfld%local)
  nullify(gfld%list_opt,gfld%igdtmpl,gfld%ipdtmpl)
  nullify(gfld%coord_list,gfld%idrtmpl,gfld%bmap,gfld%fld)

  !  SEARCH FOR REQUEST
  DO WHILE(IRET.NE.0.AND.K.LT.NNUM)
     K=K+1
     CALL G2_GBYTEC(CBUF,INLEN,IPOS*8,4*8)    ! GET LENGTH OF CURRENT
     ! INDEX RECORD
     IF ( K.LE.J ) THEN           ! SKIP THIS INDEX
        IPOS=IPOS+INLEN
        CYCLE
     ENDIF

     !  CHECK IF GRIB2 DISCIPLINE IS A MATCH
     CALL G2_GBYTEC(CBUF,GFLD%DISCIPLINE,(IPOS+41)*8,1*8)
     IF ( (JDISC.NE.-1).AND.(JDISC.NE.GFLD%DISCIPLINE) ) THEN
        IPOS=IPOS+INLEN
        CYCLE
     ENDIF

     !  CHECK IF IDENTIFICATION SECTION IS A MATCH
     MATCH1=.FALSE.
     CALL G2_GBYTEC(CBUF,LSEC1,(IPOS+44)*8,4*8)  ! GET LENGTH OF IDS
     IOF=0
     CALL GF_UNPACK1(CBUF(IPOS+45),LSEC1,IOF,GFLD%IDSECT, &
          GFLD%IDSECTLEN,ICND)
     IF ( ICND.EQ.0 ) THEN
        MATCH1=.TRUE.
        DO I=1,GFLD%IDSECTLEN
           IF ( (JIDS(I).NE.-9999).AND. &
                (JIDS(I).NE.GFLD%IDSECT(I)) ) THEN
              MATCH1=.FALSE.
              EXIT
           ENDIF
        ENDDO
     ENDIF
     IF ( .NOT. MATCH1 ) THEN
        DEALLOCATE(GFLD%IDSECT)
        IPOS=IPOS+INLEN
        CYCLE
     ENDIF

     !  CHECK IF GRID DEFINITION TEMPLATE IS A MATCH
     JPOS=IPOS+44+LSEC1
     MATCH3=.FALSE.
     CALL G2_GBYTEC(CBUF,LSEC3,JPOS*8,4*8)  ! GET LENGTH OF GDS
     IF ( JGDTN.EQ.-1 ) THEN
        MATCH3=.TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF,NUMGDT,(JPOS+12)*8,2*8)  ! GET GDT TEMPLATE NO.
        IF ( JGDTN.EQ.NUMGDT ) THEN
           IOF=0
           CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL, &
                GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
           IF ( ICND.EQ.0 ) THEN
              MATCH3=.TRUE.
              DO I=1,GFLD%IGDTLEN
                 IF ( (JGDT(I).NE.-9999).AND. &
                      (JGDT(I).NE.GFLD%IGDTMPL(I)) ) THEN
                    MATCH3=.FALSE.
                    EXIT
                 ENDIF
              ENDDO
              !                 WHERE ( JGDT(1:GFLD%IGDTLEN).NE.-9999 )  &
              !                   MATCH3=ALL(JGDT(1:GFLD%IGDTLEN).EQ.GFLD%IGDTMPL(1:GFLD%IGDTLEN))
           ENDIF
        ENDIF
     ENDIF
     IF ( .NOT. MATCH3 ) THEN
        IF (ASSOCIATED(GFLD%IGDTMPL)) DEALLOCATE(GFLD%IGDTMPL)
        IF (ASSOCIATED(GFLD%LIST_OPT)) DEALLOCATE(GFLD%LIST_OPT)
        IPOS=IPOS+INLEN
        CYCLE
     ELSE
        GFLD%GRIDDEF=KGDS(1)
        GFLD%NGRDPTS=KGDS(2)
        GFLD%NUMOCT_OPT=KGDS(3)
        GFLD%INTERP_OPT=KGDS(4)
        GFLD%IGDTNUM=KGDS(5)
     ENDIF

     !  CHECK IF PRODUCT DEFINITION TEMPLATE IS A MATCH
     JPOS=JPOS+LSEC3
     MATCH4=.FALSE.
     CALL G2_GBYTEC(CBUF,LSEC4,JPOS*8,4*8)  ! GET LENGTH OF PDS
     IF ( JPDTN.EQ.-1 ) THEN
        MATCH4=.TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF,NUMPDT,(JPOS+7)*8,2*8)  ! GET PDT TEMPLATE NO.
        IF ( JPDTN.EQ.NUMPDT ) THEN
           IOF=0
           CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM, &
                GFLD%IPDTMPL,GFLD%IPDTLEN, &
                GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
           IF ( ICND.EQ.0 ) THEN
              MATCH4=.TRUE.
              DO I=1,GFLD%IPDTLEN
                 IF ( (JPDT(I).NE.-9999).AND. &
                      (JPDT(I).NE.GFLD%IPDTMPL(I)) ) THEN
                    MATCH4=.FALSE.
                    EXIT
                 ENDIF
              ENDDO
              !                 WHERE ( JPDT.NE.-9999)  &
              !                        MATCH4=ALL( JPDT(1:GFLD%IPDTLEN) .EQ. GFLD%IPDTMPL(1:GFLD%IPDTLEN) )
           ENDIF
        ENDIF
     ENDIF
     IF ( .NOT. MATCH4 ) THEN
        IF (ASSOCIATED(GFLD%IPDTMPL)) DEALLOCATE(GFLD%IPDTMPL)
        IF (ASSOCIATED(GFLD%COORD_LIST)) DEALLOCATE(GFLD%COORD_LIST)
     ENDIF

     !  IF REQUEST IS FOUND
     !  SET VALUES FOR DERIVED TYPE GFLD AND RETURN
     IF(MATCH1.AND.MATCH3.AND.MATCH4) THEN
        LPOS=IPOS+1
        CALL G2_GBYTEC(CBUF,GFLD%VERSION,(IPOS+40)*8,1*8)
        CALL G2_GBYTEC(CBUF,GFLD%IFLDNUM,(IPOS+42)*8,2*8)
        GFLD%UNPACKED=.FALSE.
        JPOS=IPOS+44+LSEC1
        IF ( JGDTN.EQ.-1 ) THEN     ! UNPACK GDS, IF NOT DONE BEFORE
           IOF=0
           CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL, &
                GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
           GFLD%GRIDDEF=KGDS(1)
           GFLD%NGRDPTS=KGDS(2)
           GFLD%NUMOCT_OPT=KGDS(3)
           GFLD%INTERP_OPT=KGDS(4)
           GFLD%IGDTNUM=KGDS(5)
        ENDIF
        JPOS=JPOS+LSEC3
        IF ( JPDTN.EQ.-1 ) THEN     ! UNPACK PDS, IF NOT DONE BEFORE
           IOF=0
           CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM, &
                GFLD%IPDTMPL,GFLD%IPDTLEN, &
                GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
        ENDIF
        JPOS=JPOS+LSEC4
        CALL G2_GBYTEC(CBUF,LSEC5,JPOS*8,4*8)  ! GET LENGTH OF DRS
        IOF=0
        CALL GF_UNPACK5(CBUF(JPOS+1),LSEC5,IOF,GFLD%NDPTS, &
             GFLD%IDRTNUM,GFLD%IDRTMPL, &
             GFLD%IDRTLEN,ICND)
        JPOS=JPOS+LSEC5
        CALL G2_GBYTEC(CBUF,GFLD%IBMAP,(JPOS+5)*8,1*8)  ! GET IBMAP
        IRET=0
     ELSE      ! PDT DID NOT MATCH
        IPOS=IPOS+INLEN
     ENDIF
  ENDDO

  RETURN
END SUBROUTINE GETGB2S
