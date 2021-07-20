C>    @file
C>    @brief This subroutine find a reference to the grib field
C>    requested in the index file.
C>    @author Stephen Gilbert @date 2002-01-15
C>

C>    This subroutine find in the index file for a reference to the grib
C>    field requested. The grib field request specifies the number of
C>    messages to skip and the unpacked identification section, grid
C>    definition template and product defintion section parameters.
C>    (a requested parameter of -9999 means to allow any value of this
C>    parameter to be found.)
C>    Each index record has the following form:
C>    - byte 001 - 004 length of index record
C>    - byte 005 - 008 bytes to skip in data file before grib message
C>    - byte 009 - 012 bytes to skip in message before lus (local use)
C>    set = 0, if no local use section in grib2 message.
C>    - byte 013 - 016 bytes to skip in message before gds
C>    - byte 017 - 020 bytes to skip in message before pds
C>    - byte 021 - 024 bytes to skip in message before drs
C>    - byte 025 - 028 bytes to skip in message before bms
C>    - byte 029 - 032 bytes to skip in message before data section
C>    - byte 033 - 040 bytes total in the message
C>    - byte 041 - 041 grib version number ( currently 2 )
C>    - byte 042 - 042 message discipline
C>    - byte 043 - 044 field number within grib2 message
C>    - byte 045 -  ii identification section (ids)
C>    - byte ii+1-  jj grid definition section (gds)
C>    - byte jj+1-  kk product definition section (pds)
C>    - byte kk+1-  ll the data representation section (drs)
C>    - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
C>    Most of the decoded information for the selected grib field is
C>    returned in a derived type variable, GFLD. GFLD is of type
C>    gribfield, which is defined in module grib_mod, so users of this
C>    routine will need to include the line "use grib_mod" in their
C>    calling routine. Each component of the gribfield type is described
C>    in the output argument list section below. Only the unpacked
C>    bitmap and data field components are not set by this routine.
C>    
C>    Program History log:
C>    - 1995-10-31  Mark Iredell Initial development
C>    - 2002-01-02  Stephen Gilbert Modified from getg1s to work with grib2
C>    - 2011-06-24  Vuong Boi initialize variable gfld%idsect and gfld%local
C>
C>    @param[in] CBUF character*1 (nlen) buffer containing index data.
C>    @param[in] NLEN integer total length of all index records.
C>    @param[in] NNUM integer number of index records.
C>    @param[in] J integer number of fields to skip
C>    (=0 to search from beginning)
C>    @param[in] JDISC grib2 discipline number of requested field
C>    (if = -1, accept any discipline see code table 0.0)
C>    - 0 meteorological products
C>    - 1 hydrological products
C>    - 2 land surface products
C>    - 3 space products
C>    - 10 oceanographic products
C>    @param[in] JIDS integer array of values in the identification section
C>    (=-9999 for wildcard)
C>    - JIDS(1) identification of originating centre
C>    (see common code table c-1)
C>    - JIDS(2) identification of originating sub-centre
C>    - JIDS(3) grib master tables version number
C>    (see code table 1.0) 
C>     - 0 experimental
C>     - 1 initial operational version number.
C>    - JIDS(4) grib local tables version number (see code table 1.1)
C>     - 0 local tables not used
C>     - 1-254 number of local tables version used.
C>    - JIDS(5) significance of reference time (code table 1.2)
C>     - 0 analysis
C>     - 1 start of forecast 
C>     - 2 verifying time of forecast
C>     - 3 observation time
C>    - JIDS(6) year (4 digits)
C>    - JIDS(7) month
C>    - JIDS(8) day
C>    - JIDS(9) hour
C>    - JIDS(10) minute
C>    - JIDS(11) second
C>    - JIDS(12) production status of processed data (see code table 1.3)
C>     - 0 operational products
C>     - 1 operational test products;
C>     - 2 research products
C>     - 3 re-analysis products.
C>    - JIDS(13) type of processed data (see code table 1.4)
C>     - 0 analysis products
C>     - 1 forecast products
C>     - 2 analysis and forecast products
C>     - 3 control forecast products
C>     - 4 perturbed forecast products
C>     - 5 control and perturbed forecast products
C>     - 6 processed satellite observations
C>     - 7 processed radar observations.
C>    @param[in] JPDTN integer product definition template number (n)
C>    (if = -1, don't bother matching pdt - accept any)
C>    @param[in] JPDT integer array of values defining the product definition
C>    template 4.n of the field for which to search (=-9999 for wildcard)
C>    @param[in] JGDTN integer grid definition template number (m)
C>    (if = -1, don't bother matching gdt - accept any )
C>    @param[in] JGDT integer array of values defining the grid definition
C>    template 3.m of the field for which to search (=-9999 for wildcard)
C>    @param[out] K integer field number unpacked.
C>    @param[out] GFLD derived type gribfield (defined in module grib_mod)
C>    (NOTE: See Remarks Section)
C>    - gfld\%version GRIB edition number (currently 2)
C>    - gfld\%discipline Message Discipline (see Code Table 0.0)
C>    - gfld\%idsect Contains the entries in the Identification Section
C>    (Section 1) This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%idsect(1) Identification of originating Centre
C>    (see Common Code Table C-1) 7 US National Weather Service
C>    - gfld\%idsect(2) Identification of originating Sub-centre
C>    - gfld\%idsect(3) GRIB Master Tables Version Number
C>    (see Code Table 1.0) 0 Experimental; 1 Initial operational version number
C>    - gfld\%idsect(4) GRIB Local Tables Version Number (see Code Table 1.1)
C>     - 0 Local tables not used
C>     - 0 1-254 Number of local tables version used
C>    - gfld\%idsect(5) Significance of Reference Time (Code Table 1.2)
C>     - 0 Analysis
C>     - 1 Start of forecast
C>     - 2 Verifying time of forecast
C>     - 3 Observation time.
C>    - gfld\%idsect(6) Year (4 digits)
C>    - gfld\%idsect(7) Month
C>    - gfld\%idsect(8) Day
C>    - gfld\%idsect(9) Hour
C>    - gfld\%idsect(10) Minute
C>    - gfld\%idsect(11) Second
C>    - gfld\%idsect(12) Production status of processed data (see Code
C>    Table 1.3)
C>     - 0 Operational products
C>     - 1 Operational test products
C>     - 2 Research products
C>     - 3 Re-analysis products
C>    - gfld\%idsect(13) Type of processed data (see Code Table 1.4)
C>     - 0 Analysis products
C>     - 1 Forecast products
C>     - 2 Analysis and forecast products
C>     - 3 Control forecast products
C>     - 4 Perturbed forecast products
C>     - 5 Control and perturbed forecast products
C>     - 6 Processed satellite observations
C>     - 7 Processed radar observations
C>    - gfld\%idsectlen Number of elements in gfld\%idsect
C>    - gfld\%local Pointer to character array containing contents
C>    of Local Section 2, if included
C>    - gfld\%locallen length of array gfld\%local
C>    - gfld\%ifldnum field number within GRIB message
C>    - gfld\%griddef Source of grid definition (see Code Table 3.0)
C>     - 0 Specified in Code table 3.1
C>     - 1 Predetermined grid Defined by originating centre
C>    - gfld\%ngrdpts Number of grid points in the defined grid.
C>    Note that the number of actual data values returned from getgb2
C>    (in gfld\%ndpts) may be less than this value if a logical bitmap
C>    is in use with grid points that are being masked out.
C>    - gfld\%numoct_opt Number of octets needed for each additional grid
C>    points definition. Used to define number of points in each row (or
C>    column) for non-regular grids. = 0, if using regular grid.
C>    - gfld\%interp_opt Interpretation of list for optional points
C>    definition.(Code Table 3.11)
C>    - gfld\%igdtnum Grid Definition Template Number (Code Table 3.1)
C>    - gfld\%igdtmpl Contains the data values for the specified Grid
C>    Definition Template (NN=gfld\%igdtnum). Each element of this
C>    integer array contains an entry (in the order specified) of Grid
C>    Defintion Template 3.NN This element is actually a pointer to an
C>    array that holds the data.
C>    - gfld\%igdtlen Number of elements in gfld\%igdtmpl. i.e. number
C>    of entries in Grid Defintion Template 3.NN (NN=gfld\%igdtnum).
C>    - gfld\%list_opt (Used if gfld\%numoct_opt .ne. 0) This array
C>    contains the number of grid points contained in each row (or
C>    column). (part of Section 3) This element is actually a pointer
C>    to an array that holds the data. This pointer is nullified
C>    if gfld\%numoct_opt=0.
C>    - gfld\%num_opt (Used if gfld\%numoct_opt .ne. 0) The number of
C>    entries in array ideflist. i.e. number of rows (or columns) for which
C>    optional grid points are defined. This value is set to zero,
C>    if gfld\%numoct_opt=0.
C>    - gfdl\%ipdtnum Product Definition Template Number (Code Table 4.0)
C>    - gfld\%ipdtmpl Contains the data values for the specified Product
C>    Definition Template (N=gfdl\%ipdtnum). Each element of this integer
C>    array contains an entry (in the order specified) of Product Defintion
C>    Template 4.N. This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%ipdtlen Number of elements in gfld\%ipdtmpl. i.e. number of
C>    entries in Product Defintion Template 4.N (N=gfdl\%ipdtnum).
C>    - gfld\%coord_list Real array containing floating point values
C>    intended to document the vertical discretisation associated to
C>    model data on hybrid coordinate vertical levels.(part of Section 4)
C>    This element is actually a pointer to an array
C>    that holds the data.
C>    - gfld\%num_coord number of values in array gfld\%coord_list.
C>    - gfld\%ndpts Number of data points unpacked and returned.
C>    Note that this number may be different from the value of
C>    - gfld\%ngrdpts if a logical bitmap is in use with grid points
C>    that are being masked out.
C>    - gfld\%idrtnum Data Representation Template Number (Code Table 5.0)
C>    - gfld\%idrtmpl Contains the data values for the specified Data
C>    Representation Template (N=gfld\%idrtnum). Each element of this
C>    integer array contains an entry (in the order specified) of
C>    Product Defintion Template 5.N. This element is actually a
C>    pointer to an array that holds the data.
C>    - gfld\%idrtlen Number of elements in gfld\%idrtmpl. i.e. number
C>    of entries in Data Representation Template 5.N (N=gfld\%idrtnum).
C>    - gfld\%unpacked logical value indicating whether the bitmap and
C>    data values were unpacked. If false, gfld\%bmap and gfld\%fld
C>    pointers are nullified.
C>    - gfld\%expanded Logical value indicating whether the data field
C>    was expanded to the grid in the case where a bit-map is present.
C>    If true, the data points in gfld\%fld match the grid points and
C>    zeros were inserted at grid points where data was bit-mapped out.
C>    If false, the data values in gfld\%fld were not expanded to the
C>    grid and are just a consecutive array of data points corresponding
C>    to each value of "1" in gfld\%bmap.
C>    - gfld\%ibmap Bitmap indicator (see Code Table 6.0)
C>     - 0 bitmap applies and is included in Section 6.
C>     - 1-253 Predefined bitmap applies
C>     - 254 Previously defined bitmap applies to this field
C>     - 255 Bit map does not apply to this product.
C>    - gfld\%bmap Logical*1 array containing decoded bitmap, if ibmap=0
C>    or ibap=254. Otherwise nullified. This element is actually a
C>    pointer to an array that holds the data.
C>    - gfld\%fld Array of gfld\%ndpts unpacked data points. This element
C>    is actually a pointer to an array that holds the data.
C>    @param[out] LPOS starting position of the found index record
C>    within the complete index buffer, CBUF. = 0, if request not found.
C>    @param[out] IRET integer return code
C>    - 0 all ok
C>    - 97 error reading grib file
C>    - other gf_getfld grib2 unpacker return code
C>
C>    @note This subprogram is intended for private use by getgb2
C>    routines only. Note that derived type gribfield contains pointers
C>    to many arrays of data. The memory for these arrays is allocated
C>    when the values in the arrays are set, to help minimize problems
C>    with array overloading. Because of this users are encouraged to
C>    free up this memory, when it is no longer needed, by an explicit
C>    call to subroutine gf_free().
C>
C>    @author Stephen Gilbert @date 2002-01-15
C>

C-----------------------------------------------------------------------
      SUBROUTINE GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,
     &                   JGDT,K,GFLD,LPOS,IRET)

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
         subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                         mapgridlen,ideflist,idefnum,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: igdstmpl,ideflist
            integer,intent(out) :: igds(5)
            integer,intent(out) :: ierr,idefnum
         end subroutine gf_unpack3
         subroutine gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,
     &                      mappdslen,coordlist,numcoord,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            real,pointer,dimension(:) :: coordlist
            integer,pointer,dimension(:) :: ipdstmpl
            integer,intent(out) :: ipdsnum
            integer,intent(out) :: ierr,numcoord
         end subroutine gf_unpack4
         subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,
     &                         idrstmpl,mapdrslen,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,intent(out) :: ndpts,idrsnum
            integer,pointer,dimension(:) :: idrstmpl
            integer,intent(out) :: ierr
         end subroutine gf_unpack5
      end interface
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INITIALIZE
      K=0
      LPOS=0
      IRET=1
      IPOS=0
      nullify(gfld%idsect,gfld%local)
      nullify(gfld%list_opt,gfld%igdtmpl,gfld%ipdtmpl)
      nullify(gfld%coord_list,gfld%idrtmpl,gfld%bmap,gfld%fld)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR REQUEST
      DOWHILE(IRET.NE.0.AND.K.LT.NNUM)
        K=K+1
        CALL G2_GBYTEC(CBUF,INLEN,IPOS*8,4*8)    ! GET LENGTH OF CURRENT
                                              ! INDEX RECORD
        IF ( K.LE.J ) THEN           ! SKIP THIS INDEX
           IPOS=IPOS+INLEN
           CYCLE
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF GRIB2 DISCIPLINE IS A MATCH
        CALL G2_GBYTEC(CBUF,GFLD%DISCIPLINE,(IPOS+41)*8,1*8)
        IF ( (JDISC.NE.-1).AND.(JDISC.NE.GFLD%DISCIPLINE) ) THEN
           IPOS=IPOS+INLEN
           CYCLE
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF IDENTIFICATION SECTION IS A MATCH
        MATCH1=.FALSE.
        CALL G2_GBYTEC(CBUF,LSEC1,(IPOS+44)*8,4*8)  ! GET LENGTH OF IDS 
        IOF=0
        CALL GF_UNPACK1(CBUF(IPOS+45),LSEC1,IOF,GFLD%IDSECT,
     &                  GFLD%IDSECTLEN,ICND)
        IF ( ICND.EQ.0 ) THEN
           MATCH1=.TRUE.
           DO I=1,GFLD%IDSECTLEN
              IF ( (JIDS(I).NE.-9999).AND.
     &             (JIDS(I).NE.GFLD%IDSECT(I)) ) THEN
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF GRID DEFINITION TEMPLATE IS A MATCH
        JPOS=IPOS+44+LSEC1
        MATCH3=.FALSE.
        CALL G2_GBYTEC(CBUF,LSEC3,JPOS*8,4*8)  ! GET LENGTH OF GDS 
        IF ( JGDTN.EQ.-1 ) THEN
           MATCH3=.TRUE.
        ELSE
           CALL G2_GBYTEC(CBUF,NUMGDT,(JPOS+12)*8,2*8)  ! GET GDT TEMPLATE NO.
           IF ( JGDTN.EQ.NUMGDT ) THEN
              IOF=0
              CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL,
     &                     GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
              IF ( ICND.EQ.0 ) THEN
                 MATCH3=.TRUE.
                 DO I=1,GFLD%IGDTLEN
                    IF ( (JGDT(I).NE.-9999).AND.
     &                   (JGDT(I).NE.GFLD%IGDTMPL(I)) ) THEN
                       MATCH3=.FALSE.
                       EXIT
                    ENDIF
                 ENDDO
C                 WHERE ( JGDT(1:GFLD%IGDTLEN).NE.-9999 ) 
C     &              MATCH3=ALL(JGDT(1:GFLD%IGDTLEN).EQ.GFLD%IGDTMPL(1:GFLD%IGDTLEN))
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF PRODUCT DEFINITION TEMPLATE IS A MATCH
        JPOS=JPOS+LSEC3
        MATCH4=.FALSE.
        CALL G2_GBYTEC(CBUF,LSEC4,JPOS*8,4*8)  ! GET LENGTH OF PDS 
        IF ( JPDTN.EQ.-1 ) THEN
           MATCH4=.TRUE.
        ELSE
           CALL G2_GBYTEC(CBUF,NUMPDT,(JPOS+7)*8,2*8)  ! GET PDT TEMPLATE NO.
           IF ( JPDTN.EQ.NUMPDT ) THEN
              IOF=0
              CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM,
     &                        GFLD%IPDTMPL,GFLD%IPDTLEN,
     &                        GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
              IF ( ICND.EQ.0 ) THEN
                 MATCH4=.TRUE.
                 DO I=1,GFLD%IPDTLEN
                    IF ( (JPDT(I).NE.-9999).AND.
     &                   (JPDT(I).NE.GFLD%IPDTMPL(I)) ) THEN
                       MATCH4=.FALSE.
                       EXIT
                    ENDIF
                 ENDDO
c                 WHERE ( JPDT.NE.-9999) 
c     &              MATCH4=ALL( JPDT(1:GFLD%IPDTLEN) .EQ. GFLD%IPDTMPL(1:GFLD%IPDTLEN) )
              ENDIF
           ENDIF
        ENDIF
        IF ( .NOT. MATCH4 ) THEN
           IF (ASSOCIATED(GFLD%IPDTMPL)) DEALLOCATE(GFLD%IPDTMPL)
           IF (ASSOCIATED(GFLD%COORD_LIST)) DEALLOCATE(GFLD%COORD_LIST)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IF REQUEST IS FOUND
C  SET VALUES FOR DERIVED TYPE GFLD AND RETURN
        IF(MATCH1.AND.MATCH3.AND.MATCH4) THEN
           LPOS=IPOS+1
           CALL G2_GBYTEC(CBUF,GFLD%VERSION,(IPOS+40)*8,1*8)
           CALL G2_GBYTEC(CBUF,GFLD%IFLDNUM,(IPOS+42)*8,2*8)
           GFLD%UNPACKED=.FALSE.
           JPOS=IPOS+44+LSEC1
           IF ( JGDTN.EQ.-1 ) THEN     ! UNPACK GDS, IF NOT DONE BEFORE
              IOF=0
              CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL,
     &                     GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
              GFLD%GRIDDEF=KGDS(1)
              GFLD%NGRDPTS=KGDS(2)
              GFLD%NUMOCT_OPT=KGDS(3)
              GFLD%INTERP_OPT=KGDS(4)
              GFLD%IGDTNUM=KGDS(5)
           ENDIF
           JPOS=JPOS+LSEC3
           IF ( JPDTN.EQ.-1 ) THEN     ! UNPACK PDS, IF NOT DONE BEFORE
              IOF=0
              CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM,
     &                        GFLD%IPDTMPL,GFLD%IPDTLEN,
     &                        GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
           ENDIF
           JPOS=JPOS+LSEC4
           CALL G2_GBYTEC(CBUF,LSEC5,JPOS*8,4*8)  ! GET LENGTH OF DRS 
           IOF=0
           CALL GF_UNPACK5(CBUF(JPOS+1),LSEC5,IOF,GFLD%NDPTS,
     &                     GFLD%IDRTNUM,GFLD%IDRTMPL,
     &                     GFLD%IDRTLEN,ICND)
           JPOS=JPOS+LSEC5
           CALL G2_GBYTEC(CBUF,GFLD%IBMAP,(JPOS+5)*8,1*8)  ! GET IBMAP
           IRET=0
        ELSE      ! PDT DID NOT MATCH
           IPOS=IPOS+INLEN
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
