!> @file
!> @brief This subroutine read and unpack sections 6 and 7 from a
!> grib2 message.
!> @author Stephen Gilbert @date 2002-01-11

!> This subroutine reads and unpacks sections 6 and 7 from a grib2
!> message.
!>
!> It assumes that the metadata for this field already exists in
!> derived type @ref grib_mod::gribfield. Specifically, it requires
!> gfld\%ibmap, gfld\%ngrdpts, gfld\%idrtnum, gfld\%idrtmpl, and
!> gfld\%ndpts.
!>
!> It decodes information for the selected grib field and returns it
!> in a derived type variable, gfld, of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] CINDEX index record of the grib field (see 
!> subroutine ixgb2() for description of an index record.)
!> @param[out] GFLD derived type @ref grib_mod::gribfield.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @note Do not engage the same logical unit from more than one
!> processor. This subprogram is intended for private use by getgb2
!> routines only. Note that derived type gribfield contains pointers
!> to many arrays of data. The memory for these arrays is allocated
!> when the values in the arrays are set, to help minimize problems
!> with array overloading. Users should free this memory, when it is
!> no longer needed, by a call to subroutine gf_free().
!>
!>    @author Stephen Gilbert @date 2002-01-11
SUBROUTINE GETGB2R(LUGB,CINDEX,GFLD,IRET)

  USE GRIB_MOD

  INTEGER,INTENT(IN) :: LUGB
  CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
  INTEGER,INTENT(OUT) :: IRET
  TYPE(GRIBFIELD) :: GFLD

  INTEGER :: LSKIP,SKIP6,SKIP7
  CHARACTER(LEN=1):: CSIZE(4)
  CHARACTER(LEN=1),ALLOCATABLE :: CTEMP(:)
  real,pointer,dimension(:) :: newfld

  interface
     subroutine gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap, &
          bmap,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib,ngpts
       integer,intent(inout) :: iofst
       integer,intent(out) :: ibmap
       integer,intent(out) :: ierr
       logical*1,pointer,dimension(:) :: bmap
     end subroutine gf_unpack6
     subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl, &
          idrsnum,idrstmpl,ndpts,fld,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib,ndpts,idrsnum,igdsnum
       integer,intent(inout) :: iofst
       integer,pointer,dimension(:) :: idrstmpl,igdstmpl
       integer,intent(out) :: ierr
       real,pointer,dimension(:) :: fld
     end subroutine gf_unpack7
  end interface

  !  GET INFO
  NULLIFY(gfld%bmap,gfld%fld)
  IRET=0
  CALL G2_GBYTEC(CINDEX,LSKIP,4*8,4*8)
  CALL G2_GBYTEC(CINDEX,SKIP6,24*8,4*8)
  CALL G2_GBYTEC(CINDEX,SKIP7,28*8,4*8)


  !  READ AND UNPACK BIT_MAP, IF PRESENT
  IF ( gfld%ibmap.eq.0.OR.gfld%ibmap.eq.254 ) THEN
     ISKIP=LSKIP+SKIP6
     CALL BAREAD(LUGB,ISKIP,4,LREAD,CSIZE)    ! GET LENGTH OF SECTION
     CALL G2_GBYTEC(CSIZE,ILEN,0,32)
     ALLOCATE(CTEMP(ILEN))
     CALL BAREAD(LUGB,ISKIP,ILEN,LREAD,CTEMP)  ! READ IN SECTION
     IF (ILEN.NE.LREAD) THEN
        IRET=97
        DEALLOCATE(CTEMP)
        RETURN
     ENDIF
     IOFST=0
     CALL GF_UNPACK6(CTEMP,ILEN,IOFST,gfld%ngrdpts,idum, &
          gfld%bmap,ierr)
     IF (IERR.NE.0) THEN
        IRET=98
        DEALLOCATE(CTEMP)
        RETURN
     ENDIF
     DEALLOCATE(CTEMP)
  ENDIF

  !  READ AND UNPACK DATA FIELD
  ISKIP=LSKIP+SKIP7
  CALL BAREAD(LUGB,ISKIP,4,LREAD,CSIZE)    ! GET LENGTH OF SECTION
  CALL G2_GBYTEC(CSIZE,ILEN,0,32)
  if (ilen.lt.6) ilen=6
  ALLOCATE(CTEMP(ILEN))
  CALL BAREAD(LUGB,ISKIP,ILEN,LREAD,CTEMP)  ! READ IN SECTION
  IF (ILEN.NE.LREAD) THEN
     IRET=97
     DEALLOCATE(CTEMP)
     RETURN
  ENDIF
  IOFST=0
  CALL GF_UNPACK7(CTEMP,ILEN,IOFST,gfld%igdtnum,gfld%igdtmpl, &
       gfld%idrtnum,gfld%idrtmpl,gfld%ndpts, &
       gfld%fld,ierr)
  IF (IERR.NE.0) THEN
     IRET=98
     DEALLOCATE(CTEMP)
     RETURN
  ENDIF
  DEALLOCATE(CTEMP)

  !  If bitmap is used with this field, expand data field
  !  to grid, if possible.
  if ( gfld%ibmap .ne. 255 .AND. associated(gfld%bmap) ) then
     allocate(newfld(gfld%ngrdpts))
     !newfld=0.0
     !newfld=unpack(lgfld%fld,lgfld%bmap,newfld)
     n=1
     do j=1,gfld%ngrdpts
        if ( gfld%bmap(j) ) then
           newfld(j)=gfld%fld(n)
           n=n+1
        else
           newfld(j)=0.0
        endif
     enddo
     deallocate(gfld%fld);
     gfld%fld=>newfld;
     gfld%expanded=.true.
  else
     gfld%expanded=.true.
  endif

  RETURN
END SUBROUTINE GETGB2R
