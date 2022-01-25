!> @file
!> @brief This subroutine reads and unpacks a local use section from a
!> GRIB2 message.
!> @author Stephen Gilbert @date 2002-05-07

!> This subroutine reads and unpacks a local use section from a GRIB2
!> message.
!>
!> This subroutine decodes information for the selected grib field and
!> returns it in a derived type variable, gfld. gfld is of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> This subprogram is intended for private use by getgb2 routines
!> only.
!>
!> Note that derived type gribfield contains pointers to many arrays
!> of data. The memory for these arrays is allocated when the values
!> in the arrays are set, to help minimize problems with array
!> overloading. Because of this users should free this memory, when it
!> is no longer needed, by an explicit call to subroutine gf_free().
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> @param[in] CINDEX index record of the grib field (see docblock of
!> subroutine ixgb2 for description of an index record.)
!> @param[out] GFLD derived type gribfield @ref grib_mod::gribfield.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @note Do not engage the same logical unit from more than one
!> processor.
!>
!> @author Stephen Gilbert @date 2002-05-07
SUBROUTINE GETGB2L(LUGB,CINDEX,GFLD,IRET)

  USE GRIB_MOD

  INTEGER,INTENT(IN) :: LUGB
  CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
  INTEGER,INTENT(OUT) :: IRET
  TYPE(GRIBFIELD) :: GFLD

  INTEGER :: LSKIP,SKIP2
  CHARACTER(LEN=1):: CSIZE(4)
  CHARACTER(LEN=1),ALLOCATABLE :: CTEMP(:)

  interface
     subroutine gf_unpack2(cgrib,lcgrib,iofst,lencsec2,csec2,ierr)
       character(len=1),intent(in) :: cgrib(lcgrib)
       integer,intent(in) :: lcgrib
       integer,intent(inout) :: iofst
       integer,intent(out) :: lencsec2
       integer,intent(out) :: ierr
       character(len=1),pointer,dimension(:) :: csec2
     end subroutine gf_unpack2
  end interface

  !  GET INFO
  NULLIFY(gfld%local)
  IRET=0
  CALL G2_GBYTEC(CINDEX,LSKIP,4*8,4*8)
  CALL G2_GBYTEC(CINDEX,SKIP2,8*8,4*8)


  !  READ AND UNPACK LOCAL USE SECTION, IF PRESENT
  IF ( SKIP2.NE.0 ) THEN
     ISKIP=LSKIP+SKIP2
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
     CALL GF_UNPACK2(CTEMP,ILEN,IOFST,gfld%locallen, &
          gfld%local,ierr)
     IF (IERR.NE.0) THEN
        IRET=98
        DEALLOCATE(CTEMP)
        RETURN
     ENDIF
     DEALLOCATE(CTEMP)
  ELSE
     gfld%locallen=0
  ENDIF

  RETURN
END SUBROUTINE GETGB2L
