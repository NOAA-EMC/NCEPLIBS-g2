!> @file
!> @brief This subroutine finds and extracts the index for the
!> requested field from a GRIB2 file.
!> @author Stephen Gilbert @date 2003-12-31

!> This subroutine finds and extracts a grib message from a GRIB2 file
!> given the index of the requested field.
!>
!> The GRIB message returned can contain only the requested field
!> (extract=.true.), or the complete GRIB message originally
!> containing the desired field can be returned (extract=.false.) even
!> if other fields were included in the GRIB message.
!>
!> If the GRIB field is not found, then the return code will be
!> nonzero.
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] cindex Index record of the grib field (see docunentation of
!> subroutine ixgb2() for description of an index record.)
!> @param[in] extract Logical value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] gribm Returned grib message.
!> @param[out] leng Length of returned grib message in bytes.
!> @param[out] iret Return code:
!> - 0 No error.
!> - 97 Error reading grib file.
!>
!> @author Stephen Gilbert @date 2003-12-31
      SUBROUTINE GETGB2RP(LUGB,CINDEX,EXTRACT,GRIBM,LENG,IRET)

      INTEGER,INTENT(IN) :: LUGB
      CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
      LOGICAL,INTENT(IN) :: EXTRACT
      INTEGER,INTENT(OUT) :: LENG,IRET
      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
 
      INTEGER,PARAMETER :: ZERO=0
      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CSEC2,CSEC6,CSEC7
      CHARACTER(LEN=4) :: Ctemp

      IRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXTRACT GRIB MESSAGE FROM FILE
      IF ( EXTRACT ) THEN
         LEN0=16
         LEN8=4
         CALL G2_GBYTEC(CINDEX,ISKIP,4*8,4*8)    ! BYTES TO SKIP IN FILE
         CALL G2_GBYTEC(CINDEX,ISKP2,8*8,4*8)    ! BYTES TO SKIP FOR section 2
         if ( iskp2 .gt. 0 ) then
            CALL BAREAD(LUGB,ISKIP+ISKP2,4,LREAD,ctemp)
            CALL G2_GBYTEC(Ctemp,LEN2,0,4*8)      ! LENGTH OF SECTION 2
            ALLOCATE(csec2(len2))
            CALL BAREAD(LUGB,ISKIP+ISKP2,LEN2,LREAD,csec2)
         else
            LEN2=0
         endif
         CALL G2_GBYTEC(CINDEX,LEN1,44*8,4*8)      ! LENGTH OF SECTION 1
         IPOS=44+LEN1
         CALL G2_GBYTEC(CINDEX,LEN3,IPOS*8,4*8)      ! LENGTH OF SECTION 3
         IPOS=IPOS+LEN3
         CALL G2_GBYTEC(CINDEX,LEN4,IPOS*8,4*8)      ! LENGTH OF SECTION 4
         IPOS=IPOS+LEN4
         CALL G2_GBYTEC(CINDEX,LEN5,IPOS*8,4*8)      ! LENGTH OF SECTION 5
         IPOS=IPOS+LEN5
         CALL G2_GBYTEC(CINDEX,LEN6,IPOS*8,4*8)      ! LENGTH OF SECTION 6
         IPOS=IPOS+5
         CALL G2_GBYTEC(CINDEX,IBMAP,IPOS*8,1*8)      ! Bitmap indicator
         IF ( IBMAP .eq. 254 ) THEN
            CALL G2_GBYTEC(CINDEX,ISKP6,24*8,4*8)    ! BYTES TO SKIP FOR section 6
            CALL BAREAD(LUGB,ISKIP+ISKP6,4,LREAD,ctemp)
            CALL G2_GBYTEC(Ctemp,LEN6,0,4*8)      ! LENGTH OF SECTION 6
         ENDIF
         !
         !  READ IN SECTION 7 from file
         !
         CALL G2_GBYTEC(CINDEX,ISKP7,28*8,4*8)    ! BYTES TO SKIP FOR section 7
         CALL BAREAD(LUGB,ISKIP+ISKP7,4,LREAD,ctemp)
         CALL G2_GBYTEC(Ctemp,LEN7,0,4*8)      ! LENGTH OF SECTION 7
         ALLOCATE(csec7(len7))
         CALL BAREAD(LUGB,ISKIP+ISKP7,LEN7,LREAD,csec7)

         LENG=LEN0+LEN1+LEN2+LEN3+LEN4+LEN5+LEN6+LEN7+LEN8
         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))

         ! Create Section 0
         !
         GRIBM(1)='G'
         GRIBM(2)='R'
         GRIBM(3)='I'
         GRIBM(4)='B'
         GRIBM(5)=CHAR(0)
         GRIBM(6)=CHAR(0)
         GRIBM(7)=CINDEX(42)
         GRIBM(8)=CINDEX(41)
         GRIBM(9)=CHAR(0)
         GRIBM(10)=CHAR(0)
         GRIBM(11)=CHAR(0)
         GRIBM(12)=CHAR(0)
         CALL G2_SBYTEC(GRIBM,LENG,12*8,4*8)
         !
         ! Copy Section 1
         !
         GRIBM(17:16+LEN1)=CINDEX(45:44+LEN1)
         lencur=16+LEN1
         ipos=44+len1
         !
         ! Copy Section 2, if necessary
         !
         if ( iskp2 .gt. 0 ) then
           GRIBM(lencur+1:lencur+LEN2)=csec2(1:LEN2)
           lencur=lencur+LEN2
         endif
         !
         ! Copy Sections 3 through 5
         !
         GRIBM(lencur+1:lencur+LEN3+LEN4+LEN5)=
     &                      CINDEX(ipos+1:ipos+LEN3+LEN4+LEN5)
         lencur=lencur+LEN3+LEN4+LEN5
         ipos=ipos+LEN3+LEN4+LEN5
         !
         ! Copy Section 6
         !
         if ( LEN6 .eq. 6 .AND. IBMAP .ne. 254 ) then
            GRIBM(lencur+1:lencur+LEN6)=CINDEX(ipos+1:ipos+LEN6)
            lencur=lencur+LEN6
         else
            CALL G2_GBYTEC(CINDEX,ISKP6,24*8,4*8)    ! BYTES TO SKIP FOR section 6
            CALL BAREAD(LUGB,ISKIP+ISKP6,4,LREAD,ctemp)
            CALL G2_GBYTEC(Ctemp,LEN6,0,4*8)      ! LENGTH OF SECTION 6
            ALLOCATE(csec6(len6))
            CALL BAREAD(LUGB,ISKIP+ISKP6,LEN6,LREAD,csec6)
            GRIBM(lencur+1:lencur+LEN6)=csec6(1:LEN6)
            lencur=lencur+LEN6
            IF ( allocated(csec6)) DEALLOCATE(csec6)
         endif
         !
         ! Copy Section 7
         !
         GRIBM(lencur+1:lencur+LEN7)=csec7(1:LEN7)
         lencur=lencur+LEN7
         !
         ! Section 8
         !
         GRIBM(lencur+1)='7'
         GRIBM(lencur+2)='7'
         GRIBM(lencur+3)='7'
         GRIBM(lencur+4)='7'

         !  clean up
         !
         IF ( allocated(csec2)) DEALLOCATE(csec2)
         IF ( allocated(csec7)) deallocate(csec7)

      ELSE    ! DO NOT extract field from message :  Get entire message

         CALL G2_GBYTEC(CINDEX,ISKIP,4*8,4*8)    ! BYTES TO SKIP IN FILE
         CALL G2_GBYTEC(CINDEX,LENG,36*8,4*8)      ! LENGTH OF GRIB MESSAGE
         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))
         CALL BAREAD(LUGB,ISKIP,LENG,LREAD,GRIBM)
         IF ( LENG .NE. LREAD ) THEN
            DEALLOCATE(GRIBM)
            NULLIFY(GRIBM)
            IRET=97
            RETURN
         ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
