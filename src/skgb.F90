!> @file
!> @brief This subroutine generates an index record for each field in
!> a grib2 message.
!> @author Mark Iredell @date 1995-10-31

!> This subroutine searches a file for the next grib 1 message. A grib
!> 1 message is identified by its indicator section, i.e. an 8-byte
!> sequence with 'grib' in bytes 1-4 and 1 in byte 8. If found, the
!> length of the message is decoded from bytes 5-7. The search is
!> done over a given section of the file. The search is terminated if
!> an eof or i/o error is encountered.
!> PROGRAM HISTORY LOG:
!> - 1993-11-22 Mark Iredell
!> - 1995-10-31 Mark Iredell add call to baread.
!> - 1997-03-14 Mark Iredell check for '7777'.
!> - 2001-12-05 Stephen Gilbert modified to also look for grib2
!> messages.
!> - 2009-12-14 Boi Vuong modified to increase length of seek (512).
!>
!> @param[in] LUGB integer unit of the unblocked grib file.
!> @param[in] ISEEK integer number of bytes to skip before search.
!> @param[in] MSEEK integer maximum number of bytes to search.
!> @param[out] LSKIP integer number of bytes to skip before message.
!> @param[out] LGRIB integer number of bytes in message (0 if not
!> found)
!>
!> @author Mark Iredell @date 1995-10-31
SUBROUTINE SKGB(LUGB,ISEEK,MSEEK,LSKIP,LGRIB)
  implicit none
  
  integer lseek, lugb, iseek, mseek, lskip, lgrib, i1, i4
  integer k, k4, kg, km, kn, ks, kz
  PARAMETER(LSEEK=512)
  CHARACTER Z(LSEEK)
  CHARACTER Z4(4)

  LGRIB=0
  KS=ISEEK
  KN=MIN(LSEEK,MSEEK)
  KZ=LSEEK

  !  LOOP UNTIL GRIB MESSAGE IS FOUND
  DO WHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
     !  READ PARTIAL SECTION
     CALL BAREAD(LUGB,KS,KN,KZ,Z)
     KM=KZ-8+1
     K=0
     !  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
     DO WHILE(LGRIB.EQ.0.AND.K.LT.KM)
        CALL G2_GBYTEC(Z,I4,(K+0)*8,4*8)
        CALL G2_GBYTEC(Z,I1,(K+7)*8,1*8)
        IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
           !  LOOK FOR '7777' AT END OF GRIB MESSAGE
           IF (I1.EQ.1) CALL G2_GBYTEC(Z,KG,(K+4)*8,3*8)
           IF (I1.EQ.2) CALL G2_GBYTEC(Z,KG,(K+12)*8,4*8)
           CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
           IF(K4.EQ.4) THEN
              CALL G2_GBYTEC(Z4,I4,0,4*8)
              IF(I4.EQ.926365495) THEN
                 !  GRIB MESSAGE FOUND
                 LSKIP=KS+K
                 LGRIB=KG
              ENDIF
           ENDIF
        ENDIF
        K=K+1
     ENDDO
     KS=KS+KM
     KN=MIN(LSEEK,ISEEK+MSEEK-KS)
  ENDDO

  RETURN
END SUBROUTINE SKGB
