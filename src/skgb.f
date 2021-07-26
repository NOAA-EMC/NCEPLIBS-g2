C>    @file
C>    @brief This subroutine generates an index record for each field in
C>    a grib2 message.
C>    @author Mark Iredell @date 1995-10-31
C>

C>    This subroutine searches a file for the next grib 1 message. A grib 
C>    1 message is identified by its indicator section, i.e. an 8-byte
C>    sequence with 'grib' in bytes 1-4 and 1 in byte 8. If found, the
C>    length of the message is decoded from bytes 5-7. The search is
C>    done over a given section of the file. The search is terminated if
C>    an eof or i/o error is encountered.
C>    PROGRAM HISTORY LOG:
C>    - 1993-11-22 Mark Iredell
C>    - 1995-10-31 Mark Iredell add call to baread.
C>    - 1997-03-14 Mark Iredell check for '7777'.
C>    - 2001-12-05 Stephen Gilbert modified to also look for grib2
C>    messages.
C>    - 2009-12-14 Boi Vuong modified to increase length of seek (512).
C>    
C>    @param[in] LUGB integer unit of the unblocked grib file.
C>    @param[in] ISEEK integer number of bytes to skip before search.
C>    @param[in] MSEEK integer maximum number of bytes to search.
C>    @param[out] LSKIP integer number of bytes to skip before message.
C>    @param[out] LGRIB integer number of bytes in message (0 if not
C>    found)
C>    
C>    @author Mark Iredell @date 1995-10-31
C>    

C-----------------------------------------------------------------------
      SUBROUTINE SKGB(LUGB,ISEEK,MSEEK,LSKIP,LGRIB)

      PARAMETER(LSEEK=512)
      CHARACTER Z(LSEEK)
      CHARACTER Z4(4)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LGRIB=0
      KS=ISEEK
      KN=MIN(LSEEK,MSEEK)
      KZ=LSEEK
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOOP UNTIL GRIB MESSAGE IS FOUND
      DOWHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
C  READ PARTIAL SECTION
        CALL BAREAD(LUGB,KS,KN,KZ,Z)
        KM=KZ-8+1
        K=0
C  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
        DOWHILE(LGRIB.EQ.0.AND.K.LT.KM)
          CALL G2_GBYTEC(Z,I4,(K+0)*8,4*8)
          CALL G2_GBYTEC(Z,I1,(K+7)*8,1*8)
          IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
C  LOOK FOR '7777' AT END OF GRIB MESSAGE
            IF (I1.EQ.1) CALL G2_GBYTEC(Z,KG,(K+4)*8,3*8)
            IF (I1.EQ.2) CALL G2_GBYTEC(Z,KG,(K+12)*8,4*8)
            CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
            IF(K4.EQ.4) THEN
              CALL G2_GBYTEC(Z4,I4,0,4*8)
              IF(I4.EQ.926365495) THEN
C  GRIB MESSAGE FOUND
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
