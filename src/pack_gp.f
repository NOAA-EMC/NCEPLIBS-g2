!> @file
!> @brief This subroutine determines groups of variable size.
!> @author Harry Glahn @date 1994-02-01
!>

!> This subroutine determines groups of variable size, but at least of
!> size minpk, the associated max(JMAX) and min(JMIN), the number of
!> bits necessary to hold the values in each group LBIT, the number of
!> values in each group NOV, the number of bits necessary to pack the
!> JMIN values IBIT, the number of bits necessary to pack the LBIT
!> values JBIT, and the number of bits necessary to pack the NOV
!> values KBIT.
!>
!> The routine is designed to determine the groups such that a small
!> number of bits is necessary to pack the data without excessive
!> computations. If all values in the group are zero, the number of
!> bits to use in packing is defined as zero when there can be no
!> missing values; when there can be missing values, the number of
!> bits must be at least 1 to have the capability to recognize the
!> missing value. However, if all values in a group are missing, the
!> number of bits needed is 0, and the unpacker recognizes this.
!>
!> All variables are integer, even though the groups are initially of
!> size minpk or larger, an adjustment between two groups (the
!> lookback procedure) may make a group smaller than minpk. The
!> control on group size is that the sum of the sizes of the two
!> consecutive groups, each of size minpk or larger, is not decreased.
!>
!> When determining the number of bits necessary for packing, the
!> largest value that can be accommodated in, say mbits is 2**mbits-1
!> this largest value (and the next smallest value) is reserved for
!> the missing value indicator (only) when is 523 ne 0.
!>
!> If the dimension NDG is not large enough to hold all the groups,
!> the local value of minpk is increased by 50 percent. this is
!> repeated until ndg will suffice. A diagnostic is printed whenever
!> this happens, which should be very rarely. If it happens often, NDG
!> in subroutine pack should be increased and a corresponding increase
!> in subroutine unpack made.
!>
!> Considerable code is provided so that no more checking for missing
!> values within loops is done than necessary; the added efficiency of
!> this is relatively minor, but does no harm.
!>
!> For grib2, the reference value for the length of groups in nov and
!> for the number of bits necessary to pack group values are
!> determined, and subtracted before jbit and kbit are
!> determined. When 1 or more groups are large compared to the others,
!> the width of all groups must be as large as the largest.  A
!> subroutine reduce breaks up large groups into 2 or more to reduce
!> total bits required. If reduce() should abort, pack_gp() will be
!> executed again without the call to reduce.
!>
!> @param[in] IC array to hold data for packing. The values do not
!> have to be positive at this point, but must be in the range
!> -2**30 to +2**30 (the value of mallow). These integer values
!> will be retained exactly through packing and unpacking.
!> @param[in] NXY number of values in IC. also treated as
!> its dimension.
!> @param[in] IS523 missing value management 0=data contains no
!> missing values: 1 data contains primary missing values; 2=data
!> contains primary and secondary missing values.
!> @param[in] MINPK the minimum size of each group, except possibly
!> the last one.
!> @param[in] INC the number of values to add to an already existing
!> group in determining whether or not to start a new group. Ideally,
!> this would be 1, but each time inc values are attempted, the max
!> and min of the next minpk values must be found. This is "a loop
!> within a loop," and a slightly larger value may give about as good
!> results with slightly less computational time. If inc is le 0, 1
!> is used, and a diagnostic is output. note: it is expected that
!> INC will equal 1. The code uses inc primarily in the loops
!> starting at statement 180. If INC were 1, there would not need
!> to be loops as such. However, kinc (the local value of INC) is
!> set ge 1 when near the end of the data to forestall a very small
!> group at the end.
!> @param[in] MISSP when missing points can be present in the data,
!> they will have the value missp or misss. missp is the primary
!> missing value and misss is the secondary missing value. These
!> must not be values that would occur with subtracting the minimum
!> (reference) value or scaling. for example, missp = 0 would not
!> be advisable.
!> @param[in] MISSS secondary missing value indicator (see missp).
!> @param[out] JMIN the minimum of each group (j=1,lx).
!> @param[out] JMAX the maximum of each group (j=1,lx). This is not
!> really needed, but since the max of each group must be found,
!> saving it here is cheap in case the user wants it.
!> @param[out] LBIT the number of bits necessary to pack each group
!> (j=1,lx). It is assumed the minimum of each group will be removed
!> before packing, and the values to pack will, therefore, all be
!> positive. However, IC does not necessarily contain all positive
!> values. If the overall minimum has been removed (the usual case),
!> then IC will contain only positive values.
!> @param[out] NOV the number of values in each group (j=1,lx).
!> @param[in] NDG the dimension of JMIN, JMAX, LBIT, and NOV.
!> @param[out] LX the number of groups determined.
!> @param[out] IBIT the number of bits necessary to pack the JMIN(j)
!> values, j=1,LX.
!> @param[out] JBIT the number of bits necessary to pack the LBIT(j)
!> values, j=1,LX.
!> @param[out] KBIT the number of bits necessary to pack the NOV(j)
!> values, j=1,LX.
!> @param[out] NOVREF reference value for NOV.
!> @param[out] LBITREF reference value for LBIT.
!> @param[out] IER error return.
!> - 706 value will not pack in 30 bits--fatal
!> - 714 error in reduce--non-fatal
!> - 715 ngp not large enough in reduce--non-fatal
!> - 716 minpk inceased--non-fatal
!> - 717 inc set = 1--non-fatal
!>
!> @author Harry Glahn @date 1994-02-01
      SUBROUTINE PACK_GP(IC,NXY,IS523,MINPK,INC,MISSP,MISSS,
     1                   JMIN,JMAX,LBIT,NOV,NDG,LX,IBIT,JBIT,KBIT,
     2                   NOVREF,LBITREF,IER)            

      PARAMETER (MALLOW=2**30+1)
C
      CHARACTER*1 CFEED
      LOGICAL ADDA
C
      DIMENSION IC(NXY)
      DIMENSION JMIN(NDG),JMAX(NDG),LBIT(NDG),NOV(NDG)
      DIMENSION MISSLX(NDG)
C        MISSLX( ) IS AN AUTOMATIC ARRAY.
      INTEGER, PARAMETER :: IBXX2(0:30) = (/ 1, 2, 4, 8, 16, 32, 64,    &
     &     128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536,  &
     &     131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,  &
     &     16777216, 33554432, 67108864, 134217728, 268435456,          &
     &     536870912, 1073741824 /)
C
      
      PARAMETER (IFEED=12)
C
      IER=0
      IERSAV=0
C     CALL TIMPR(KFILDO,KFILDO,'START PACK_GP        ')
      CFEED=CHAR(IFEED)
C
      IRED=0
C        IRED IS A FLAG.  WHEN ZERO, REDUCE WILL BE CALLED.
C        IF REDUCE ABORTS, IRED = 1 AND IS NOT CALLED.  IN
C        THIS CASE PACK_GP EXECUTES AGAIN EXCEPT FOR REDUCE.
C
      IF(INC.LE.0)THEN
         IERSAV=717
C        WRITE(KFILDO,101)INC
C101     FORMAT(/' ****INC ='I8,' NOT CORRECT IN PACK_GP.  1 IS USED.')
      ENDIF
C
C        THERE WILL BE A RESTART OF PACK_GP IF SUBROUTINE REDUCE
C        ABORTS.  THIS SHOULD NOT HAPPEN, BUT IF IT DOES, PACK_GP
C        WILL COMPLETE WITHOUT SUBROUTINE REDUCE.  A NON FATAL
C        DIAGNOSTIC RETURN IS PROVIDED.
C
 102  KINC=MAX(INC,1)
      LMINPK=MINPK
C
C        THERE WILL BE A RESTART AT 105 IS NDG IS NOT LARGE ENOUGH.
C        A NON FATAL DIAGNOSTIC RETURN IS PROVIDED.
C
 105  KSTART=1
      KTOTAL=0
      LX=0
      ADDA=.FALSE.
      LMISS=0
      IF(IS523.EQ.1)LMISS=1
      IF(IS523.EQ.2)LMISS=2
C
C        *************************************
C
C        THIS SECTION COMPUTES STATISTICS FOR GROUP A.  GROUP A IS
C        A GROUP OF SIZE LMINPK.
C
C        *************************************
C
      IBITA=0
      MINA=MALLOW
      MAXA=-MALLOW
      MINAK=MALLOW
      MAXAK=-MALLOW
C
C        FIND THE MIN AND MAX OF GROUP A.  THIS WILL INITIALLY BE OF
C        SIZE LMINPK (IF THERE ARE STILL LMINPK VALUES IN IC( )), BUT
C        WILL INCREASE IN SIZE IN INCREMENTS OF INC UNTIL A NEW
C        GROUP IS STARTED.  THE DEFINITION OF GROUP A IS DONE HERE
C        ONLY ONCE (UPON INITIAL ENTRY), BECAUSE A GROUP B CAN ALWAYS
C        BECOME A NEW GROUP A AFTER A IS PACKED, EXCEPT IF LMINPK 
C        HAS TO BE INCREASED BECAUSE NDG IS TOO SMALL.  THEREFORE,
C        THE SEPARATE LOOPS FOR MISSING AND NON-MISSING HERE BUYS
C        ALMOST NOTHING.
C
      NENDA=MIN(KSTART+LMINPK-1,NXY)
      IF(NXY-NENDA.LE.LMINPK/2)NENDA=NXY
C        ABOVE STATEMENT GUARANTEES THE LAST GROUP IS GT LMINPK/2 BY 
C        MAKING THE ACTUAL GROUP LARGER.  IF A PROVISION LIKE THIS IS 
C        NOT INCLUDED, THERE WILL MANY TIMES BE A VERY SMALL GROUP
C        AT THE END.  USE SEPARATE LOOPS FOR MISSING AND NO MISSING
C        VALUES FOR EFFICIENCY.
C
C        DETERMINE WHETHER THERE IS A LONG STRING OF THE SAME VALUE
C        UNLESS NENDA = NXY.  THIS MAY ALLOW A LARGE GROUP A TO
C        START WITH, AS WITH MISSING VALUES.   SEPARATE LOOPS FOR
C        MISSING OPTIONS.  THIS SECTION IS ONLY EXECUTED ONCE,
C        IN DETERMINING THE FIRST GROUP.  IT HELPS FOR AN ARRAY
C        OF MOSTLY MISSING VALUES OR OF ONE VALUE, SUCH AS
C        RADAR OR PRECIP DATA.
C
      IF(NENDA.NE.NXY.AND.IC(KSTART).EQ.IC(KSTART+1))THEN
C           NO NEED TO EXECUTE IF FIRST TWO VALUES ARE NOT EQUAL.
C
         IF(IS523.EQ.0)THEN
C              THIS LOOP IS FOR NO MISSING VALUES.
C
            DO 111 K=KSTART+1,NXY
C
               IF(IC(K).NE.IC(KSTART))THEN
                  NENDA=MAX(NENDA,K-1)
                  GO TO 114
               ENDIF
C
 111        CONTINUE
C
            NENDA=NXY
C              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
C
         ELSEIF(IS523.EQ.1)THEN
C              THIS LOOP IS FOR PRIMARY MISSING VALUES ONLY.
C
            DO 112 K=KSTART+1,NXY
C        
               IF(IC(K).NE.MISSP)THEN
C
                  IF(IC(K).NE.IC(KSTART))THEN
                     NENDA=MAX(NENDA,K-1)
                     GO TO 114
                  ENDIF
C
               ENDIF
C
 112        CONTINUE
C
            NENDA=NXY
C              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
C
         ELSE
C              THIS LOOP IS FOR PRIMARY AND SECONDARY MISSING VALUES.
C
            DO 113 K=KSTART+1,NXY
C        
               IF(IC(K).NE.MISSP.AND.IC(K).NE.MISSS)THEN
C
                  IF(IC(K).NE.IC(KSTART))THEN
                     NENDA=MAX(NENDA,K-1)
                     GO TO 114
                  ENDIF
C
               ENDIF
C
 113        CONTINUE
C
            NENDA=NXY
C              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
         ENDIF
C
      ENDIF
C
 114  IF(IS523.EQ.0)THEN
C
         DO 115 K=KSTART,NENDA
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
            MINAK=K
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
            MAXAK=K
         ENDIF
 115     CONTINUE
C
      ELSEIF(IS523.EQ.1)THEN
C
         DO 117 K=KSTART,NENDA
         IF(IC(K).EQ.MISSP)GO TO 117
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
            MINAK=K
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
            MAXAK=K
         ENDIF
 117     CONTINUE
C
      ELSE
C
         DO 120 K=KSTART,NENDA
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 120
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
            MINAK=K
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
            MAXAK=K
         ENDIF
 120     CONTINUE
C
      ENDIF
C
      KOUNTA=NENDA-KSTART+1
C
C        INCREMENT KTOTAL AND FIND THE BITS NEEDED TO PACK THE A GROUP.
C
      KTOTAL=KTOTAL+KOUNTA
      MISLLA=0
      IF(MINA.NE.MALLOW)GO TO 125
C        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINA=0
      MAXA=0
      MISLLA=1
      IBITB=0
      IF(IS523.NE.2)GO TO 130
C        WHEN ALL VALUES ARE MISSING AND THERE ARE NO
C        SECONDARY MISSING VALUES, IBITA = 0.
C        OTHERWISE, IBITA MUST BE CALCULATED.
C
 125  ITEST=MAXA-MINA+LMISS
C  
      DO 126 IBITA=0,30
      IF(ITEST.LT.IBXX2(IBITA))GO TO 130
C***        THIS TEST IS THE SAME AS:
C***     IF(MAXA-MINA.LT.IBXX2(IBITA)-LMISS)GO TO 130
 126  CONTINUE
C
C     WRITE(KFILDO,127)MAXA,MINA
C127  FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
C    1       '  MAXA ='I13,'  MINA ='I13,'.  ERROR AT 127.')
      IER=706
      GO TO 900
C
 130  CONTINUE
C
C***D     WRITE(KFILDO,131)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA
C***D131  FORMAT(' AT 130, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
C***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3) 
C
 133  IF(KTOTAL.GE.NXY)GO TO 200
C
C        *************************************
C
C        THIS SECTION COMPUTES STATISTICS FOR GROUP B.  GROUP B IS A
C        GROUP OF SIZE LMINPK IMMEDIATELY FOLLOWING GROUP A.
C
C        *************************************
C
 140  MINB=MALLOW
      MAXB=-MALLOW
      MINBK=MALLOW
      MAXBK=-MALLOW
      IBITBS=0
      MSTART=KTOTAL+1
C
C        DETERMINE WHETHER THERE IS A LONG STRING OF THE SAME VALUE.
C        THIS WORKS WHEN THERE ARE NO MISSING VALUES.
C
      NENDB=1
C
      IF(MSTART.LT.NXY)THEN
C
         IF(IS523.EQ.0)THEN
C              THIS LOOP IS FOR NO MISSING VALUES.
C
            DO 145 K=MSTART+1,NXY
C
               IF(IC(K).NE.IC(MSTART))THEN
                  NENDB=K-1
                  GO TO 150
               ENDIF
C
 145        CONTINUE
C
            NENDB=NXY
C              FALL THROUGH THE LOOP MEANS ALL REMAINING VALUES
C              ARE THE SAME.
         ENDIF
C
      ENDIF
C         
 150  NENDB=MAX(NENDB,MIN(KTOTAL+LMINPK,NXY))
C**** 150  NENDB=MIN(KTOTAL+LMINPK,NXY)
C
      IF(NXY-NENDB.LE.LMINPK/2)NENDB=NXY
C        ABOVE STATEMENT GUARANTEES THE LAST GROUP IS GT LMINPK/2 BY 
C        MAKING THE ACTUAL GROUP LARGER.  IF A PROVISION LIKE THIS IS 
C        NOT INCLUDED, THERE WILL MANY TIMES BE A VERY SMALL GROUP
C        AT THE END.  USE SEPARATE LOOPS FOR MISSING AND NO MISSING
C
C        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
C        FOR EFFICIENCY.
C
      IF(IS523.EQ.0)THEN
C              
         DO 155 K=MSTART,NENDB
         IF(IC(K).LE.MINB)THEN
            MINB=IC(K)
C              NOTE LE, NOT LT.  LT COULD BE USED BUT THEN A 
C              RECOMPUTE OVER THE WHOLE GROUP WOULD BE NEEDED
C              MORE OFTEN.  SAME REASONING FOR GE AND OTHER
C              LOOPS BELOW.
            MINBK=K
         ENDIF
         IF(IC(K).GE.MAXB)THEN
            MAXB=IC(K)
            MAXBK=K
         ENDIF
 155     CONTINUE
C
      ELSEIF(IS523.EQ.1)THEN
C
         DO 157 K=MSTART,NENDB
         IF(IC(K).EQ.MISSP)GO TO 157
         IF(IC(K).LE.MINB)THEN
            MINB=IC(K)
            MINBK=K
         ENDIF
         IF(IC(K).GE.MAXB)THEN
            MAXB=IC(K)
            MAXBK=K
         ENDIF
 157     CONTINUE
C
      ELSE
C
         DO 160 K=MSTART,NENDB
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 160
         IF(IC(K).LE.MINB)THEN
            MINB=IC(K)
            MINBK=K
         ENDIF
         IF(IC(K).GE.MAXB)THEN
            MAXB=IC(K)
            MAXBK=K
         ENDIF
 160     CONTINUE
C
      ENDIF
C
      KOUNTB=NENDB-KTOTAL
      MISLLB=0
      IF(MINB.NE.MALLOW)GO TO 165
C        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINB=0
      MAXB=0
      MISLLB=1
      IBITB=0
C
      IF(IS523.NE.2)GO TO 170
C        WHEN ALL VALUES ARE MISSING AND THERE ARE NO SECONDARY
C        MISSING VALUES, IBITB = 0.  OTHERWISE, IBITB MUST BE
C        CALCULATED.
C
 165  DO 166 IBITB=IBITBS,30
         IF(MAXB-MINB.LT.IBXX2(IBITB)-LMISS)GO TO 170
 166  CONTINUE
C
C     WRITE(KFILDO,167)MAXB,MINB
C167  FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
C    1       '  MAXB ='I13,'  MINB ='I13,'.  ERROR AT 167.')
      IER=706
      GO TO 900
C
C        COMPARE THE BITS NEEDED TO PACK GROUP B WITH THOSE NEEDED
C        TO PACK GROUP A.  IF IBITB GE IBITA, TRY TO ADD TO GROUP A.
C        IF NOT, TRY TO ADD A'S POINTS TO B, UNLESS ADDITION TO A
C        HAS BEEN DONE.  THIS LATTER IS CONTROLLED WITH ADDA.
C
 170  CONTINUE
C
C***D     WRITE(KFILDO,171)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA,
C***D    1                               MINB,MAXB,IBITB,MISLLB
C***D171  FORMAT(' AT 171, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
C***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3,
C***D    2       '  MINB ='I8,'  MAXB ='I8,'  IBITB ='I3,'  MISLLB ='I3)  
C
      IF(IBITB.GE.IBITA)GO TO 180
      IF(ADDA)GO TO 200
C
C        *************************************
C
C        GROUP B REQUIRES LESS BITS THAN GROUP A.  PUT AS MANY OF A'S
C        POINTS INTO B AS POSSIBLE WITHOUT EXCEEDING THE NUMBER OF
C        BITS NECESSARY TO PACK GROUP B.
C
C        *************************************
C
      KOUNTS=KOUNTA
C        KOUNTA REFERS TO THE PRESENT GROUP A.
      MINTST=MINB
      MAXTST=MAXB
      MINTSTK=MINBK
      MAXTSTK=MAXBK
C
C        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
C        FOR EFFICIENCY.
C
      IF(IS523.EQ.0)THEN
C 
         DO 1715 K=KTOTAL,KSTART,-1
C           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB))GO TO 174
C           NOTE THAT FOR THIS LOOP, LMISS = 0.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         KOUNTA=KOUNTA-1
C           THERE IS ONE LESS POINT NOW IN A.
 1715    CONTINUE  
C
      ELSEIF(IS523.EQ.1)THEN            
C
         DO 1719 K=KTOTAL,KSTART,-1
C           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).EQ.MISSP)GO TO 1718
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB)-LMISS)GO TO 174
C           FOR THIS LOOP, LMISS = 1.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         MISLLB=0
C           WHEN THE POINT IS NON MISSING, MISLLB SET = 0.
 1718    KOUNTA=KOUNTA-1
C           THERE IS ONE LESS POINT NOW IN A.
 1719    CONTINUE  
C
      ELSE             
C
         DO 173 K=KTOTAL,KSTART,-1
C           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 1729
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB)-LMISS)GO TO 174
C           FOR THIS LOOP, LMISS = 2.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         MISLLB=0
C           WHEN THE POINT IS NON MISSING, MISLLB SET = 0.
 1729    KOUNTA=KOUNTA-1
C           THERE IS ONE LESS POINT NOW IN A.
 173     CONTINUE  
C
      ENDIF
C
C        AT THIS POINT, KOUNTA CONTAINS THE NUMBER OF POINTS TO CLOSE
C        OUT GROUP A WITH.  GROUP B NOW STARTS WITH KSTART+KOUNTA AND
C        ENDS WITH NENDB.  MINB AND MAXB HAVE BEEN ADJUSTED AS
C        NECESSARY TO REFLECT GROUP B (EVEN THOUGH THE NUMBER OF BITS
C        NEEDED TO PACK GROUP B HAVE NOT INCREASED, THE END POINTS
C        OF THE RANGE MAY HAVE).
C
 174  IF(KOUNTA.EQ.KOUNTS)GO TO 200
C        ON TRANSFER, GROUP A WAS NOT CHANGED.  CLOSE IT OUT.
C
C        ONE OR MORE POINTS WERE TAKEN OUT OF A.  RANGE AND IBITA
C        MAY HAVE TO BE RECOMPUTED; IBITA COULD BE LESS THAN
C        ORIGINALLY COMPUTED.  IN FACT, GROUP A CAN NOW CONTAIN
C        ONLY ONE POINT AND BE PACKED WITH ZERO BITS
C        (UNLESS MISSS NE 0).
C
      NOUTA=KOUNTS-KOUNTA
      KTOTAL=KTOTAL-NOUTA
      KOUNTB=KOUNTB+NOUTA
      IF(NENDA-NOUTA.GT.MINAK.AND.NENDA-NOUTA.GT.MAXAK)GO TO 200
C        WHEN THE ABOVE TEST IS MET, THE MIN AND MAX OF THE 
C        CURRENT GROUP A WERE WITHIN THE OLD GROUP A, SO THE
C        RANGE AND IBITA DO NOT NEED TO BE RECOMPUTED.
C        NOTE THAT MINAK AND MAXAK ARE NO LONGER NEEDED.
      IBITA=0
      MINA=MALLOW
      MAXA=-MALLOW
C
C        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
C        FOR EFFICIENCY.
C
      IF(IS523.EQ.0)THEN
C 
         DO 1742 K=KSTART,NENDA-NOUTA
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 1742    CONTINUE
C
      ELSEIF(IS523.EQ.1)THEN 
C
         DO 1744 K=KSTART,NENDA-NOUTA
         IF(IC(K).EQ.MISSP)GO TO 1744
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 1744    CONTINUE
C
      ELSE 
C
         DO 175 K=KSTART,NENDA-NOUTA
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 175
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 175     CONTINUE
C
      ENDIF
C
      MISLLA=0
      IF(MINA.NE.MALLOW)GO TO 1750
C        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINA=0
      MAXA=0
      MISLLA=1
      IF(IS523.NE.2)GO TO 177
C        WHEN ALL VALUES ARE MISSING AND THERE ARE NO SECONDARY
C        MISSING VALUES IBITA = 0 AS ORIGINALLY SET.  OTHERWISE,
C        IBITA MUST BE CALCULATED.
C
 1750 ITEST=MAXA-MINA+LMISS
C
      DO 176 IBITA=0,30
      IF(ITEST.LT.IBXX2(IBITA))GO TO 177
C***        THIS TEST IS THE SAME AS:
C***         IF(MAXA-MINA.LT.IBXX2(IBITA)-LMISS)GO TO 177
 176  CONTINUE
C
C     WRITE(KFILDO,1760)MAXA,MINA
C1760 FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
C    1       '  MAXA ='I13,'  MINA ='I13,'.  ERROR AT 1760.')
      IER=706
      GO TO 900
C
 177  CONTINUE
      GO TO 200
C
C        *************************************
C
C        AT THIS POINT, GROUP B REQUIRES AS MANY BITS TO PACK AS GROUPA.
C        THEREFORE, TRY TO ADD INC POINTS TO GROUP A WITHOUT INCREASING
C        IBITA.  THIS AUGMENTED GROUP IS CALLED GROUP C.
C
C        *************************************
C
 180  IF(MISLLA.EQ.1)THEN
         MINC=MALLOW
         MINCK=MALLOW
         MAXC=-MALLOW
         MAXCK=-MALLOW
      ELSE
         MINC=MINA
         MAXC=MAXA
         MINCK=MINAK
         MAXCK=MINAK
      ENDIF
C
      NOUNT=0
      IF(NXY-(KTOTAL+KINC).LE.LMINPK/2)KINC=NXY-KTOTAL
C        ABOVE STATEMENT CONSTRAINS THE LAST GROUP TO BE NOT LESS THAN
C        LMINPK/2 IN SIZE.  IF A PROVISION LIKE THIS IS NOT INCLUDED,
C        THERE WILL MANY TIMES BE A VERY SMALL GROUP AT THE END.
C
C        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
C        FOR EFFICIENCY.  SINCE KINC IS USUALLY 1, USING SEPARATE
C        LOOPS HERE DOESN'T BUY MUCH.  A MISSING VALUE WILL ALWAYS
C        TRANSFER BACK TO GROUP A.
C
      IF(IS523.EQ.0)THEN
C
         DO 185 K=KTOTAL+1,MIN(KTOTAL+KINC,NXY)
         IF(IC(K).LT.MINC)THEN
            MINC=IC(K)
            MINCK=K
         ENDIF
         IF(IC(K).GT.MAXC)THEN
            MAXC=IC(K)
            MAXCK=K
         ENDIF
         NOUNT=NOUNT+1
 185     CONTINUE
C
      ELSEIF(IS523.EQ.1)THEN
C
         DO 187 K=KTOTAL+1,MIN(KTOTAL+KINC,NXY)
         IF(IC(K).EQ.MISSP)GO TO 186
         IF(IC(K).LT.MINC)THEN
            MINC=IC(K)
            MINCK=K
         ENDIF
         IF(IC(K).GT.MAXC)THEN
            MAXC=IC(K)
            MAXCK=K
         ENDIF
 186     NOUNT=NOUNT+1
 187     CONTINUE
C
      ELSE
C
         DO 190 K=KTOTAL+1,MIN(KTOTAL+KINC,NXY)
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 189
         IF(IC(K).LT.MINC)THEN
            MINC=IC(K)
            MINCK=K
         ENDIF
         IF(IC(K).GT.MAXC)THEN
            MAXC=IC(K)
            MAXCK=K
         ENDIF
 189     NOUNT=NOUNT+1
 190     CONTINUE
C
      ENDIF
C
C***D     WRITE(KFILDO,191)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA,
C***D    1   MINC,MAXC,NOUNT,IC(KTOTAL),IC(KTOTAL+1)
C***D191  FORMAT(' AT 191, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
C***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3,
C***D    2       '  MINC ='I8,'  MAXC ='I8,
C***D    3       '  NOUNT ='I5,'  IC(KTOTAL) ='I9,'  IC(KTOTAL+1) =',I9) 
C
C        IF THE NUMBER OF BITS NEEDED FOR GROUP C IS GT IBITA,
C        THEN THIS GROUP A IS A GROUP TO PACK.
C
      IF(MINC.EQ.MALLOW)THEN
         MINC=MINA
         MAXC=MAXA
         MINCK=MINAK
         MAXCK=MAXAK
         MISLLC=1
         GO TO 195
C           WHEN THE NEW VALUE(S) ARE MISSING, THEY CAN ALWAYS
C           BE ADDED.
C
      ELSE
         MISLLC=0
      ENDIF
C
      IF(MAXC-MINC.GE.IBXX2(IBITA)-LMISS) GO TO 200
C
C        THE BITS NECESSARY FOR GROUP C HAS NOT INCREASED FROM THE
C        BITS NECESSARY FOR GROUP A.  ADD THIS POINT(S) TO GROUP A.
C        COMPUTE THE NEXT GROUP B, ETC., UNLESS ALL POINTS HAVE BEEN
C        USED.
C 
 195  KTOTAL=KTOTAL+NOUNT
      KOUNTA=KOUNTA+NOUNT
      MINA=MINC
      MAXA=MAXC
      MINAK=MINCK
      MAXAK=MAXCK
      MISLLA=MISLLC
      ADDA=.TRUE.
      IF(KTOTAL.GE.NXY)GO TO 200
C
      IF(MINBK.GT.KTOTAL.AND.MAXBK.GT.KTOTAL)THEN
         MSTART=NENDB+1
C           THE MAX AND MIN OF GROUP B WERE NOT FROM THE POINTS
C           REMOVED, SO THE WHOLE GROUP DOES NOT HAVE TO BE LOOKED
C           AT TO DETERMINE THE NEW MAX AND MIN.  RATHER START
C           JUST BEYOND THE OLD NENDB.
         IBITBS=IBITB
         NENDB=1
         GO TO 150
      ELSE
         GO TO 140
      ENDIF
C
C        *************************************
C
C        GROUP A IS TO BE PACKED.  STORE VALUES IN JMIN( ), JMAX( ),
C        LBIT( ), AND NOV( ).
C
C        *************************************
C
 200  LX=LX+1
      IF(LX.LE.NDG)GO TO 205
      LMINPK=LMINPK+LMINPK/2
C     WRITE(KFILDO,201)NDG,LMINPK,LX
C201  FORMAT(' ****NDG ='I5,' NOT LARGE ENOUGH.',
C    1       '  LMINPK IS INCREASED TO 'I3,' FOR THIS FIELD.'/
C    2       '  LX = 'I10)
      IERSAV=716
      GO TO 105
C
 205  JMIN(LX)=MINA
      JMAX(LX)=MAXA
      LBIT(LX)=IBITA
      NOV(LX)=KOUNTA
      KSTART=KTOTAL+1
C
      IF(MISLLA.EQ.0)THEN
         MISSLX(LX)=MALLOW
      ELSE
         MISSLX(LX)=IC(KTOTAL)
C           IC(KTOTAL) WAS THE LAST VALUE PROCESSED.  IF MISLLA NE 0,
C           THIS MUST BE THE MISSING VALUE FOR THIS GROUP.
      ENDIF
C
C***D     WRITE(KFILDO,206)MISLLA,IC(KTOTAL),KTOTAL,LX,JMIN(LX),JMAX(LX),
C***D    1                 LBIT(LX),NOV(LX),MISSLX(LX)
C***D206  FORMAT(' AT 206,  MISLLA ='I2,'  IC(KTOTAL) ='I5,'  KTOTAL ='I8,
C***D    1       '  LX ='I6,'  JMIN(LX) ='I8,'  JMAX(LX) ='I8,
C***D    2       '  LBIT(LX) ='I5,'  NOV(LX) ='I8,'  MISSLX(LX) =',I7) 
C
      IF(KTOTAL.GE.NXY)GO TO 209
C
C        THE NEW GROUP A WILL BE THE PREVIOUS GROUP B.  SET LIMITS, ETC.
C
      IBITA=IBITB
      MINA=MINB
      MAXA=MAXB
      MINAK=MINBK
      MAXAK=MAXBK
      MISLLA=MISLLB
      NENDA=NENDB
      KOUNTA=KOUNTB
      KTOTAL=KTOTAL+KOUNTA
      ADDA=.FALSE.
      GO TO 133
C
C        *************************************
C
C        CALCULATE IBIT, THE NUMBER OF BITS NEEDED TO HOLD THE GROUP
C        MINIMUM VALUES.
C
C        *************************************
C
 209  IBIT=0
C
      DO 220 L=1,LX
 210  IF(JMIN(L).LT.IBXX2(IBIT))GO TO 220
      IBIT=IBIT+1
      GO TO 210
 220  CONTINUE
C
C        INSERT THE VALUE IN JMIN( ) TO BE USED FOR ALL MISSING
C        VALUES WHEN LBIT( ) = 0.  WHEN SECONDARY MISSING 
C        VALUES CAN BE PRESENT, LBIT(L) WILL NOT = 0.
C
      IF(IS523.EQ.1)THEN
C
         DO 226 L=1,LX
C   
         IF(LBIT(L).EQ.0)THEN
C
            IF(MISSLX(L).EQ.MISSP)THEN
               JMIN(L)=IBXX2(IBIT)-1
            ENDIF
C
         ENDIF
C
 226     CONTINUE
C
      ENDIF
C
C        *************************************
C
C        CALCULATE JBIT, THE NUMBER OF BITS NEEDED TO HOLD THE BITS
C        NEEDED TO PACK THE VALUES IN THE GROUPS.  BUT FIND AND
C        REMOVE THE REFERENCE VALUE FIRST.
C
C        *************************************
C
C     WRITE(KFILDO,228)CFEED,LX
C228  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP WIDTHS LBIT( ) FOR ',I8,' GROUPS'
C    2          /' *****************************************')
C     WRITE(KFILDO,229) (LBIT(J),J=1,MIN(LX,100))
C229  FORMAT(/' '20I6)
C
      LBITREF=LBIT(1)
C
      DO 230 K=1,LX
      IF(LBIT(K).LT.LBITREF)LBITREF=LBIT(K)
 230  CONTINUE
C
      IF(LBITREF.NE.0)THEN
C
         DO 240 K=1,LX
         LBIT(K)=LBIT(K)-LBITREF
 240     CONTINUE
C
      ENDIF
C
C     WRITE(KFILDO,241)CFEED,LBITREF
C241  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP WIDTHS LBIT( ) AFTER REMOVING REFERENCE ',
C    2             I8,
C    3          /' *****************************************')
C     WRITE(KFILDO,242) (LBIT(J),J=1,MIN(LX,100))
C242  FORMAT(/' '20I6)
C
      JBIT=0
C
      DO 320 K=1,LX
 310  IF(LBIT(K).LT.IBXX2(JBIT))GO TO 320
      JBIT=JBIT+1
      GO TO 310
 320  CONTINUE
C
C        *************************************
C
C        CALCULATE KBIT, THE NUMBER OF BITS NEEDED TO HOLD THE NUMBER
C        OF VALUES IN THE GROUPS.  BUT FIND AND REMOVE THE
C        REFERENCE FIRST.
C
C        *************************************
C
C     WRITE(KFILDO,321)CFEED,LX
C321  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP SIZES NOV( ) FOR ',I8,' GROUPS'
C    2          /' *****************************************')
C     WRITE(KFILDO,322) (NOV(J),J=1,MIN(LX,100))
C322  FORMAT(/' '20I6)
C
      NOVREF=NOV(1)
C
      DO 400 K=1,LX
      IF(NOV(K).LT.NOVREF)NOVREF=NOV(K)
 400  CONTINUE
C
      IF(NOVREF.GT.0)THEN
C
         DO 405 K=1,LX
         NOV(K)=NOV(K)-NOVREF
 405     CONTINUE
C
      ENDIF
C
C     WRITE(KFILDO,406)CFEED,NOVREF
C406  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP SIZES NOV( ) AFTER REMOVING REFERENCE ',I8,
C    2          /' *****************************************')
C     WRITE(KFILDO,407) (NOV(J),J=1,MIN(LX,100))
C407  FORMAT(/' '20I6)
C     WRITE(KFILDO,408)CFEED
C408  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP REFERENCES JMIN( )'
C    2          /' *****************************************')
C     WRITE(KFILDO,409) (JMIN(J),J=1,MIN(LX,100))
C409  FORMAT(/' '20I6)
C
      KBIT=0
C
      DO 420 K=1,LX
 410  IF(NOV(K).LT.IBXX2(KBIT))GO TO 420
      KBIT=KBIT+1
      GO TO 410
 420  CONTINUE
C
C        DETERMINE WHETHER THE GROUP SIZES SHOULD BE REDUCED
C        FOR SPACE EFFICIENCY.
C
      IF(IRED.EQ.0)THEN
         CALL REDUCE(KFILDO,JMIN,JMAX,LBIT,NOV,LX,NDG,IBIT,JBIT,KBIT,
     1               NOVREF,IBXX2,IER)
C
         IF(IER.EQ.714.OR.IER.EQ.715)THEN
C              REDUCE HAS ABORTED.  REEXECUTE PACK_GP WITHOUT REDUCE.
C              PROVIDE FOR A NON FATAL RETURN FROM REDUCE.  
            IERSAV=IER
            IRED=1
            IER=0
            GO TO 102 
         ENDIF
C
      ENDIF         
C
C     CALL TIMPR(KFILDO,KFILDO,'END   PACK_GP        ')
      IF(IERSAV.NE.0)THEN
         IER=IERSAV
         RETURN
      ENDIF
C
C 900  IF(IER.NE.0)RETURN1
C
 900  RETURN
      END
