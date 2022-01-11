!>    @file
!>    @brief This subroutine determines groups of variable size.
!>    @author Harry Glahn @date 1994-02-01
!>

!>    This subroutine determines groups of variable size, but at least
!>    of size minpk, the associated max(JMAX) and min(JMIN), the number
!>    of bits necessary to hold the values in each group LBIT, the
!>    number of values in each group NOV, the number of bits necessary
!>    to pack the JMIN values IBIT, the number of bits necessary to pack
!>    the LBIT values JBIT, and the number of bits necessary to pack the
!>    NOV values KBIT. The routine is designed to determine the groups
!>    such that a small number of bits is necessary to pack the data
!>    without excessive computations. If all values in the group are
!>    zero, the number of bits to use in packing is defined as zero when
!>    there can be no missing values; when there can be missing values,
!>    the number of bits must be at least 1 to have the capability to
!>    recognize the missing value. However, if all values in a group are
!>    missing, the number of bits needed is 0, and the unpacker recognizes
!>    this. All variables are integer. even though the groups are
!>    initially of size minpk or larger, an adjustment between two groups
!>    (the lookback procedure) may make a group smaller than minpk. The
!>    control on group size is that the sum of the sizes of the two
!>    consecutive groups, each of size minpk or larger, is not decreased.
!>    When determining the number of bits necessary for packing, the
!>    largest value that can be accommodated in, say mbits is 2**mbits-1
!>    this largest value (and the next smallest value) is reserved for
!>    the missing value indicator (only) when is523 ne 0. If the
!>    dimension NDG is not large enough to hold all the groups, the
!>    local value of minpk is increased by 50 percent. this is repeated
!>    until ndg will suffice. A diagnostic is printed whenever this
!>    happens, which should be very rarely. If it happens often, NDG in
!>    subroutine pack should be increased and a corresponding increase
!>    in subroutine unpack made. Considerable code is provided so that
!>    no more checking for missing values within loops is done than
!>    necessary; the added efficiency of this is relatively minor,
!>    but does no harm. For grib2, the reference value for the length
!>    of groups in nov and for the number of bits necessary to pack
!>    group values are determined, and subtracted before jbit and kbit
!>    are determined. When 1 or more groups are large compared to the
!>    others, the width of all groups must be as large as the largest.
!>    A subroutine reduce breaks up large groups into 2 or more to reduce
!>    total bits required. If reduce should abort, pack_gp will be
!>    executed again without the call to reduce.
!>
!>    PROGRAM HISTORY LOG:
!>    - 1994-02-01 Harry Glahn tdl mos-2000.
!>    - 1995-06-01 Harry Glahn modified for lmiss error.
!>    - 1996-07-01 Harry Glahn added misss.
!>    - 1997-02-01 Harry Glahn removed 4 redundant tests for missp.eq.0;
!>    inserted a test to better handle a string of 9999's.
!>    - 1997-02-01 Harry Glahn added loops to eliminate test for misss
!>    when misss = 0.
!>    - 1997-03-01 Harry Glahn corrected for secondary missing value.
!>    - 1997-03-01 Harry Glahn corrected for use of local value of minpk.
!>    - 1997-03-01 Harry Glahn corrected for secondary missing value.
!>    - 1997-03-01 Harry Glahn changed calculating number of bits
!>    through exponents to an array (improved overall packing performance
!>    by about 35 percent). Allowed 0 bit for packing JMIN, LBIT, and NOV.
!>    - 1997-05-01 Harry Glahn a number of changes for efficiency. mod
!>    functions eliminated and one ifthen added. Jount removed.
!>    Recomputation of bits not made unless necessary after moving points
!>    from one group to another. Nendb adjusted to eliminate possibility
!>    of very small group at the end. About 8 percent improvement in
!>    overall packing. ISKIPA removed; There is always a group b that can
!>    become group A. Control on size of group b (statement below 150)
!>    added. Added adda, and use of ge and le instead of gt and lt in
!>    loop between 150 and 160. IBITBS added to shorten trip through loop.
!>    - 2000-03-01 Harry Glahn modified for grib2; changed name from
!>    packgp.
!>    - 2001-01-01 Harry Glahn Add comments; ier = 706 substituted for
!>    stops; added return; removed statement number 110; added ier.
!>    - 2001-11-01 Harry Glahn changed some diagnostic formats to
!>    allow printing larger numbers
!>    - 2001-11-01 Harry Glahn added misslx to put maximum value into JMIN
!>    when all values missing to agree with grib standard.
!>    - 2001-11-01 Harry Glahn changed two tests on missp and misss eq 0
!>    to tests on is523. However, missp and misss cannot in general be 0.
!>    - 2001-11-01 Harry Glahn added call to reduce; defined itest
!>    before loops to reduce computation; started large group when all
!>    same value.
!>    - 2001-12-01 Harry Glahn modified and added a few comments.
!>    - 2002-01-01 Harry Glahn removed loop before 150 to determine
!>    a group of all same value.
!>    - 2002-01-01 Harry Glahn changed mallow from 9999999 to 2**30+1,
!>    and made it a parameter.
!>    - 2002-03-01 Harry Glahn added non fatal ier = 716, 717; removed
!>    nendb=nxy above 150; added iersav=0.
!>
!>    @param[in] KFILDO unit number for output/print file.
!>    @param[in] IC array to hold data for packing. The values do not
!>    have to be positive at this point, but must be in the range
!>    -2**30 to +2**30 (the value of mallow). These integer values
!>    will be retained exactly through packing and unpacking.
!>    @param[in] NXY number of values in IC. also treated as
!>    its dimension.
!>    @param[in] IS523 missing value management 0=data contains no
!>    missing values: 1 data contains primary missing values; 2=data
!>    contains primary and secondary missing values.
!>    @param[in] MINPK the minimum size of each group, except possibly
!>    the last one.
!>    @param[in] INC the number of values to add to an already existing
!>    group in determining whether or not to start a new group. Ideally,
!>    this would be 1, but each time inc values are attempted, the max
!>    and min of the next minpk values must be found. This is "a loop
!>    within a loop," and a slightly larger value may give about as good
!>    results with slightly less computational time. If inc is le 0, 1
!>    is used, and a diagnostic is output. note: it is expected that
!>    INC will equal 1. The code uses inc primarily in the loops
!>    starting at statement 180. If INC were 1, there would not need
!>    to be loops as such. However, kinc (the local value of INC) is
!>    set ge 1 when near the end of the data to forestall a very small
!>    group at the end.
!>    @param[in] MISSP when missing points can be present in the data,
!>    they will have the value missp or misss. missp is the primary
!>    missing value and misss is the secondary missing value. These
!>    must not be values that would occur with subtracting the minimum
!>    (reference) value or scaling. for example, missp = 0 would not
!>    be advisable.
!>    @param[in] MISSS secondary missing value indicator (see missp).
!>    @param[out] JMIN the minimum of each group (j=1,lx).
!>    @param[out] JMAX the maximum of each group (j=1,lx). This is not
!>    really needed, but since the max of each group must be found,
!>    saving it here is cheap in case the user wants it.
!>    @param[out] LBIT the number of bits necessary to pack each group
!>    (j=1,lx). It is assumed the minimum of each group will be removed
!>    before packing, and the values to pack will, therefore, all be
!>    positive. However, IC does not necessarily contain all positive
!>    values. If the overall minimum has been removed (the usual case),
!>    then IC will contain only positive values.
!>    @param[out] NOV the number of values in each group (j=1,lx).
!>    @param[in] NDG the dimension of JMIN, JMAX, LBIT, and NOV.
!>    @param[out] LX the number of groups determined.
!>    @param[out] IBIT the number of bits necessary to pack the JMIN(j)
!>    values, j=1,LX.
!>    @param[out] JBIT the number of bits necessary to pack the LBIT(j)
!>    values, j=1,LX.
!>    @param[out] KBIT the number of bits necessary to pack the NOV(j)
!>    values, j=1,LX.
!>    @param[out] NOVREF reference value for NOV.
!>    @param[out] LBITREF reference value for LBIT.
!>    @param[out] IER error return.
!>    - 706 value will not pack in 30 bits--fatal
!>    - 714 error in reduce--non-fatal
!>    - 715 ngp not large enough in reduce--non-fatal
!>    - 716 minpk inceased--non-fatal
!>    - 717 inc set = 1--non-fatal
!>
!>    @author Harry Glahn @date 1994-02-01
SUBROUTINE PACK_GP(KFILDO,IC,NXY,IS523,MINPK,INC,MISSP,MISSS, &
     JMIN,JMAX,LBIT,NOV,NDG,LX,IBIT,JBIT,KBIT, &
     NOVREF,LBITREF,IER)            

      PARAMETER (MALLOW=2**30+1)
!
      CHARACTER*1 CFEED
      LOGICAL ADDA
!
      DIMENSION IC(NXY)
      DIMENSION JMIN(NDG),JMAX(NDG),LBIT(NDG),NOV(NDG)
      DIMENSION MISSLX(NDG)
!        MISSLX( ) IS AN AUTOMATIC ARRAY.
      INTEGER, PARAMETER :: IBXX2(0:30) = (/ 1, 2, 4, 8, 16, 32, 64,    & 
           128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536,  & 
           131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,  & 
           16777216, 33554432, 67108864, 134217728, 268435456,          & 
           536870912, 1073741824 /)
!
      
      PARAMETER IFEED=12
!
      IER=0
      IERSAV=0
!     CALL TIMPR(KFILDO,KFILDO,'START PACK_GP        ')
      CFEED=CHAR(IFEED)
!
      IRED=0
!        IRED IS A FLAG.  WHEN ZERO, REDUCE WILL BE CALLED.
!        IF REDUCE ABORTS, IRED = 1 AND IS NOT CALLED.  IN
!        THIS CASE PACK_GP EXECUTES AGAIN EXCEPT FOR REDUCE.
!
      IF(INC.LE.0)THEN
         IERSAV=717
!        WRITE(KFILDO,101)INC
!101     FORMAT(/' ****INC ='I8,' NOT CORRECT IN PACK_GP.  1 IS USED.')
      ENDIF
!
!        THERE WILL BE A RESTART OF PACK_GP IF SUBROUTINE REDUCE
!        ABORTS.  THIS SHOULD NOT HAPPEN, BUT IF IT DOES, PACK_GP
!        WILL COMPLETE WITHOUT SUBROUTINE REDUCE.  A NON FATAL
!        DIAGNOSTIC RETURN IS PROVIDED.
!
 102  KINC=MAX(INC,1)
      LMINPK=MINPK
!
!        THERE WILL BE A RESTART AT 105 IS NDG IS NOT LARGE ENOUGH.
!        A NON FATAL DIAGNOSTIC RETURN IS PROVIDED.
!
 105  KSTART=1
      KTOTAL=0
      LX=0
      ADDA=.FALSE.
      LMISS=0
      IF(IS523.EQ.1)LMISS=1
      IF(IS523.EQ.2)LMISS=2
!
!        *************************************
!
!        THIS SECTION COMPUTES STATISTICS FOR GROUP A.  GROUP A IS
!        A GROUP OF SIZE LMINPK.
!
!        *************************************
!
      IBITA=0
      MINA=MALLOW
      MAXA=-MALLOW
      MINAK=MALLOW
      MAXAK=-MALLOW
!
!        FIND THE MIN AND MAX OF GROUP A.  THIS WILL INITIALLY BE OF
!        SIZE LMINPK (IF THERE ARE STILL LMINPK VALUES IN IC( )), BUT
!        WILL INCREASE IN SIZE IN INCREMENTS OF INC UNTIL A NEW
!        GROUP IS STARTED.  THE DEFINITION OF GROUP A IS DONE HERE
!        ONLY ONCE (UPON INITIAL ENTRY), BECAUSE A GROUP B CAN ALWAYS
!        BECOME A NEW GROUP A AFTER A IS PACKED, EXCEPT IF LMINPK 
!        HAS TO BE INCREASED BECAUSE NDG IS TOO SMALL.  THEREFORE,
!        THE SEPARATE LOOPS FOR MISSING AND NON-MISSING HERE BUYS
!        ALMOST NOTHING.
!
      NENDA=MIN(KSTART+LMINPK-1,NXY)
      IF(NXY-NENDA.LE.LMINPK/2)NENDA=NXY
!        ABOVE STATEMENT GUARANTEES THE LAST GROUP IS GT LMINPK/2 BY 
!        MAKING THE ACTUAL GROUP LARGER.  IF A PROVISION LIKE THIS IS 
!        NOT INCLUDED, THERE WILL MANY TIMES BE A VERY SMALL GROUP
!        AT THE END.  USE SEPARATE LOOPS FOR MISSING AND NO MISSING
!        VALUES FOR EFFICIENCY.
!
!        DETERMINE WHETHER THERE IS A LONG STRING OF THE SAME VALUE
!        UNLESS NENDA = NXY.  THIS MAY ALLOW A LARGE GROUP A TO
!        START WITH, AS WITH MISSING VALUES.   SEPARATE LOOPS FOR
!        MISSING OPTIONS.  THIS SECTION IS ONLY EXECUTED ONCE,
!        IN DETERMINING THE FIRST GROUP.  IT HELPS FOR AN ARRAY
!        OF MOSTLY MISSING VALUES OR OF ONE VALUE, SUCH AS
!        RADAR OR PRECIP DATA.
!
      IF(NENDA.NE.NXY.AND.IC(KSTART).EQ.IC(KSTART+1))THEN
!           NO NEED TO EXECUTE IF FIRST TWO VALUES ARE NOT EQUAL.
!
         IF(IS523.EQ.0)THEN
!              THIS LOOP IS FOR NO MISSING VALUES.
!
            DO 111 K=KSTART+1,NXY
!
               IF(IC(K).NE.IC(KSTART))THEN
                  NENDA=MAX(NENDA,K-1)
                  GO TO 114
               ENDIF
!
 111        CONTINUE
!
            NENDA=NXY
!              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
!
         ELSEIF(IS523.EQ.1)THEN
!              THIS LOOP IS FOR PRIMARY MISSING VALUES ONLY.
!
            DO 112 K=KSTART+1,NXY
!        
               IF(IC(K).NE.MISSP)THEN
!
                  IF(IC(K).NE.IC(KSTART))THEN
                     NENDA=MAX(NENDA,K-1)
                     GO TO 114
                  ENDIF
!
               ENDIF
!
 112        CONTINUE
!
            NENDA=NXY
!              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
!
         ELSE
!              THIS LOOP IS FOR PRIMARY AND SECONDARY MISSING VALUES.
!
            DO 113 K=KSTART+1,NXY
!        
               IF(IC(K).NE.MISSP.AND.IC(K).NE.MISSS)THEN
!
                  IF(IC(K).NE.IC(KSTART))THEN
                     NENDA=MAX(NENDA,K-1)
                     GO TO 114
                  ENDIF
!
               ENDIF
!
 113        CONTINUE
!
            NENDA=NXY
!              FALL THROUGH THE LOOP MEANS ALL VALUES ARE THE SAME.
         ENDIF
!
      ENDIF
!
 114  IF(IS523.EQ.0)THEN
!
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
!
      ELSEIF(IS523.EQ.1)THEN
!
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
!
      ELSE
!
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
!
      ENDIF
!
      KOUNTA=NENDA-KSTART+1
!
!        INCREMENT KTOTAL AND FIND THE BITS NEEDED TO PACK THE A GROUP.
!
      KTOTAL=KTOTAL+KOUNTA
      MISLLA=0
      IF(MINA.NE.MALLOW)GO TO 125
!        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINA=0
      MAXA=0
      MISLLA=1
      IBITB=0
      IF(IS523.NE.2)GO TO 130
!        WHEN ALL VALUES ARE MISSING AND THERE ARE NO
!        SECONDARY MISSING VALUES, IBITA = 0.
!        OTHERWISE, IBITA MUST BE CALCULATED.
!
 125  ITEST=MAXA-MINA+LMISS
!  
      DO 126 IBITA=0,30
      IF(ITEST.LT.IBXX2(IBITA))GO TO 130
!***        THIS TEST IS THE SAME AS:
!***     IF(MAXA-MINA.LT.IBXX2(IBITA)-LMISS)GO TO 130
 126  CONTINUE
!
!     WRITE(KFILDO,127)MAXA,MINA
!127  FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
!    1       '  MAXA ='I13,'  MINA ='I13,'.  ERROR AT 127.')
      IER=706
      GO TO 900
!
 130  CONTINUE
!
!***D     WRITE(KFILDO,131)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA
!***D131  FORMAT(' AT 130, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
!***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3) 
!
 133  IF(KTOTAL.GE.NXY)GO TO 200
!
!        *************************************
!
!        THIS SECTION COMPUTES STATISTICS FOR GROUP B.  GROUP B IS A
!        GROUP OF SIZE LMINPK IMMEDIATELY FOLLOWING GROUP A.
!
!        *************************************
!
 140  MINB=MALLOW
      MAXB=-MALLOW
      MINBK=MALLOW
      MAXBK=-MALLOW
      IBITBS=0
      MSTART=KTOTAL+1
!
!        DETERMINE WHETHER THERE IS A LONG STRING OF THE SAME VALUE.
!        THIS WORKS WHEN THERE ARE NO MISSING VALUES.
!
      NENDB=1
!
      IF(MSTART.LT.NXY)THEN
!
         IF(IS523.EQ.0)THEN
!              THIS LOOP IS FOR NO MISSING VALUES.
!
            DO 145 K=MSTART+1,NXY
!
               IF(IC(K).NE.IC(MSTART))THEN
                  NENDB=K-1
                  GO TO 150
               ENDIF
!
 145        CONTINUE
!
            NENDB=NXY
!              FALL THROUGH THE LOOP MEANS ALL REMAINING VALUES
!              ARE THE SAME.
         ENDIF
!
      ENDIF
!         
 150  NENDB=MAX(NENDB,MIN(KTOTAL+LMINPK,NXY))
!**** 150  NENDB=MIN(KTOTAL+LMINPK,NXY)
!
      IF(NXY-NENDB.LE.LMINPK/2)NENDB=NXY
!        ABOVE STATEMENT GUARANTEES THE LAST GROUP IS GT LMINPK/2 BY 
!        MAKING THE ACTUAL GROUP LARGER.  IF A PROVISION LIKE THIS IS 
!        NOT INCLUDED, THERE WILL MANY TIMES BE A VERY SMALL GROUP
!        AT THE END.  USE SEPARATE LOOPS FOR MISSING AND NO MISSING
!
!        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
!        FOR EFFICIENCY.
!
      IF(IS523.EQ.0)THEN
!              
         DO 155 K=MSTART,NENDB
         IF(IC(K).LE.MINB)THEN
            MINB=IC(K)
!              NOTE LE, NOT LT.  LT COULD BE USED BUT THEN A 
!              RECOMPUTE OVER THE WHOLE GROUP WOULD BE NEEDED
!              MORE OFTEN.  SAME REASONING FOR GE AND OTHER
!              LOOPS BELOW.
            MINBK=K
         ENDIF
         IF(IC(K).GE.MAXB)THEN
            MAXB=IC(K)
            MAXBK=K
         ENDIF
 155     CONTINUE
!
      ELSEIF(IS523.EQ.1)THEN
!
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
!
      ELSE
!
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
!
      ENDIF
!
      KOUNTB=NENDB-KTOTAL
      MISLLB=0
      IF(MINB.NE.MALLOW)GO TO 165
!        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINB=0
      MAXB=0
      MISLLB=1
      IBITB=0
!
      IF(IS523.NE.2)GO TO 170
!        WHEN ALL VALUES ARE MISSING AND THERE ARE NO SECONDARY
!        MISSING VALUES, IBITB = 0.  OTHERWISE, IBITB MUST BE
!        CALCULATED.
!
 165  DO 166 IBITB=IBITBS,30
         IF(MAXB-MINB.LT.IBXX2(IBITB)-LMISS)GO TO 170
 166  CONTINUE
!
!     WRITE(KFILDO,167)MAXB,MINB
!167  FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
!    1       '  MAXB ='I13,'  MINB ='I13,'.  ERROR AT 167.')
      IER=706
      GO TO 900
!
!        COMPARE THE BITS NEEDED TO PACK GROUP B WITH THOSE NEEDED
!        TO PACK GROUP A.  IF IBITB GE IBITA, TRY TO ADD TO GROUP A.
!        IF NOT, TRY TO ADD A'S POINTS TO B, UNLESS ADDITION TO A
!        HAS BEEN DONE.  THIS LATTER IS CONTROLLED WITH ADDA.
!
 170  CONTINUE
!
!***D     WRITE(KFILDO,171)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA,
!***D    1                               MINB,MAXB,IBITB,MISLLB
!***D171  FORMAT(' AT 171, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
!***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3,
!***D    2       '  MINB ='I8,'  MAXB ='I8,'  IBITB ='I3,'  MISLLB ='I3)  
!
      IF(IBITB.GE.IBITA)GO TO 180
      IF(ADDA)GO TO 200
!
!        *************************************
!
!        GROUP B REQUIRES LESS BITS THAN GROUP A.  PUT AS MANY OF A'S
!        POINTS INTO B AS POSSIBLE WITHOUT EXCEEDING THE NUMBER OF
!        BITS NECESSARY TO PACK GROUP B.
!
!        *************************************
!
      KOUNTS=KOUNTA
!        KOUNTA REFERS TO THE PRESENT GROUP A.
      MINTST=MINB
      MAXTST=MAXB
      MINTSTK=MINBK
      MAXTSTK=MAXBK
!
!        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
!        FOR EFFICIENCY.
!
      IF(IS523.EQ.0)THEN
! 
         DO 1715 K=KTOTAL,KSTART,-1
!           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB))GO TO 174
!           NOTE THAT FOR THIS LOOP, LMISS = 0.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         KOUNTA=KOUNTA-1
!           THERE IS ONE LESS POINT NOW IN A.
 1715    CONTINUE  
!
      ELSEIF(IS523.EQ.1)THEN            
!
         DO 1719 K=KTOTAL,KSTART,-1
!           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).EQ.MISSP)GO TO 1718
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB)-LMISS)GO TO 174
!           FOR THIS LOOP, LMISS = 1.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         MISLLB=0
!           WHEN THE POINT IS NON MISSING, MISLLB SET = 0.
 1718    KOUNTA=KOUNTA-1
!           THERE IS ONE LESS POINT NOW IN A.
 1719    CONTINUE  
!
      ELSE             
!
         DO 173 K=KTOTAL,KSTART,-1
!           START WITH THE END OF THE GROUP AND WORK BACKWARDS.
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 1729
         IF(IC(K).LT.MINB)THEN
            MINTST=IC(K)
            MINTSTK=K
         ELSEIF(IC(K).GT.MAXB)THEN
            MAXTST=IC(K)
            MAXTSTK=K
         ENDIF
         IF(MAXTST-MINTST.GE.IBXX2(IBITB)-LMISS)GO TO 174
!           FOR THIS LOOP, LMISS = 2.
         MINB=MINTST
         MAXB=MAXTST
         MINBK=MINTSTK
         MAXBK=MAXTSTK
         MISLLB=0
!           WHEN THE POINT IS NON MISSING, MISLLB SET = 0.
 1729    KOUNTA=KOUNTA-1
!           THERE IS ONE LESS POINT NOW IN A.
 173     CONTINUE  
!
      ENDIF
!
!        AT THIS POINT, KOUNTA CONTAINS THE NUMBER OF POINTS TO CLOSE
!        OUT GROUP A WITH.  GROUP B NOW STARTS WITH KSTART+KOUNTA AND
!        ENDS WITH NENDB.  MINB AND MAXB HAVE BEEN ADJUSTED AS
!        NECESSARY TO REFLECT GROUP B (EVEN THOUGH THE NUMBER OF BITS
!        NEEDED TO PACK GROUP B HAVE NOT INCREASED, THE END POINTS
!        OF THE RANGE MAY HAVE).
!
 174  IF(KOUNTA.EQ.KOUNTS)GO TO 200
!        ON TRANSFER, GROUP A WAS NOT CHANGED.  CLOSE IT OUT.
!
!        ONE OR MORE POINTS WERE TAKEN OUT OF A.  RANGE AND IBITA
!        MAY HAVE TO BE RECOMPUTED; IBITA COULD BE LESS THAN
!        ORIGINALLY COMPUTED.  IN FACT, GROUP A CAN NOW CONTAIN
!        ONLY ONE POINT AND BE PACKED WITH ZERO BITS
!        (UNLESS MISSS NE 0).
!
      NOUTA=KOUNTS-KOUNTA
      KTOTAL=KTOTAL-NOUTA
      KOUNTB=KOUNTB+NOUTA
      IF(NENDA-NOUTA.GT.MINAK.AND.NENDA-NOUTA.GT.MAXAK)GO TO 200
!        WHEN THE ABOVE TEST IS MET, THE MIN AND MAX OF THE 
!        CURRENT GROUP A WERE WITHIN THE OLD GROUP A, SO THE
!        RANGE AND IBITA DO NOT NEED TO BE RECOMPUTED.
!        NOTE THAT MINAK AND MAXAK ARE NO LONGER NEEDED.
      IBITA=0
      MINA=MALLOW
      MAXA=-MALLOW
!
!        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
!        FOR EFFICIENCY.
!
      IF(IS523.EQ.0)THEN
! 
         DO 1742 K=KSTART,NENDA-NOUTA
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 1742    CONTINUE
!
      ELSEIF(IS523.EQ.1)THEN 
!
         DO 1744 K=KSTART,NENDA-NOUTA
         IF(IC(K).EQ.MISSP)GO TO 1744
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 1744    CONTINUE
!
      ELSE 
!
         DO 175 K=KSTART,NENDA-NOUTA
         IF(IC(K).EQ.MISSP.OR.IC(K).EQ.MISSS)GO TO 175
         IF(IC(K).LT.MINA)THEN
            MINA=IC(K)
         ENDIF
         IF(IC(K).GT.MAXA)THEN
            MAXA=IC(K)
         ENDIF
 175     CONTINUE
!
      ENDIF
!
      MISLLA=0
      IF(MINA.NE.MALLOW)GO TO 1750
!        ALL MISSING VALUES MUST BE ACCOMMODATED.
      MINA=0
      MAXA=0
      MISLLA=1
      IF(IS523.NE.2)GO TO 177
!        WHEN ALL VALUES ARE MISSING AND THERE ARE NO SECONDARY
!        MISSING VALUES IBITA = 0 AS ORIGINALLY SET.  OTHERWISE,
!        IBITA MUST BE CALCULATED.
!
 1750 ITEST=MAXA-MINA+LMISS
!
      DO 176 IBITA=0,30
      IF(ITEST.LT.IBXX2(IBITA))GO TO 177
!***        THIS TEST IS THE SAME AS:
!***         IF(MAXA-MINA.LT.IBXX2(IBITA)-LMISS)GO TO 177
 176  CONTINUE
!
!     WRITE(KFILDO,1760)MAXA,MINA
!1760 FORMAT(' ****ERROR IN PACK_GP.  VALUE WILL NOT PACK IN 30 BITS.',
!    1       '  MAXA ='I13,'  MINA ='I13,'.  ERROR AT 1760.')
      IER=706
      GO TO 900
!
 177  CONTINUE
      GO TO 200
!
!        *************************************
!
!        AT THIS POINT, GROUP B REQUIRES AS MANY BITS TO PACK AS GROUPA.
!        THEREFORE, TRY TO ADD INC POINTS TO GROUP A WITHOUT INCREASING
!        IBITA.  THIS AUGMENTED GROUP IS CALLED GROUP C.
!
!        *************************************
!
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
!
      NOUNT=0
      IF(NXY-(KTOTAL+KINC).LE.LMINPK/2)KINC=NXY-KTOTAL
!        ABOVE STATEMENT CONSTRAINS THE LAST GROUP TO BE NOT LESS THAN
!        LMINPK/2 IN SIZE.  IF A PROVISION LIKE THIS IS NOT INCLUDED,
!        THERE WILL MANY TIMES BE A VERY SMALL GROUP AT THE END.
!
!        USE SEPARATE LOOPS FOR MISSING AND NO MISSING VALUES
!        FOR EFFICIENCY.  SINCE KINC IS USUALLY 1, USING SEPARATE
!        LOOPS HERE DOESN'T BUY MUCH.  A MISSING VALUE WILL ALWAYS
!        TRANSFER BACK TO GROUP A.
!
      IF(IS523.EQ.0)THEN
!
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
!
      ELSEIF(IS523.EQ.1)THEN
!
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
!
      ELSE
!
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
!
      ENDIF
!
!***D     WRITE(KFILDO,191)KOUNTA,KTOTAL,MINA,MAXA,IBITA,MISLLA,
!***D    1   MINC,MAXC,NOUNT,IC(KTOTAL),IC(KTOTAL+1)
!***D191  FORMAT(' AT 191, KOUNTA ='I8,'  KTOTAL ='I8,'  MINA ='I8,
!***D    1       '  MAXA ='I8,'  IBITA ='I3,'  MISLLA ='I3,
!***D    2       '  MINC ='I8,'  MAXC ='I8,
!***D    3       '  NOUNT ='I5,'  IC(KTOTAL) ='I9,'  IC(KTOTAL+1) =',I9) 
!
!        IF THE NUMBER OF BITS NEEDED FOR GROUP C IS GT IBITA,
!        THEN THIS GROUP A IS A GROUP TO PACK.
!
      IF(MINC.EQ.MALLOW)THEN
         MINC=MINA
         MAXC=MAXA
         MINCK=MINAK
         MAXCK=MAXAK
         MISLLC=1
         GO TO 195
!           WHEN THE NEW VALUE(S) ARE MISSING, THEY CAN ALWAYS
!           BE ADDED.
!
      ELSE
         MISLLC=0
      ENDIF
!
      IF(MAXC-MINC.GE.IBXX2(IBITA)-LMISS) GO TO 200
!
!        THE BITS NECESSARY FOR GROUP C HAS NOT INCREASED FROM THE
!        BITS NECESSARY FOR GROUP A.  ADD THIS POINT(S) TO GROUP A.
!        COMPUTE THE NEXT GROUP B, ETC., UNLESS ALL POINTS HAVE BEEN
!        USED.
! 
 195  KTOTAL=KTOTAL+NOUNT
      KOUNTA=KOUNTA+NOUNT
      MINA=MINC
      MAXA=MAXC
      MINAK=MINCK
      MAXAK=MAXCK
      MISLLA=MISLLC
      ADDA=.TRUE.
      IF(KTOTAL.GE.NXY)GO TO 200
!
      IF(MINBK.GT.KTOTAL.AND.MAXBK.GT.KTOTAL)THEN
         MSTART=NENDB+1
!           THE MAX AND MIN OF GROUP B WERE NOT FROM THE POINTS
!           REMOVED, SO THE WHOLE GROUP DOES NOT HAVE TO BE LOOKED
!           AT TO DETERMINE THE NEW MAX AND MIN.  RATHER START
!           JUST BEYOND THE OLD NENDB.
         IBITBS=IBITB
         NENDB=1
         GO TO 150
      ELSE
         GO TO 140
      ENDIF
!
!        *************************************
!
!        GROUP A IS TO BE PACKED.  STORE VALUES IN JMIN( ), JMAX( ),
!        LBIT( ), AND NOV( ).
!
!        *************************************
!
 200  LX=LX+1
      IF(LX.LE.NDG)GO TO 205
      LMINPK=LMINPK+LMINPK/2
!     WRITE(KFILDO,201)NDG,LMINPK,LX
!201  FORMAT(' ****NDG ='I5,' NOT LARGE ENOUGH.',
!    1       '  LMINPK IS INCREASED TO 'I3,' FOR THIS FIELD.'/
!    2       '  LX = 'I10)
      IERSAV=716
      GO TO 105
!
 205  JMIN(LX)=MINA
      JMAX(LX)=MAXA
      LBIT(LX)=IBITA
      NOV(LX)=KOUNTA
      KSTART=KTOTAL+1
!
      IF(MISLLA.EQ.0)THEN
         MISSLX(LX)=MALLOW
      ELSE
         MISSLX(LX)=IC(KTOTAL)
!           IC(KTOTAL) WAS THE LAST VALUE PROCESSED.  IF MISLLA NE 0,
!           THIS MUST BE THE MISSING VALUE FOR THIS GROUP.
      ENDIF
!
!***D     WRITE(KFILDO,206)MISLLA,IC(KTOTAL),KTOTAL,LX,JMIN(LX),JMAX(LX),
!***D    1                 LBIT(LX),NOV(LX),MISSLX(LX)
!***D206  FORMAT(' AT 206,  MISLLA ='I2,'  IC(KTOTAL) ='I5,'  KTOTAL ='I8,
!***D    1       '  LX ='I6,'  JMIN(LX) ='I8,'  JMAX(LX) ='I8,
!***D    2       '  LBIT(LX) ='I5,'  NOV(LX) ='I8,'  MISSLX(LX) =',I7) 
!
      IF(KTOTAL.GE.NXY)GO TO 209
!
!        THE NEW GROUP A WILL BE THE PREVIOUS GROUP B.  SET LIMITS, ETC.
!
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
!
!        *************************************
!
!        CALCULATE IBIT, THE NUMBER OF BITS NEEDED TO HOLD THE GROUP
!        MINIMUM VALUES.
!
!        *************************************
!
 209  IBIT=0
!
      DO 220 L=1,LX
 210  IF(JMIN(L).LT.IBXX2(IBIT))GO TO 220
      IBIT=IBIT+1
      GO TO 210
 220  CONTINUE
!
!        INSERT THE VALUE IN JMIN( ) TO BE USED FOR ALL MISSING
!        VALUES WHEN LBIT( ) = 0.  WHEN SECONDARY MISSING 
!        VALUES CAN BE PRESENT, LBIT(L) WILL NOT = 0.
!
      IF(IS523.EQ.1)THEN
!
         DO 226 L=1,LX
!   
         IF(LBIT(L).EQ.0)THEN
!
            IF(MISSLX(L).EQ.MISSP)THEN
               JMIN(L)=IBXX2(IBIT)-1
            ENDIF
!
         ENDIF
!
 226     CONTINUE
!
      ENDIF
!
!        *************************************
!
!        CALCULATE JBIT, THE NUMBER OF BITS NEEDED TO HOLD THE BITS
!        NEEDED TO PACK THE VALUES IN THE GROUPS.  BUT FIND AND
!        REMOVE THE REFERENCE VALUE FIRST.
!
!        *************************************
!
!     WRITE(KFILDO,228)CFEED,LX
!228  FORMAT(A1,/' *****************************************'
!    1          /' THE GROUP WIDTHS LBIT( ) FOR ',I8,' GROUPS'
!    2          /' *****************************************')
!     WRITE(KFILDO,229) (LBIT(J),J=1,MIN(LX,100))
!229  FORMAT(/' '20I6)
!
      LBITREF=LBIT(1)
!
      DO 230 K=1,LX
      IF(LBIT(K).LT.LBITREF)LBITREF=LBIT(K)
 230  CONTINUE
!
      IF(LBITREF.NE.0)THEN
!
         DO 240 K=1,LX
         LBIT(K)=LBIT(K)-LBITREF
 240     CONTINUE
!
      ENDIF
!
!     WRITE(KFILDO,241)CFEED,LBITREF
!241  FORMAT(A1,/' *****************************************'
!    1          /' THE GROUP WIDTHS LBIT( ) AFTER REMOVING REFERENCE ',
!    2             I8,
!    3          /' *****************************************')
!     WRITE(KFILDO,242) (LBIT(J),J=1,MIN(LX,100))
!242  FORMAT(/' '20I6)
!
      JBIT=0
!
      DO 320 K=1,LX
 310  IF(LBIT(K).LT.IBXX2(JBIT))GO TO 320
      JBIT=JBIT+1
      GO TO 310
 320  CONTINUE
!
!        *************************************
!
!        CALCULATE KBIT, THE NUMBER OF BITS NEEDED TO HOLD THE NUMBER
!        OF VALUES IN THE GROUPS.  BUT FIND AND REMOVE THE
!        REFERENCE FIRST.
!
!        *************************************
!
!     WRITE(KFILDO,321)CFEED,LX
!321  FORMAT(A1,/' *****************************************'
!    1          /' THE GROUP SIZES NOV( ) FOR ',I8,' GROUPS'
!    2          /' *****************************************')
!     WRITE(KFILDO,322) (NOV(J),J=1,MIN(LX,100))
!322  FORMAT(/' '20I6)
!
      NOVREF=NOV(1)
!
      DO 400 K=1,LX
      IF(NOV(K).LT.NOVREF)NOVREF=NOV(K)
 400  CONTINUE
!
      IF(NOVREF.GT.0)THEN
!
         DO 405 K=1,LX
         NOV(K)=NOV(K)-NOVREF
 405     CONTINUE
!
      ENDIF
!
!     WRITE(KFILDO,406)CFEED,NOVREF
!406  FORMAT(A1,/' *****************************************'
!    1          /' THE GROUP SIZES NOV( ) AFTER REMOVING REFERENCE ',I8,
!    2          /' *****************************************')
!     WRITE(KFILDO,407) (NOV(J),J=1,MIN(LX,100))
!407  FORMAT(/' '20I6)
!     WRITE(KFILDO,408)CFEED
!408  FORMAT(A1,/' *****************************************'
!    1          /' THE GROUP REFERENCES JMIN( )'
!    2          /' *****************************************')
!     WRITE(KFILDO,409) (JMIN(J),J=1,MIN(LX,100))
!409  FORMAT(/' '20I6)
!
      KBIT=0
!
      DO 420 K=1,LX
 410  IF(NOV(K).LT.IBXX2(KBIT))GO TO 420
      KBIT=KBIT+1
      GO TO 410
 420  CONTINUE
!
!        DETERMINE WHETHER THE GROUP SIZES SHOULD BE REDUCED
!        FOR SPACE EFFICIENCY.
!
      IF(IRED.EQ.0)THEN
         CALL REDUCE(KFILDO,JMIN,JMAX,LBIT,NOV,LX,NDG,IBIT,JBIT,KBIT, &
              NOVREF,IBXX2,IER)
!
         IF(IER.EQ.714.OR.IER.EQ.715)THEN
!              REDUCE HAS ABORTED.  REEXECUTE PACK_GP WITHOUT REDUCE.
!              PROVIDE FOR A NON FATAL RETURN FROM REDUCE.  
            IERSAV=IER
            IRED=1
            IER=0
            GO TO 102 
         ENDIF
!
      ENDIF         
!
!     CALL TIMPR(KFILDO,KFILDO,'END   PACK_GP        ')
      IF(IERSAV.NE.0)THEN
         IER=IERSAV
         RETURN
      ENDIF
!
! 900  IF(IER.NE.0)RETURN1
!
 900  RETURN
      END
