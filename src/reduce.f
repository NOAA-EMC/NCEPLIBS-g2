!> @file
!> @brief This subroutine determines groups of variable size.
!> @author Harry Glahn @date 2001-11-01

!> This subroutine determines whether the number of groups should be
!> increased in order to reduce the size of the large groups, and to
!> make that adjustment.
!>
!> By reducing the size of the large groups, less bits may be
!> necessary for packing the group sizes and all the information about
!> the groups.
!>
!> The reference for NOV was removed in the calling routine so that
!> kbit could be determined. This furnishes a starting point for the
!> iterations in reduce.
!>
!> @param[in] KFILDO unit number for output/print file.
!> @param[inout] JMIN the minimum of each group (j=1,lx). JMIN is
!> really the group reference and doesn't have to be the smallest
!> value.
!> @param[inout] JMAX the maximum of each group (j=1,lx).
!> @param[inout] LBIT the number of bits necessary to pack each group
!> (j=1,lx).
!> @param[inout] NOV the number of values in each group (j=1,lx).
!> @param[inout] LX the number of groups. This will be increased, if
!> groups are split.
!> @param[in] NDG the dimension of JMIN, JMAX, LBIT, and NOV.
!> @param[in] IBIT the number of bits necessary to pack the JMIN(j)
!> values, j=1,LX.
!> @param[in] JBIT the number of bits necessary to pack the LBIT(j)
!> values, j=1,LX.
!> @param[inout] KBIT the number of bits necessary to pack the NOV(j)
!> values, j=1,LX. If the groups are split, kbit is reduced.
!> @param[in] NOVREF reference value for NOV.
!> @param[in] IBXX2 ibxx2(j) = 2**j (j=0,30).
!> @param[out] IER error return.
!> - 0 good return.
!> - 714 error in reduce--non-fatal
!> - 715 ngp not large enough in reduce--non-fatal
!>
!> @author Harry Glahn @date 2001-11-01
      SUBROUTINE REDUCE(KFILDO,JMIN,JMAX,LBIT,NOV,LX,NDG,IBIT,JBIT,KBIT,
     1                  NOVREF,IBXX2,IER)            

      CHARACTER*1 CFEED
C
      DIMENSION JMIN(NDG),JMAX(NDG),LBIT(NDG),NOV(NDG)
      DIMENSION NEWBOX(NDG),NEWBOXP(NDG)
C        NEWBOX( ) AND NEWBOXP( ) ARE AUTOMATIC ARRAYS.
      DIMENSION NTOTBT(0:31),NBOXJ(0:31)
      DIMENSION IBXX2(0:30)
C
      DATA IFEED/12/
C
      IER=0
      IF(LX.EQ.1)GO TO 410
C        IF THERE IS ONLY ONE GROUP, RETURN.
C
      CFEED=CHAR(IFEED)
C
C        INITIALIZE NUMBER OF NEW BOXES PER GROUP TO ZERO.
C
      DO 110 L=1,LX
         NEWBOX(L)=0
 110  CONTINUE
C
C        INITIALIZE NUMBER OF TOTAL NEW BOXES PER J TO ZERO.
C
      DO 112 J=0,31
         NTOTBT(J)=999999999
         NBOXJ(J)=0
 112  CONTINUE
C
      IORIGB=(IBIT+JBIT+KBIT)*LX
C        IBIT = BITS TO PACK THE JMIN( ).
C        JBIT = BITS TO PACK THE LBIT( ).
C        KBIT = BITS TO PACK THE NOV( ).
C        LX = NUMBER OF GROUPS.
         NTOTBT(KBIT)=IORIGB
C           THIS IS THE VALUE OF TOTAL BITS FOR THE ORIGINAL LX
C           GROUPS, WHICH REQUIRES KBITS TO PACK THE GROUP
C           LENGHTS.  SETTING THIS HERE MAKES ONE LESS LOOPS
C           NECESSARY BELOW.
C
C        COMPUTE BITS NOW USED FOR THE PARAMETERS DEFINED.
C
C        DETERMINE OTHER POSSIBILITES BY INCREASING LX AND DECREASING
C        NOV( ) WITH VALUES GREATER THAN THRESHOLDS.  ASSUME A GROUP IS
C        SPLIT INTO 2 OR MORE GROUPS SO THAT KBIT IS REDUCED WITHOUT
C        CHANGING IBIT OR JBIT.
C
      JJ=0
C
      DO 200 J=MIN(30,KBIT-1),2,-1
C           VALUES GE KBIT WILL NOT REQUIRE SPLITS.  ONCE THE TOTAL
C           BITS START INCREASING WITH DECREASING J, STOP.  ALSO, THE
C           NUMBER OF BITS REQUIRED IS KNOWN FOR KBITS = NTOTBT(KBIT).
C
         NEWBOXT=0
C
         DO 190 L=1,LX
C
            IF(NOV(L).LT.IBXX2(J))THEN
               NEWBOX(L)=0
C                 NO SPLITS OR NEW BOXES.
               GO TO 190
            ELSE
               NOVL=NOV(L)
C
               M=(NOV(L)-1)/(IBXX2(J)-1)+1
C                 M IS FOUND BY SOLVING THE EQUATION BELOW FOR M:
C                 (NOV(L)+M-1)/M LT IBXX2(J)
C                 M GT (NOV(L)-1)/(IBXX2(J)-1)
C                 SET M = (NOV(L)-1)/(IBXX2(J)-1)+1
 130           NOVL=(NOV(L)+M-1)/M
C                 THE +M-1 IS NECESSARY.  FOR INSTANCE, 15 WILL FIT
C                 INTO A BOX 4 BITS WIDE, BUT WON'T DIVIDE INTO
C                 TWO BOXES 3 BITS WIDE EACH.
C      
               IF(NOVL.LT.IBXX2(J))THEN
                  GO TO 185
               ELSE
                  M=M+1
C***                  WRITE(KFILDO,135)L,NOV(L),NOVL,M,J,IBXX2(J)
C*** 135              FORMAT(/' AT 135--L,NOV(L),NOVL,M,J,IBXX2(J)',6I10)               
                  GO TO 130
               ENDIF
C
C                 THE ABOVE DO LOOP WILL NEVER COMPLETE.
            ENDIF
C
 185        NEWBOX(L)=M-1
            NEWBOXT=NEWBOXT+M-1
 190     CONTINUE
C
         NBOXJ(J)=NEWBOXT
         NTOTPR=NTOTBT(J+1)
         NTOTBT(J)=(IBIT+JBIT)*(LX+NEWBOXT)+J*(LX+NEWBOXT)
C
         IF(NTOTBT(J).GE.NTOTPR)THEN
            JJ=J+1
C              THE PLUS IS USED BECAUSE J DECREASES PER ITERATION.
            GO TO 250
         ELSE
C
C              SAVE THE TOTAL NEW BOXES AND NEWBOX( ) IN CASE THIS
C              IS THE J TO USE.
C
            NEWBOXTP=NEWBOXT
C
            DO 195 L=1,LX
               NEWBOXP(L)=NEWBOX(L)
 195        CONTINUE
C
C           WRITE(KFILDO,197)NEWBOXT,IBXX2(J)
C197        FORMAT(/' *****************************************'
C    1             /' THE NUMBER OF NEWBOXES PER GROUP OF THE TOTAL',
C    2              I10,' FOR GROUP MAXSIZE PLUS 1 ='I10
C    3             /' *****************************************')
C           WRITE(KFILDO,198) (NEWBOX(L),L=1,LX)
C198        FORMAT(/' '20I6/(' '20I6))
    
         ENDIF
C        
C205     WRITE(KFILDO,209)KBIT,IORIGB
C209     FORMAT(/' ORIGINAL BITS WITH KBIT OF',I5,' =',I10)
C        WRITE(KFILDO,210)(N,N=2,10),(IBXX2(N),N=2,10),
C    1                    (NTOTBT(N),N=2,10),(NBOXJ(N),N=2,10),
C    2                    (N,N=11,20),(IBXX2(N),N=11,20),
C    3                    (NTOTBT(N),N=11,20),(NBOXJ(N),N=11,20),
C    4                    (N,N=21,30),(IBXX2(N),N=11,20),
C    5                    (NTOTBT(N),N=21,30),(NBOXJ(N),N=21,30)
C210     FORMAT(/' THE TOTAL BYTES FOR MAXIMUM GROUP LENGTHS BY ROW'//
C    1      '   J         = THE NUMBER OF BITS PER GROUP LENGTH'/
C    2      '   IBXX2(J)  = THE MAXIMUM GROUP LENGTH PLUS 1 FOR THIS J'/
C    3      '   NTOTBT(J) = THE TOTAL BITS FOR THIS J'/
C    4      '   NBOXJ(J)  = THE NEW GROUPS FOR THIS J'/
C    5      4(/10X,9I10)/4(/10I10)/4(/10I10))
C
 200  CONTINUE
C
 250  PIMP=((IORIGB-NTOTBT(JJ))/FLOAT(IORIGB))*100.
C     WRITE(KFILDO,252)PIMP,KBIT,JJ
C252  FORMAT(/' PERCENT IMPROVEMENT =',F6.1,
C    1        ' BY DECREASING GROUP LENGTHS FROM',I4,' TO',I4,' BITS')
      IF(PIMP.GE.2.)THEN
C
C        WRITE(KFILDO,255)CFEED,NEWBOXTP,IBXX2(JJ)
C255     FORMAT(A1,/' *****************************************'
C    1             /' THE NUMBER OF NEWBOXES PER GROUP OF THE TOTAL',
C    2             I10,' FOR GROUP MAXSIZE PLUS 1 ='I10
C    2             /' *****************************************')
C        WRITE(KFILDO,256) (NEWBOXP(L),L=1,LX)
C256     FORMAT(/' '20I6)
C
C           ADJUST GROUP LENGTHS FOR MAXIMUM LENGTH OF JJ BITS.
C           THE MIN PER GROUP AND THE NUMBER OF BITS REQUIRED
C           PER GROUP ARE NOT CHANGED.  THIS MAY MEAN THAT A
C           GROUP HAS A MIN (OR REFERENCE) THAT IS NOT ZERO.
C           THIS SHOULD NOT MATTER TO THE UNPACKER.
C
         LXNKP=LX+NEWBOXTP
C           LXNKP = THE NEW NUMBER OF BOXES
C  
         IF(LXNKP.GT.NDG)THEN
C              DIMENSIONS NOT LARGE ENOUGH.  PROBABLY AN ERROR
C              OF SOME SORT.  ABORT.
C           WRITE(KFILDO,257)NDG,LXNPK
C        1         2         3         4         5         6         7 X
C257        FORMAT(/' DIMENSIONS OF JMIN, ETC. IN REDUCE =',I8,
C    1              ' NOT LARGE ENOUGH FOR THE EXPANDED NUMBER OF',
C    2              ' GROUPS =',I8,'.  ABORT REDUCE.')
            IER=715
            GO TO 410
C              AN ABORT CAUSES THE CALLING PROGRAM TO REEXECUTE 
C              WITHOUT CALLING REDUCE.
         ENDIF
C
         LXN=LXNKP
C           LXN IS THE NUMBER OF THE BOX IN THE NEW SERIES BEING
C           FILLED.  IT DECREASES PER ITERATION.
         IBXX2M1=IBXX2(JJ)-1
C           IBXX2M1 IS THE MAXIMUM NUMBER OF VALUES PER GROUP.
C
         DO 300 L=LX,1,-1
C
C              THE VALUES IS NOV( ) REPRESENT THOSE VALUES + NOVREF.
C              WHEN VALUES ARE MOVED TO ANOTHER BOX, EACH VALUE
C              MOVED TO A NEW BOX REPRESENTS THAT VALUE + NOVREF.
C              THIS HAS TO BE CONSIDERED IN MOVING VALUES.
C
            IF(NEWBOXP(L)*(IBXX2M1+NOVREF)+NOVREF.GT.NOV(L)+NOVREF)THEN
C                 IF THE ABOVE TEST IS MET, THEN MOVING IBXX2M1 VALUES
C                 FOR ALL NEW BOXES WILL LEAVE A NEGATIVE NUMBER FOR
C                 THE LAST BOX.  NOT A TOLERABLE SITUATION.
               MOVMIN=(NOV(L)-(NEWBOXP(L))*NOVREF)/NEWBOXP(L)
               LEFT=NOV(L)
C                 LEFT = THE NUMBER OF VALUES TO MOVE FROM THE ORIGINAL
C                 BOX TO EACH NEW BOX EXCEPT THE LAST.  LEFT IS THE
C                 NUMBER LEFT TO MOVE.
            ELSE
               MOVMIN=IBXX2M1
C                 MOVMIN VALUES CAN BE MOVED FOR EACH NEW BOX.
               LEFT=NOV(L)
C                 LEFT IS THE NUMBER OF VALUES LEFT TO MOVE.
            ENDIF
C
            IF(NEWBOXP(L).GT.0)THEN
               IF((MOVMIN+NOVREF)*NEWBOXP(L)+NOVREF.LE.NOV(L)+NOVREF.
     1          AND.(MOVMIN+NOVREF)*(NEWBOXP(L)+1).GE.NOV(L)+NOVREF)THEN
                  GO TO 288
               ELSE
C***D                 WRITE(KFILDO,287)L,MOVMIN,NOVREF,NEWBOXP(L),NOV(L)
C***D287              FORMAT(/' AT 287 IN REDUCE--L,MOVMIN,NOVREF,',
C***D    1                    'NEWBOXP(L),NOV(L)',5I12
C***D    2                    ' REDUCE ABORTED.')
C              WRITE(KFILDO,2870)
C2870          FORMAT(/' AN ERROR IN REDUCE ALGORITHM.  ABORT REDUCE.')
               IER=714
               GO TO 410
C                 AN ABORT CAUSES THE CALLING PROGRAM TO REEXECUTE 
C                 WITHOUT CALLING REDUCE.
               ENDIF
C
            ENDIF
C
 288        DO 290 J=1,NEWBOXP(L)+1
               MOVE=MIN(MOVMIN,LEFT)
               JMIN(LXN)=JMIN(L)
               JMAX(LXN)=JMAX(L)
               LBIT(LXN)=LBIT(L)
               NOV(LXN)=MOVE
               LXN=LXN-1
               LEFT=LEFT-(MOVE+NOVREF)
C                 THE MOVE OF MOVE VALUES REALLY REPRESENTS A MOVE OF
C                 MOVE + NOVREF VALUES.
 290        CONTINUE
C
            IF(LEFT.NE.-NOVREF)THEN
C***               WRITE(KFILDO,292)L,LXN,MOVE,LXNKP,IBXX2(JJ),LEFT,NOV(L),
C***     1                          MOVMIN
C*** 292           FORMAT(' AT 292 IN REDUCE--L,LXN,MOVE,LXNKP,',
C***     1                'IBXX2(JJ),LEFT,NOV(L),MOVMIN'/8I12)
            ENDIF
C     
 300     CONTINUE
C
         LX=LXNKP
C           LX IS NOW THE NEW NUMBER OF GROUPS.
         KBIT=JJ
C           KBIT IS NOW THE NEW NUMBER OF BITS REQUIRED FOR PACKING
C           GROUP LENGHTS.
      ENDIF
C
C     WRITE(KFILDO,406)CFEED,LX
C406  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP SIZES NOV( ) AFTER REDUCTION IN SIZE',
C    2           ' FOR'I10,' GROUPS',
C    3          /' *****************************************')
C     WRITE(KFILDO,407) (NOV(J),J=1,LX)
C407  FORMAT(/' '20I6)
C     WRITE(KFILDO,408)CFEED,LX
C408  FORMAT(A1,/' *****************************************'
C    1          /' THE GROUP MINIMA JMIN( ) AFTER REDUCTION IN SIZE',
C    2           ' FOR'I10,' GROUPS',
C    3          /' *****************************************')
C     WRITE(KFILDO,409) (JMIN(J),J=1,LX)
C409  FORMAT(/' '20I6)
C
 410  RETURN
      END
      
