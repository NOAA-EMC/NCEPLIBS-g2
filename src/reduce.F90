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
SUBROUTINE REDUCE(KFILDO,JMIN,JMAX,LBIT,NOV,LX,NDG,IBIT,JBIT,KBIT, &
     NOVREF,IBXX2,IER)

  CHARACTER*1 CFEED
  !
  DIMENSION JMIN(NDG),JMAX(NDG),LBIT(NDG),NOV(NDG)
  DIMENSION NEWBOX(NDG),NEWBOXP(NDG)
  !        NEWBOX( ) AND NEWBOXP( ) ARE AUTOMATIC ARRAYS.
  DIMENSION NTOTBT(0:31),NBOXJ(0:31)
  DIMENSION IBXX2(0:30)
  !
  DATA IFEED/12/
  !
  IER=0
  IF(LX.EQ.1)GO TO 410
  !        IF THERE IS ONLY ONE GROUP, RETURN.
  !
  CFEED=CHAR(IFEED)
  !
  !        INITIALIZE NUMBER OF NEW BOXES PER GROUP TO ZERO.
  !
  DO 110 L=1,LX
     NEWBOX(L)=0
110  CONTINUE
     !
     !        INITIALIZE NUMBER OF TOTAL NEW BOXES PER J TO ZERO.
     !
     DO 112 J=0,31
        NTOTBT(J)=999999999
        NBOXJ(J)=0
112     CONTINUE
        !
        IORIGB=(IBIT+JBIT+KBIT)*LX
        !        IBIT = BITS TO PACK THE JMIN( ).
        !        JBIT = BITS TO PACK THE LBIT( ).
        !        KBIT = BITS TO PACK THE NOV( ).
        !        LX = NUMBER OF GROUPS.
        NTOTBT(KBIT)=IORIGB
        !           THIS IS THE VALUE OF TOTAL BITS FOR THE ORIGINAL LX
        !           GROUPS, WHICH REQUIRES KBITS TO PACK THE GROUP
        !           LENGHTS.  SETTING THIS HERE MAKES ONE LESS LOOPS
        !           NECESSARY BELOW.
        !
        !        COMPUTE BITS NOW USED FOR THE PARAMETERS DEFINED.
        !
        !        DETERMINE OTHER POSSIBILITES BY INCREASING LX AND DECREASING
        !        NOV( ) WITH VALUES GREATER THAN THRESHOLDS.  ASSUME A GROUP IS
        !        SPLIT INTO 2 OR MORE GROUPS SO THAT KBIT IS REDUCED WITHOUT
        !        CHANGING IBIT OR JBIT.
        !
        JJ=0
        !
        DO 200 J=MIN(30,KBIT-1),2,-1
           !           VALUES GE KBIT WILL NOT REQUIRE SPLITS.  ONCE THE TOTAL
           !           BITS START INCREASING WITH DECREASING J, STOP.  ALSO, THE
           !           NUMBER OF BITS REQUIRED IS KNOWN FOR KBITS = NTOTBT(KBIT).
           !
           NEWBOXT=0
           !
           DO 190 L=1,LX
              !
              IF(NOV(L).LT.IBXX2(J))THEN
                 NEWBOX(L)=0
                 !                 NO SPLITS OR NEW BOXES.
                 GO TO 190
              ELSE
                 NOVL=NOV(L)
                 !
                 M=(NOV(L)-1)/(IBXX2(J)-1)+1
                 !                 M IS FOUND BY SOLVING THE EQUATION BELOW FOR M:
                 !                 (NOV(L)+M-1)/M LT IBXX2(J)
                 !                 M GT (NOV(L)-1)/(IBXX2(J)-1)
                 !                 SET M = (NOV(L)-1)/(IBXX2(J)-1)+1
130              NOVL=(NOV(L)+M-1)/M
                 !                 THE +M-1 IS NECESSARY.  FOR INSTANCE, 15 WILL FIT
                 !                 INTO A BOX 4 BITS WIDE, BUT WON'T DIVIDE INTO
                 !                 TWO BOXES 3 BITS WIDE EACH.
                 !
                 IF(NOVL.LT.IBXX2(J))THEN
                    GO TO 185
                 ELSE
                    M=M+1
                    !***                  WRITE(KFILDO,135)L,NOV(L),NOVL,M,J,IBXX2(J)
                    !*** 135              FORMAT(/' AT 135--L,NOV(L),NOVL,M,J,IBXX2(J)',6I10)
                    GO TO 130
                 ENDIF
                 !
                 !                 THE ABOVE DO LOOP WILL NEVER COMPLETE.
              ENDIF
              !
185           NEWBOX(L)=M-1
              NEWBOXT=NEWBOXT+M-1
190           CONTINUE
              !
              NBOXJ(J)=NEWBOXT
              NTOTPR=NTOTBT(J+1)
              NTOTBT(J)=(IBIT+JBIT)*(LX+NEWBOXT)+J*(LX+NEWBOXT)
              !
              IF(NTOTBT(J).GE.NTOTPR)THEN
                 JJ=J+1
                 !              THE PLUS IS USED BECAUSE J DECREASES PER ITERATION.
                 GO TO 250
              ELSE
                 !
                 !              SAVE THE TOTAL NEW BOXES AND NEWBOX( ) IN CASE THIS
                 !              IS THE J TO USE.
                 !
                 NEWBOXTP=NEWBOXT
                 !
                 DO 195 L=1,LX
                    NEWBOXP(L)=NEWBOX(L)
195                 CONTINUE
                    !
                    !           WRITE(KFILDO,197)NEWBOXT,IBXX2(J)
                    !197        FORMAT(/' *****************************************'
                    !    1             /' THE NUMBER OF NEWBOXES PER GROUP OF THE TOTAL',
                    !    2              I10,' FOR GROUP MAXSIZE PLUS 1 ='I10
                    !    3             /' *****************************************')
                    !           WRITE(KFILDO,198) (NEWBOX(L),L=1,LX)
                    !198        FORMAT(/' '20I6/(' '20I6))

                 ENDIF
                 !
                 !205     WRITE(KFILDO,209)KBIT,IORIGB
                 !209     FORMAT(/' ORIGINAL BITS WITH KBIT OF',I5,' =',I10)
                 !        WRITE(KFILDO,210)(N,N=2,10),(IBXX2(N),N=2,10),
                 !    1                    (NTOTBT(N),N=2,10),(NBOXJ(N),N=2,10),
                 !    2                    (N,N=11,20),(IBXX2(N),N=11,20),
                 !    3                    (NTOTBT(N),N=11,20),(NBOXJ(N),N=11,20),
                 !    4                    (N,N=21,30),(IBXX2(N),N=11,20),
                 !    5                    (NTOTBT(N),N=21,30),(NBOXJ(N),N=21,30)
                 !210     FORMAT(/' THE TOTAL BYTES FOR MAXIMUM GROUP LENGTHS BY ROW'//
                 !    1      '   J         = THE NUMBER OF BITS PER GROUP LENGTH'/
                 !    2      '   IBXX2(J)  = THE MAXIMUM GROUP LENGTH PLUS 1 FOR THIS J'/
                 !    3      '   NTOTBT(J) = THE TOTAL BITS FOR THIS J'/
                 !    4      '   NBOXJ(J)  = THE NEW GROUPS FOR THIS J'/
                 !    5      4(/10X,9I10)/4(/10I10)/4(/10I10))
                 !
200              CONTINUE
                 !
250              PIMP=((IORIGB-NTOTBT(JJ))/FLOAT(IORIGB))*100.
                 !     WRITE(KFILDO,252)PIMP,KBIT,JJ
                 !252  FORMAT(/' PERCENT IMPROVEMENT =',F6.1,
                 !    1        ' BY DECREASING GROUP LENGTHS FROM',I4,' TO',I4,' BITS')
                 IF(PIMP.GE.2.)THEN
                    !
                    !        WRITE(KFILDO,255)CFEED,NEWBOXTP,IBXX2(JJ)
                    !255     FORMAT(A1,/' *****************************************'
                    !    1             /' THE NUMBER OF NEWBOXES PER GROUP OF THE TOTAL',
                    !    2             I10,' FOR GROUP MAXSIZE PLUS 1 ='I10
                    !    2             /' *****************************************')
                    !        WRITE(KFILDO,256) (NEWBOXP(L),L=1,LX)
                    !256     FORMAT(/' '20I6)
                    !
                    !           ADJUST GROUP LENGTHS FOR MAXIMUM LENGTH OF JJ BITS.
                    !           THE MIN PER GROUP AND THE NUMBER OF BITS REQUIRED
                    !           PER GROUP ARE NOT CHANGED.  THIS MAY MEAN THAT A
                    !           GROUP HAS A MIN (OR REFERENCE) THAT IS NOT ZERO.
                    !           THIS SHOULD NOT MATTER TO THE UNPACKER.
                    !
                    LXNKP=LX+NEWBOXTP
                    !           LXNKP = THE NEW NUMBER OF BOXES
                    !
                    IF(LXNKP.GT.NDG)THEN
                       !              DIMENSIONS NOT LARGE ENOUGH.  PROBABLY AN ERROR
                       !              OF SOME SORT.  ABORT.
                       !           WRITE(KFILDO,257)NDG,LXNPK
                       !        1         2         3         4         5         6         7 X
                       !257        FORMAT(/' DIMENSIONS OF JMIN, ETC. IN REDUCE =',I8,
                       !    1              ' NOT LARGE ENOUGH FOR THE EXPANDED NUMBER OF',
                       !    2              ' GROUPS =',I8,'.  ABORT REDUCE.')
                       IER=715
                       GO TO 410
                       !              AN ABORT CAUSES THE CALLING PROGRAM TO REEXECUTE
                       !              WITHOUT CALLING REDUCE.
                    ENDIF
                    !
                    LXN=LXNKP
                    !           LXN IS THE NUMBER OF THE BOX IN THE NEW SERIES BEING
                    !           FILLED.  IT DECREASES PER ITERATION.
                    IBXX2M1=IBXX2(JJ)-1
                    !           IBXX2M1 IS THE MAXIMUM NUMBER OF VALUES PER GROUP.
                    !
                    DO 300 L=LX,1,-1
                       !
                       !              THE VALUES IS NOV( ) REPRESENT THOSE VALUES + NOVREF.
                       !              WHEN VALUES ARE MOVED TO ANOTHER BOX, EACH VALUE
                       !              MOVED TO A NEW BOX REPRESENTS THAT VALUE + NOVREF.
                       !              THIS HAS TO BE CONSIDERED IN MOVING VALUES.
                       !
                       IF(NEWBOXP(L)*(IBXX2M1+NOVREF)+NOVREF.GT.NOV(L)+NOVREF)THEN
                          !                 IF THE ABOVE TEST IS MET, THEN MOVING IBXX2M1 VALUES
                          !                 FOR ALL NEW BOXES WILL LEAVE A NEGATIVE NUMBER FOR
                          !                 THE LAST BOX.  NOT A TOLERABLE SITUATION.
                          MOVMIN=(NOV(L)-(NEWBOXP(L))*NOVREF)/NEWBOXP(L)
                          LEFT=NOV(L)
                          !                 LEFT = THE NUMBER OF VALUES TO MOVE FROM THE ORIGINAL
                          !                 BOX TO EACH NEW BOX EXCEPT THE LAST.  LEFT IS THE
                          !                 NUMBER LEFT TO MOVE.
                       ELSE
                          MOVMIN=IBXX2M1
                          !                 MOVMIN VALUES CAN BE MOVED FOR EACH NEW BOX.
                          LEFT=NOV(L)
                          !                 LEFT IS THE NUMBER OF VALUES LEFT TO MOVE.
                       ENDIF
                       !
                       IF(NEWBOXP(L).GT.0)THEN
                          IF((MOVMIN+NOVREF)*NEWBOXP(L)+NOVREF.LE.NOV(L)+NOVREF &
                               .AND.(MOVMIN+NOVREF)*(NEWBOXP(L)+1).GE.NOV(L)+NOVREF)THEN
                             GO TO 288
                          ELSE
                             !***D                 WRITE(KFILDO,287)L,MOVMIN,NOVREF,NEWBOXP(L),NOV(L)
                             !***D287              FORMAT(/' AT 287 IN REDUCE--L,MOVMIN,NOVREF,',
                             !***D    1                    'NEWBOXP(L),NOV(L)',5I12
                             !***D    2                    ' REDUCE ABORTED.')
                             !              WRITE(KFILDO,2870)
                             !2870          FORMAT(/' AN ERROR IN REDUCE ALGORITHM.  ABORT REDUCE.')
                             IER=714
                             GO TO 410
                             !                 AN ABORT CAUSES THE CALLING PROGRAM TO REEXECUTE
                             !                 WITHOUT CALLING REDUCE.
                          ENDIF
                          !
                       ENDIF
                       !
288                    DO 290 J=1,NEWBOXP(L)+1
                          MOVE=MIN(MOVMIN,LEFT)
                          JMIN(LXN)=JMIN(L)
                          JMAX(LXN)=JMAX(L)
                          LBIT(LXN)=LBIT(L)
                          NOV(LXN)=MOVE
                          LXN=LXN-1
                          LEFT=LEFT-(MOVE+NOVREF)
                          !                 THE MOVE OF MOVE VALUES REALLY REPRESENTS A MOVE OF
                          !                 MOVE + NOVREF VALUES.
290                       CONTINUE
                          !
                          IF(LEFT.NE.-NOVREF)THEN
                             !***               WRITE(KFILDO,292)L,LXN,MOVE,LXNKP,IBXX2(JJ),LEFT,NOV(L),
                             !***     1                          MOVMIN
                             !*** 292           FORMAT(' AT 292 IN REDUCE--L,LXN,MOVE,LXNKP,',
                             !***     1                'IBXX2(JJ),LEFT,NOV(L),MOVMIN'/8I12)
                          ENDIF
                          !
300                       CONTINUE
                          !
                          LX=LXNKP
                          !           LX IS NOW THE NEW NUMBER OF GROUPS.
                          KBIT=JJ
                          !           KBIT IS NOW THE NEW NUMBER OF BITS REQUIRED FOR PACKING
                          !           GROUP LENGHTS.
                       ENDIF
                       !
                       !     WRITE(KFILDO,406)CFEED,LX
                       !406  FORMAT(A1,/' *****************************************'
                       !    1          /' THE GROUP SIZES NOV( ) AFTER REDUCTION IN SIZE',
                       !    2           ' FOR'I10,' GROUPS',
                       !    3          /' *****************************************')
                       !     WRITE(KFILDO,407) (NOV(J),J=1,LX)
                       !407  FORMAT(/' '20I6)
                       !     WRITE(KFILDO,408)CFEED,LX
                       !408  FORMAT(A1,/' *****************************************'
                       !    1          /' THE GROUP MINIMA JMIN( ) AFTER REDUCTION IN SIZE',
                       !    2           ' FOR'I10,' GROUPS',
                       !    3          /' *****************************************')
                       !     WRITE(KFILDO,409) (JMIN(J),J=1,LX)
                       !409  FORMAT(/' '20I6)
                       !
410                    RETURN
                    END DO
