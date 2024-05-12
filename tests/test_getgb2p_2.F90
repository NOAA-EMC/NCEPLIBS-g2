! This test reads a large RRFS output file and runs the getgb2p subroutine for
! each message. It verifies that the subroutine is correctly indexing the
! lengths of each message section by verifying that the last four octets are
! '7777'. 
!
! Alex Richert, May 2024
PROGRAM test_getgb2p_2
  use grib_mod
  use pdstemplates
  use gridtemplates
  integer,dimension(200) :: IDS,GDT,PDT
  integer   ::    DSCPL,GDTN,PDTN
  integer   ::    nrec
  integer,parameter :: jrew=0
  character * 1 :: a7,b7,c7,d7

  CHARACTER * 80  DESC,WMOHEAD
  CHARACTER * 200  fileb,filei,fileo
  character(len=1),pointer,dimension(:) :: gribm

  logical :: extract=.false.

  interface
     SUBROUTINE GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
          EXTRACT,K,GRIBM,LENG,IRET)
       INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
       INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
       LOGICAL,INTENT(IN) :: EXTRACT
       INTEGER,INTENT(OUT) :: K,IRET
       CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
     END SUBROUTINE GETGB2P
  end interface
  NAMELIST /GRIBIDS/DSCPL,IDS,GDTN,GDT,PDTN,PDT,DESC,WMOHEAD,EXTRACT

  lugb=11      ! Input GRIB2 File
  lugi=0      ! Input GRIB2 INdex File
  lugo=51      ! Output transmission file.

  !        Read GRIB2 data and index file names from the FORT_nn
  !        environment variables, and open the files.
  fileb='data/rrfs.t12z.prslevfaa.f010.na3km.grib2'
  filei=''

  call baopenr(lugb,fileb,iret1)
  if (iret1  .ne. 0) then
     write(6,fmt='(" Error opening GRIB file: ",A200)') fileb
     write(6,fmt='(" baopenr error = ",I5)') iret1
     stop 10
  endif

  !        Read output GRIB bulletin file name from  FORTnn
  !        environment variable, and open file.
  fileo='test_tocgrib2.output.grib2'
  call baopenw(lugo,fileo,iret1)
  if (iret1  .ne. 0) then
     write(6,fmt='(" Error opening output transmission file: ", &
          A200)') fileo
     write(6,fmt='(" baopenw error = ",I5)') iret1
     stop 20
  endif

  !        loop through input control records.
  iret=0
  nrec = 0
  open(12, file='data/grib2.awips.rrfs.010')
  foreachinputrecord: do

     !  Set Namelist defaults
     DSCPL=-1     ! Grib2 Discipline number
     IDS=-9999    ! GRIB2 Identification Section
     GDTN=-1      ! Grid Definition Template Number
     GDT=-9999    ! Grid Definition Template
     PDTN=-1      ! Product Definition Template Number
     PDT=-9999    ! Product Definition Template
     WMOHEAD='TTAAnn CCCC'
     EXTRACT=.false.

     READ (12,GRIBIDS,iostat=ios)
     if (ios .ne. 0) then
        write(6,fmt='(" Error reading PDS from input file. iostat = " &
             ,i5)') ios
        cycle
     endif
     nrec = nrec + 1

     !  Echo input record
     WRITE(6,FMT='(/,''***********************************'', &
          ''********************************************'')')
     write(6,'(A,I0)') ' Start new record no. =  ',nrec
     write(6,'(73A)') ' DESC=',DESC(1:73)
     write(6,'(11A)') ' WMOHEAD=',WMOHEAD(1:11)
     write(6,'(A,I0)') ' GRIB2 DISCIPLINE= ',DSCPL
     write(6,'(A,20(1x,I0))')' Section 1=', &
          (IDS(j2),j2=1,13)
     if (GDTN .ne. -1) then
        write(6,'(A,I0,A,100(1x,I0))') ' GDT 3. ',GDTN,' =', &
             (GDT(j2),j2=1,getgdtlen(GDTN))
     endif
     if (PDTN .ne. -1) then
        write(6,'(A,I0,A,100(1x,I0))') ' PDT 4. ',PDTN,' =', &
             (PDT(j2),j2=1,getpdtlen(PDTN))
     endif

     !        Read and return packed GRIB field
     CALL GETGB2P(lugb,lugi,jrew,DSCPL,IDS,PDTN,PDT, &
          GDTN,GDT,extract,KREW,gribm,itot,iret)
     IF (IRET.NE.0) THEN
        IF (IRET.EQ.96)WRITE(6,'(A)')' GETGB2P: ERROR READING INDEX' &
             //' FILE'
        IF (IRET.EQ.97)WRITE(6,'(A)')' GETGB2P: ERROR READING GRIB' &
             //' FILE'
        IF (IRET.EQ.99)WRITE(6,'(A)')' GETGB2P: ERROR REQUEST NOT' &
             //' FOUND'
        cycle
     END IF
     a7=gribm(size(gribm)-3)
     b7=gribm(size(gribm)-2)
     c7=gribm(size(gribm)-1)
     d7=gribm(size(gribm))
     if(.not.all((/a7,b7,c7,d7/).eq.'7')) stop 77
     deallocate(gribm)
    return
  enddo foreachinputrecord

END PROGRAM test_getgb2p_2
