! This test reads a large RRFS output file and runs the getgb2p subroutine for
! each message. It verifies that the subroutine is correctly indexing the
! lengths of each message section by verifying that the last four octets are
! '7777'. 
!
! Alex Richert, Edward Hartnett, May, 2024
PROGRAM test_getgb2p_2
  use g2logging
  use grib_mod
  use pdstemplates
  use gridtemplates
  implicit none

  integer, dimension(200) :: ids, gdt, pdt
  integer :: dscpl, gdtn, pdtn
  integer :: nrec
  integer, parameter :: jrew = 0
  character * 1 :: a7, b7, c7, d7

  character * 80 desc, wmohead
  character * 200 fileb, filei, fileo
  character(len = 1), pointer, dimension(:) :: gribm

  logical :: extract = .false.
  integer :: idxver = 2
  integer (kind = 8) :: itot
  integer :: ios, iret, iret1, j2, krew, lugb, lugi, lugo

  interface
     subroutine getgb2p2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          extract, idxver, k, gribm, leng8, iret)
       integer, intent(in) :: lugb, lugi, j, jdisc
       integer, dimension(:) :: jids(*)
       integer, intent(in) :: jpdtn
       integer, dimension(:) :: jpdt(*)
       integer, intent(in) :: jgdtn
       integer, dimension(:) :: jgdt(*)
       logical, intent(in) :: extract
       integer, intent(inout) :: idxver
       integer, intent(out) :: k
       character(len = 1), pointer, dimension(:) :: gribm
       integer (kind = 8), intent(out) :: leng8
       integer, intent(out) :: iret
     end subroutine getgb2p2
  end interface
  namelist /gribids/DSCPL, IDS, GDTN, GDT, PDTN, PDT, DESC, WMOHEAD, EXTRACT

  lugb = 11      ! Input GRIB2 File
  lugi = 0      ! Input GRIB2 INdex File
  lugo = 51      ! Output transmission file.

  ! Read GRIB2 data and index file names from the FORT_nn
  ! environment variables, and open the files.
  fileb = 'data/rrfs.t12z.prslevfaa.f010.na3km.grib2'
  filei = ''
  
  print *, 'Testing getgb2p2() on', fileb

  call baopenr(lugb, fileb, iret1)
  if (iret1  .ne. 0) then
     print *, 'error opening data file, iret1', iret1
     stop 10
  endif

  ! Read output GRIB bulletin file name from FORTnn
  ! environment variable, and open file.
  fileo = 'test_tocgrib2.output.grib2'
  call baopenw(lugo, fileo, iret1)
  if (iret1  .ne. 0) then
     print *, 'Error opening output transmission file, iret1', iret1
     stop 20
  endif

  ! Loop through input control records.
  iret = 0
  nrec = 0
  open(12, file = 'data/grib2.awips.rrfs.010')
  foreachinputrecord: do

     ! Set Namelist defaults.
     dscpl = -1     ! Grib2 Discipline number.
     ids = -9999    ! GRIB2 Identification Section.
     gdtn = -1      ! Grid Definition Template Number.
     gdt = -9999    ! Grid Definition Template.
     pdtn = -1      ! Product Definition Template Number.
     pdt = -9999    ! Product Definition Template.
     wmohead = 'TTAAnn CCCC'
     extract = .false.

     read (12, gribids, iostat = ios)
     if (ios .ne. 0) then
        if (nrec .eq. 337) then
           print *, 'All input records processed!'
           exit
        endif
        print *, 'Error reading PDS from input file. ios', ios, 'nrec', nrec
        stop 500
        cycle
     endif
     nrec = nrec + 1

     !  Echo input record
     write(6, '(A, I0)') ' Start new record no. =  ', nrec
     if (nrec .eq. 314) then
        g2_log_level = 0
     endif
     write(6, '(73A)') ' DESC = ', DESC(1:73)
     write(6, '(11A)') ' WMOHEAD = ', WMOHEAD(1:11)
     write(6, '(A, I0)') ' GRIB2 DISCIPLINE =  ', DSCPL
     write(6, '(A, 20(1x, I0))')' Section 1 = ', (IDS(j2), j2 = 1, 13)
     if (GDTN .ne. -1) then
        write(6, '(A, I0, A, 100(1x, I0))') ' GDT 3. ', GDTN, '  = ', (GDT(j2), j2 = 1, getgdtlen(GDTN))
     endif
     if (PDTN .ne. -1) then
        write(6, '(A, I0, A, 100(1x, I0))') ' PDT 4. ', PDTN, '  = ', (PDT(j2), j2 = 1, getpdtlen(PDTN))
     endif

     ! Read and return packed GRIB field.
     print *, '*** calling getgb2p2()'
     call getgb2p2(lugb, lugi, jrew, dscpl, ids, pdtn, pdt, gdtn, gdt, &
          extract, idxver, krew, gribm, itot, iret)
     print *, '*** iret', iret
     if (iret .ne. 0) then
        if (iret .eq. 96) then
           print *, ' test_getgb2p_2: error reading index file'
           stop 505
        endif
        if (iret .eq. 97) then
           print *, ' test_getgb2p_2: error reading grib file'
           stop 510
        endif
        if (iret .eq. 99) then
           print *, ' test_getgb2p_2: error request not found, nrec', nrec
           write(6, '(A, I0, A, 100(1x, I0))') ' PDT 4. ', PDTN, '  = ', (PDT(j2), j2 = 1, getpdtlen(PDTN))
           ! We expect this one won't be found.
           if (nrec .ne. 314) then
              stop 515
           endif
        endif
        cycle
     end if
     a7 = gribm(size(gribm) - 3)
     b7 = gribm(size(gribm) - 2)
     c7 = gribm(size(gribm) - 1)
     d7 = gribm(size(gribm))
     if (.not. all((/a7, b7, c7, d7/) .eq. '7')) then
        print *, 'error with section 8'
        stop 77
     endif
     deallocate(gribm)
  enddo foreachinputrecord

  print *, 'OK!'
  print *,  'SUCCESS!'

END PROGRAM test_getgb2p_2
