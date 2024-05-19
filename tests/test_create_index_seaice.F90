! This is a test program for NCEPLIBS-g2.
!
! This program tests index file functionality with g2_create_index()
! for one of the files optionally downloaded via FTP during testing:
! seaice.t00z.grb.grib2
!
! Ed Hartnett 2/23/24
program test_create_index_seaice
  use grib_mod
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_SEAICE
  parameter (TEST_FILE_SEAICE = 'seaice.t00z.grb.grib2')
  character(*) :: TEST_FILE_SEAICE_INDEX
  parameter (TEST_FILE_SEAICE_INDEX = 'test_create_index_seaice.grb2index')
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver, myidxver, nlen, nnum, lugi = 31, lugb = 11
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: i
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2022, 11, 17, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 720, 360, 0, 0, &
       89750000, 250000, 48, -89750000, 359750000, 500000, 500000, 0 /)

  integer :: expected_ipdtmpl(15) = (/ 2, 0, 2, 0, 120, 0, 0, 1, 0, 101, 0, 0, 255, 0, 0/)
  
  ! These are the DRT templates of the 19 messages in the test file,
  ! verified with degrib2.
  integer :: expected_idrtmpl(7) = (/ 0, 0, 2, 8, 0, 0, 255 /)

  integer :: ios

  interface
     subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
       integer, intent(in) :: lugi
       character(len=1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: idxver, nlen, nnum, iret
     end subroutine getg2i2
     subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
       integer, intent(in) :: lugb, lugi, idxver
       character*(*) :: filename
       integer, intent(out) :: iret
     end subroutine g2_create_index
  end interface

  print *, 'Testing index creation and reading.'
  do idxver = 1, 2
     print *, 'testing g2_create_index version ', idxver

     ! Open GRIB2 file for reading.
     call baopenr(lugb, 'data/' // TEST_FILE_SEAICE, ios)
     if (ios .ne. 0) stop 2

     ! Open output file where index will be written.
     call baopen(lugi, TEST_FILE_SEAICE_INDEX, ios)
     if (ios .ne. 0) stop 3

     call g2_create_index(lugb, lugi, idxver, TEST_FILE_SEAICE, iret)
     if (iret .ne. 0) stop 10

     call baclose(lugb, ios)
     if (ios .ne. 0) stop 11
     call baclose(lugi, ios)
     if (ios .ne. 0) stop 12

     print *, '   OK!'
     print *, '   testing that index file can be read with getg2i2()...'

     ! Open the index file.
     call baopen(lugi, TEST_FILE_SEAICE_INDEX, iret)
     if (iret .ne. 0) stop 20

     ! Read the index file.
     call getg2i2(lugi, cbuf, myidxver, nlen, nnum, iret)
     !print *, myidxver, nlen, nnum, iret
     if (myidxver .ne. idxver) stop 79
     if (idxver .eq. 1) then
        if (nlen .ne. 200) stop 80
     else
        if (nlen .ne. 220) then
           print *, nlen
           stop 81
        endif
     endif
     if (nnum .ne. 1 .or. iret .ne. 0) stop 83

     ! Close the index file.
     call baclose(lugi, iret)
     if (iret .ne. 0) stop 100

     print *, '   OK!'

     ! Parse the index info in cbuf, and fill gfld with the info about
     ! the first message.
     jdisc = -1
     do i = 1, 13
        jids(i) = -9999
     end do
     jpdtn = -1
     do i = 1, 100
        jpdt(i) = -9999
     end do
     jgdtn = -1
     do i = 1, 250
        jgdt(i) = -9999
     end do
     do j = 1, nnum
        print *, '   testing unpacking index buffer with getgb2s2() for message', j
        call getgb2s2(cbuf, idxver, nlen, nnum, j - 1, jdisc, jids, jpdtn, jpdt, jgdtn, &
             jgdt, k, gfld, lpos, iret)
        if (iret .ne. 0) stop 101

        ! print *, '****************'
        ! print *, gfld%version, gfld%discipline, gfld%idsectlen, gfld%ifldnum, gfld%griddef
        ! print *, gfld%ngrdpts, gfld%numoct_opt, gfld%interp_opt, gfld%num_opt
        ! print *, gfld%igdtnum, gfld%igdtlen
        ! print *, gfld%ipdtnum, gfld%ipdtlen, gfld%num_coord
        ! print *, gfld%unpacked, gfld%ibmap
        ! print *, 'sec1:', gfld%idsect
        ! print *, 'gdt:', gfld%igdtmpl

        ! Check that the information is correct for the first record.
        if (gfld%version .ne. 2) stop 102
        if (gfld%discipline .ne. 10) stop 103
        if (gfld%idsectlen .ne. 13) stop 110
        if (gfld%ifldnum .ne. 1) stop 111
        if (gfld%griddef .ne. 0) stop 112
        if (gfld%ngrdpts .ne. 259200) stop 120
        if (gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. gfld%num_opt .ne. 0) stop 122
        if (gfld%igdtnum .ne. 0 .or. gfld%igdtlen .ne. 19) stop 123
        if (gfld%ipdtnum .ne. 0 .or. gfld%ipdtlen .ne. 15 .or. gfld%num_coord .ne. 0) stop 130
        if (gfld%unpacked .neqv. .FALSE.) stop 131
        if (gfld%ibmap .ne. 255) stop 132
        do i = 1, gfld%idsectlen
           if (gfld%idsect(i) .ne. expected_idsect(i)) then
              print *, gfld%idsect
              stop 200
           endif
        end do
        do i = 1, gfld%igdtlen
           if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) then
              print *, gfld%igdtmpl
              stop 210
           endif
        end do
        do i = 1, gfld%ipdtlen
           if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) then
              print *, i, gfld%ipdtmpl(i), expected_ipdtmpl(i)
              print *, 'gfld%ipdtmpl', gfld%ipdtmpl
              print *, 'expected_ipdtmpl', expected_ipdtmpl
              stop 220
           endif
        end do
        do i = 1, gfld%idrtlen
           if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) then
              print *, i, gfld%idrtmpl(i), expected_idrtmpl(i)
              print *, 'gfld%idrtmpl', gfld%idrtmpl
              print *, 'expected_idrtmpl', expected_idrtmpl
              stop 230
           endif
        end do

        ! Free memory.
        call gf_free(gfld)
        print *, '   OK!'
     end do

     ! Clean up.
     deallocate(cbuf)
     call gf_finalize(iret)
     if (iret .ne. 0) stop 200
     print *, 'OK!'
  end do
  print *, 'SUCCESS!...'
end program test_create_index_seaice
