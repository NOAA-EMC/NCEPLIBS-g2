! This is a test program for NCEPLIBS-g2.
!
! This program tests index file functionality with g2_create_index().
!
! Ed Hartnett 2/15/24
program test_create_index
  use grib_mod
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'test_create_index_gdaswave.grb2index')
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver, myidxver, nlen, nnum, lugi = 31, lugb = 11
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: i
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2021, 11, 30, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, 151, 0, 0, 50000000, &
       210000000, 48, 25000000, 250000000, 166667, 166667, 0 /)

  ! These are the PDT templates of the 19 messages in the test file,
  ! verified with degrib2.
  integer :: expected_ipdtmpl(15, 19) = reshape((/ &
       2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       2, 0, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       2, 2, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       2, 3, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 3, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 11, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 10, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 5, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0, &
       0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0, &
       0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0, &
       0, 6, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0, &
       0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0, &
       0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0, &
       0, 4, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0, &
       0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0, &
       0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0, &
       0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0 /), &
       shape(expected_ipdtmpl))
  
  ! These are the DRT templates of the 19 messages in the test file,
  ! verified with degrib2.
  integer :: expected_idrtmpl(7, 19) = reshape((/ &
        1092616192, 0, 2, 11, 0, 0, 255, &
        1065353216, 0, 2, 16, 0, 0, 255, &
        -1006403584, 0, 2, 11, 0, 0, 255, &
        -996777984, 0, 2, 12, 0, 0, 255, &
        1102053376, 0, 2, 9, 0, 0, 255, &
        1144815616, 0, 2, 10, 0, 0, 255, &
        1185159680, 0, 2, 14, 0, 0, 255, &
        1086324736, 0, 2, 9, 0, 0, 255, &
        1095761920, 0, 2, 9, 0, 0, 255, &
        1086324736, 0, 2, 8, 0, 0, 255, &
        1084227584, 0, 2, 7, 0, 0, 255, &
        1125908480, 0, 2, 11, 0, 0, 255, &
        1136328704, 0, 2, 11, 0, 0, 255, &
        1135411200, 0, 2, 11, 0, 0, 255, &
        1133510656, 0, 2, 11, 0, 0, 255, &
        0, 0, 2, 16, 0, 0, 255, &
        1183603200, 0, 2, 14, 0, 0, 255, &
        1140424704, 0, 2, 16, 0, 0, 255, &
        1092616192, 0, 2, 16, 0, 0, 255 /), &
        shape(expected_idrtmpl))

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
     call baopenr(lugb, TEST_FILE_GDAS, ios)
     if (ios .ne. 0) stop 2

     ! Open output file where index will be written.
     call baopen(lugi, TEST_FILE_GDAS_INDEX, ios)
     if (ios .ne. 0) stop 3

     call g2_create_index(lugb, lugi, idxver, TEST_FILE_GDAS, iret)
     if (iret .ne. 0) stop 10

     call baclose(lugb, ios)
     if (ios .ne. 0) stop 11
     call baclose(lugi, ios)
     if (ios .ne. 0) stop 12

     print *, '   OK!'
     print *, '   testing that index file can be read with getg2i2()...'

     ! Open the index file.
     call baopen(lugi, TEST_FILE_GDAS_INDEX, iret)
     if (iret .ne. 0) stop 20

     ! Read the index file.
     call getg2i2(lugi, cbuf, myidxver, nlen, nnum, iret)
     !print *, myidxver, nlen, nnum, iret
     if (myidxver .ne. idxver) stop 79
     if (idxver .eq. 1) then
        print *, nlen
        if (nlen .ne. 3800) stop 80
     else
        print *, nlen
        if (nlen .ne. 4104) stop 81
     endif
     if (nnum .ne. 19 .or. iret .ne. 0) stop 82

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
     do j = 1, 19
        print *, '   testing unpacking index buffer with getgb2s2() for message', j
        call getgb2s2(cbuf, idxver, nlen, nnum, j - 1, jdisc, jids, jpdtn, jpdt, jgdtn, &
             jgdt, k, gfld, lpos, iret)
        if (iret .ne. 0) stop 101

        ! Check that the information is correct for the first record.
        if (gfld%version .ne. 2) stop 102
        if (j .lt. 5) then
           if (gfld%discipline .ne. 0) stop 103
        else
           if (gfld%discipline .ne. 10) stop 104
        end if
        if (gfld%idsectlen .ne. 13) stop 110
        if (gfld%ifldnum .ne. 1) stop 111
        if (gfld%griddef .ne. 0) stop 112
        if (gfld%ngrdpts .ne. 36391) stop 120
        if (gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. gfld%num_opt .ne. 0) stop 122
        if (gfld%igdtnum .ne. 0 .or. gfld%igdtlen .ne. 19) stop 123
        if (gfld%ipdtnum .ne. 0 .or. gfld%ipdtlen .ne. 15 .or. gfld%num_coord .ne. 0) stop 130
        if (gfld%unpacked .neqv. .FALSE.) stop 131
        if (gfld%ibmap .ne. 0) stop 132
        do i = 1, gfld%idsectlen
           if (gfld%idsect(i) .ne. expected_idsect(i)) stop 200
        end do
        do i = 1, gfld%igdtlen
           if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) stop 210
        end do
        do i = 1, gfld%ipdtlen
           if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i, j)) then
              print *, i, gfld%ipdtmpl(i), expected_ipdtmpl(i, j)
              print *, 'gfld%ipdtmpl', gfld%ipdtmpl
              print *, 'expected_ipdtmpl', expected_ipdtmpl
              stop 220
           endif
        end do
        do i = 1, gfld%idrtlen
           if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i, j)) then
              print *, i, gfld%idrtmpl(i), expected_idrtmpl(i, j)
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
end program test_create_index
