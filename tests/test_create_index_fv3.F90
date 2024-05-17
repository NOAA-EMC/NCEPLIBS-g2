! This is a test program for NCEPLIBS-g2.
!
! This program tests index file functionality with g2_create_index() with a file > 2 GB.
!
! Ed Hartnett 2/16/24
program test_create_index_fv3
  use grib_mod
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_FV3
  parameter (TEST_FILE_FV3 = 'fv3lam.t00z.prslev.f000.grib2')
  character(*) :: TEST_FILE_FV3_INDEX
  parameter (TEST_FILE_FV3_INDEX = 'test_create_index_fv3.grb2index')
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: idxver, myidxver, nlen, nnum, lugi = 31, lugb = 11
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: i
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2022, 6, 21, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(22) = (/ 6, 0, 0, 0, 0, 0, 0, 4881, 2961, 0, 0, &
       -37000000, 299000000, 48, 37000000, 61000000, 25000, 25000, 64, -35000000, 247000000, 0 /)

  ! These are the PDT templates of the 19 messages in the test file,
  ! verified with degrib2.
  integer :: expected_ipdtmpl(15) = (/ 3, 0, 2, 0, 134, 0, 0, 1, 0, 1, 0, 0, 255, 0, 0 /)
  
  ! These are the DRT templates of the 19 messages in the test file,
  ! verified with degrib2.
  integer :: expected_idrtmpl(18) = (/ 1216637952, 0, 3, 17, 0, 1, 0, 0, 0, &
       560759, 0, 5, 1, 1, 63, 6, 2, 3 /)

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
  idxver = 2
  print *, 'testing g2_create_index version ', idxver

  ! Open GRIB2 file for reading.
  call baopenr(lugb, 'data/' // TEST_FILE_FV3, ios)
  if (ios .ne. 0) stop 2

  ! Open output file where index will be written.
  call baopen(lugi, TEST_FILE_FV3_INDEX, ios)
  if (ios .ne. 0) stop 3

  call g2_create_index(lugb, lugi, idxver, TEST_FILE_FV3, iret)
  if (iret .ne. 0) stop 10

  call baclose(lugb, ios)
  if (ios .ne. 0) stop 11
  call baclose(lugi, ios)
  if (ios .ne. 0) stop 12

  print *, 'OK!'
  print *, 'testing that index file can be read with getg2i2()...'

  ! Open the index file.
  call baopen(lugi, TEST_FILE_FV3_INDEX, iret)
  if (iret .ne. 0) stop 20

  ! Read the index file.
  call getg2i2(lugi, cbuf, myidxver, nlen, nnum, iret)
  print *, myidxver, nlen, nnum, iret
  if (nlen .ne. 272694) then
     print *, nlen
     stop 80
  endif
  if (nnum .ne. 1081 .or. iret .ne. 0) stop 81

  ! Close the index file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 100

  print *, 'OK!'

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
  print *, 'testing unpacking index buffer with getgb2s2() for all messages...'
  do j = 1, nnum
     call getgb2s2(cbuf, idxver, nlen, nnum, j - 1, jdisc, jids, jpdtn, jpdt, jgdtn, &
          jgdt, k, gfld, lpos, iret)
     if (iret .ne. 0) stop 101

     ! Check that the information is correct for many records.
     if (gfld%version .ne. 2) stop 102
     if (j .lt. 47) then
        if (gfld%discipline .ne. 0) stop 103
     elseif (j .eq. 47) then
        if (gfld%discipline .ne. 2) stop 104
     elseif (j .lt. 756) then
        if (gfld%discipline .ne. 0) stop 104
     elseif (j .eq. 756) then
        if (gfld%discipline .ne. 2) stop 104
     elseif (j .lt. 818) then
        if (gfld%discipline .ne. 0) stop 104
     elseif (j .lt. 834) then
        if (gfld%discipline .ne. 2) stop 104
     elseif (j .lt. 837) then
        if (gfld%discipline .ne. 0) stop 104
     elseif (j .lt. 839) then
        if (gfld%discipline .ne. 2) stop 104
     elseif (j .lt. 854) then
        if (gfld%discipline .ne. 0) stop 104
     elseif (j .eq. 854) then
        if (gfld%discipline .ne. 1) stop 104
     elseif (j .lt. 861) then
        if (gfld%discipline .ne. 0) stop 104
     elseif (j .lt. 862) then
        if (gfld%discipline .ne. 2) stop 104
     end if

     ! Check the values of the last message.
     if (j .eq. nnum) then
        print *, '   Checking values of last message index...'
           if (gfld%idsectlen .ne. 13) stop 110
           if (gfld%ifldnum .ne. 1) stop 111
           if (gfld%griddef .ne. 0) stop 112
           if (gfld%ngrdpts .ne. 14452641) stop 120
           if (gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. gfld%num_opt .ne. 0) stop 122
           if (gfld%igdtnum .ne. 1 .or. gfld%igdtlen .ne. 22) stop 123
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
              if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) stop 220
           end do
           do i = 1, gfld%idrtlen
              if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) stop 230
           end do
        print *, '   OK!'
     endif
        
     ! Free memory.
     call gf_free(gfld)
  end do

  ! Clean up.
  deallocate(cbuf)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 200

  print *, 'SUCCESS!...'
end program test_create_index_fv3
