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
  integer :: idxver = 2, nlen, nnum, lugi = 31, lugb = 11
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: i
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2021, 11, 30, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, 151, 0, 0, 50000000, &
       210000000, 48, 25000000, 250000000, 166667, 166667, 0 /)
  integer :: expected_ipdtmpl(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_idrtmpl(7) = (/ 1092616192, 0, 2, 11, 0, 0, 255 /)
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
  print *, 'testing g2_create_index on ', TEST_FILE_GDAS

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

  print *, 'OK!'
  print *, 'testing that index file can be read with getg2i2()...'

  ! Open the index file.
  call baopen(lugi, TEST_FILE_GDAS_INDEX, iret)
  if (iret .ne. 0) stop 20

  ! Read the index file.
  call getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
  print *, nlen, nnum, iret
  if (idxver .eq. 1) then
     if (nlen .ne. 3800) stop 80
  else
     if (nlen .ne. 3876) stop 80
  endif
  if (nnum .ne. 19 .or. iret .ne. 0) stop 81

  ! Close the index file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 100

  print *, 'OK!'
  print *, 'testing that index buffer can be understood by getgb2s2()...'

  ! Parse the index info in cbuf, and fill gfld with the info about
  ! the first message.
  j = 0
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
  call getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)
  if (iret .ne. 0) stop 101

  ! Check that the information is correct for the first record.
  if (gfld%version .ne. 2 .or. gfld%discipline .ne. 0) stop 102
  if (gfld%idsectlen .ne. 13) stop 103
  if (gfld%ifldnum .ne. 1) stop 105
  if (gfld%griddef .ne. 0) stop 106
  if (gfld%ngrdpts .ne. 36391) stop 107
  if (gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. gfld%num_opt .ne. 0) stop 108
  if (gfld%igdtnum .ne. 0 .or. gfld%igdtlen .ne. 19) stop 109
  if (gfld%ipdtnum .ne. 0 .or. gfld%ipdtlen .ne. 15 .or. gfld%num_coord .ne. 0) stop 110
  if (gfld%unpacked .neqv. .FALSE.) stop 112
  if (gfld%ibmap .ne. 0) stop 113
  do i = 1, gfld%idsectlen
     if (gfld%idsect(i) .ne. expected_idsect(i)) stop 200
  end do
  do i = 1, gfld%igdtlen
     if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) stop 201
  end do
  do i = 1, gfld%ipdtlen
     if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) stop 202
  end do
  do i = 1, gfld%idrtlen
     if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) stop 203
  end do

  ! Free memory.
  call gf_free(gfld)

  print *, 'OK!'

  ! Clean up.
  deallocate(cbuf)
  call gf_finalize(iret)
  if (iret .ne. 0) stop 200
  
  print *, 'SUCCESS!...'
end program test_create_index
