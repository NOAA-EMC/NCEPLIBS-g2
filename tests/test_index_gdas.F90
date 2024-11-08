! This is a test program for NCEPLIBS-g2.
!
! This program tests index file functionality with getg2ir() and
! getg2i() with the test GDAS file.
!
! Ed Hartnett 5/17/23
program test_index_gdas
  use bacio_module
  use grib_mod
  use index_rec
  implicit none

  ! These are the test files we will use.
  character(*) :: TEST_FILE_GDAS
  parameter (TEST_FILE_GDAS = 'data/gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: TEST_FILE_GDAS_INDEX
  parameter (TEST_FILE_GDAS_INDEX = 'data/ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')

  integer :: LUGB, LUGI
  parameter(LUGB = 3, LUGI = 4)
  integer :: NUM_MESSAGES
  parameter(NUM_MESSAGES = 19)
  integer :: INDEX_REC_LEN
  parameter(INDEX_REC_LEN = 200)
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: BYTES_TO_SEARCH
  parameter(BYTES_TO_SEARCH = 1000)
  integer :: mnum
  integer :: nlen, nnum, nmess, iret
  integer :: nlen_expected
  type (index_rec_data) :: idx, expected_idx(NUM_MESSAGES)
  integer :: i, k, lpos
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  type(gribfield) :: gfld, expected_gfld(NUM_MESSAGES)
  integer :: IDSECTLEN
  parameter(IDSECTLEN = 13)

  ! All messages share the same ID section values.
  integer :: expected_idsect1(IDSECTLEN) = (/ 7, 0, 2, 1, 1, 2021, 11, 30, 0, 0, 0, 0, 1 /)

  ! All messages share the same Grid template values.
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, 151, 0, 0, 50000000, &
       210000000, 48, 25000000, 250000000, 166667, 166667, 0 /)

  ! Our test messages have different PDT values.
  integer :: expected_ipdtmpl_1(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_2(15) = (/ 2, 0, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_3(15) = (/ 2, 2, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_4(15) = (/ 2, 3, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_5(15) = (/ 0, 3, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_6(15) = (/ 0, 11, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_7(15) = (/ 0, 10, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_8(15) = (/ 0, 5, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_9(15) = (/ 0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_10(15) = (/ 0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0 /)
  integer :: expected_ipdtmpl_11(15) = (/ 0, 8, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0 /)
  integer :: expected_ipdtmpl_12(15) = (/ 0, 6, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_13(15) = (/ 0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_14(15) = (/ 0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0 /)
  integer :: expected_ipdtmpl_15(15) = (/ 0, 9, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0 /)
  integer :: expected_ipdtmpl_16(15) = (/ 0, 4, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_17(15) = (/ 0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 1, 255, 0, 0 /)
  integer :: expected_ipdtmpl_18(15) = (/ 0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 2, 255, 0, 0 /)
  integer :: expected_ipdtmpl_19(15) = (/ 0, 7, 2, 0, 11, 0, 0, 1, 0, 241, 0, 3, 255, 0, 0 /)

  ! Some messages have different DRT values.
  integer :: expected_idrtmpl_1(7) = (/ 1092616192, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_2(7) = (/ 1065353216, 0, 2, 16, 0, 0, 255 /)
  integer :: expected_idrtmpl_3(7) = (/ -1006403584, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_4(7) = (/ -996777984, 0, 2, 12, 0, 0, 255 /)
  integer :: expected_idrtmpl_5(7) = (/ 1102053376, 0, 2, 9, 0, 0, 255 /)
  integer :: expected_idrtmpl_6(7) = (/ 1144815616, 0, 2, 10, 0, 0, 255 /)
  integer :: expected_idrtmpl_7(7) = (/ 1185159680, 0, 2, 14, 0, 0, 255 /)
  integer :: expected_idrtmpl_8(7) = (/ 1086324736, 0, 2, 9, 0, 0, 255 /)
  integer :: expected_idrtmpl_9(7) = (/ 1092616192, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_10(7) = (/ 1095761920, 0, 2, 9, 0, 0, 255 /)
  integer :: expected_idrtmpl_11(7) = (/ 1086324736, 0, 2, 8, 0, 0, 255 /)
  integer :: expected_idrtmpl_12(7) = (/ 1084227584, 0, 2, 7, 0, 0, 255 /)
  integer :: expected_idrtmpl_13(7) = (/ 1125908480, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_14(7) = (/ 1136328704, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_15(7) = (/ 1135411200, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_16(7) = (/ 1133510656, 0, 2, 11, 0, 0, 255 /)
  integer :: expected_idrtmpl_17(7) = (/ 0, 0, 2, 16, 0, 0, 255 /)
  integer :: expected_idrtmpl_18(7) = (/ 1183603200, 0, 2, 14, 0, 0, 255 /)
  integer :: expected_idrtmpl_19(7) = (/ 1140424704, 0, 2, 16, 0, 0, 255 /)
  
  interface
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb, msk1, msk2, mnum
       character(len = 1),pointer,dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
  end interface

  interface
     subroutine getg2i(lugi,cbuf,nlen,nnum,iret)
       integer,intent(in) :: lugi
       character(len=1),pointer,dimension(:) :: cbuf
       integer,intent(out) :: nlen, nnum, iret
     end subroutine getg2i
  end interface

  interface
     subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(out) :: nlen, nnum, iret
       character(len = 1), pointer, dimension(:) :: cindex
     end subroutine getidx
  end interface

  interface
     subroutine getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
          jgdt, k, gfld, lpos, iret)
       use grib_mod       
       character(len = 1), intent(in) :: cbuf(*)
       integer, intent(in) :: nlen, nnum, j, jdisc, jpdtn, jgdtn
       integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
       integer, intent(out) :: k, lpos, iret
       type(gribfield), intent(out) :: gfld
     end subroutine getgb2s
  end interface

  print *, 'Testing index functions on GDAS test file and index.'

  ! Initialize expected results. These numbers can also be seen in a
  ! degrib2 of the file.
  call init_index(INDEX_REC_LEN, 0, 0, 37, 109, 143, 166, 4721, 15254, 2, 0, 1, expected_idx(1))
  call init_index(INDEX_REC_LEN, 15254, 0, 37, 109, 143, 166, 4721, 22643, 2, 0, 1, expected_idx(2))
  call init_index(INDEX_REC_LEN, 37897, 0, 37, 109, 143, 166, 4721, 15897, 2, 0, 1, expected_idx(3))
  call init_index(INDEX_REC_LEN, 53794, 0, 37, 109, 143, 166, 4721, 15270, 2, 0, 1, expected_idx(4))
  call init_index(INDEX_REC_LEN, 69064, 0, 37, 109, 143, 166, 4721, 10418, 2, 10, 1, expected_idx(5))
  call init_index(INDEX_REC_LEN, 79482, 0, 37, 109, 143, 166, 4721, 11826, 2, 10, 1, expected_idx(6))
  call init_index(INDEX_REC_LEN, 91308, 0, 37, 109, 143, 166, 4721, 17233, 2, 10, 1, expected_idx(7))
  call init_index(INDEX_REC_LEN, 108541, 0, 37, 109, 143, 166, 4721, 8175, 2, 10, 1, expected_idx(8))
  call init_index(INDEX_REC_LEN, 116716, 0, 37, 109, 143, 166, 4721, 12116, 2, 10, 1, expected_idx(9))
  call init_index(INDEX_REC_LEN, 128832, 0, 37, 109, 143, 166, 4721, 12016, 2, 10, 1, expected_idx(10))
  call init_index(INDEX_REC_LEN, 140848, 0, 37, 109, 143, 166, 4721, 10884, 2, 10, 1, expected_idx(11))
  call init_index(INDEX_REC_LEN, 151732, 0, 37, 109, 143, 166, 4721, 9289, 2, 10, 1, expected_idx(12))
  call init_index(INDEX_REC_LEN, 161021, 0, 37, 109, 143, 166, 4721, 12655, 2, 10, 1, expected_idx(13))
  call init_index(INDEX_REC_LEN, 173676, 0, 37, 109, 143, 166, 4721, 15749, 2, 10, 1, expected_idx(14))
  call init_index(INDEX_REC_LEN, 189425, 0, 37, 109, 143, 166, 4721, 15860, 2, 10, 1, expected_idx(15))
  call init_index(INDEX_REC_LEN, 205285, 0, 37, 109, 143, 166, 4721, 12978, 2, 10, 1, expected_idx(16))
  call init_index(INDEX_REC_LEN, 218263, 0, 37, 109, 143, 166, 4721, 18772, 2, 10, 1, expected_idx(17))
  call init_index(INDEX_REC_LEN, 237035, 0, 37, 109, 143, 166, 4721, 22188, 2, 10, 1, expected_idx(18))
  call init_index(INDEX_REC_LEN, 259223, 0, 37, 109, 143, 166, 4721, 22427, 2, 10, 1, expected_idx(19))

  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_1, 0, 11041, 40, 7, expected_idrtmpl_1, .false., .false., 0, expected_gfld(1))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_2, 0, 11041, 40, 7, expected_idrtmpl_2, .false., .false., 0, expected_gfld(2))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_3, 0, 11041, 40, 7, expected_idrtmpl_3, .false., .false., 0, expected_gfld(3))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_4, 0, 11041, 40, 7, expected_idrtmpl_4, .false., .false., 0, expected_gfld(4))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_5, 0, 11041, 40, 7, expected_idrtmpl_5, .false., .false., 0, expected_gfld(5))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_6, 0, 11041, 40, 7, expected_idrtmpl_6, .false., .false., 0, expected_gfld(6))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_7, 0, 11041, 40, 7, expected_idrtmpl_7, .false., .false., 0, expected_gfld(7))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_8, 0, 11041, 40, 7, expected_idrtmpl_8, .false., .false., 0, expected_gfld(8))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_9, 0, 11041, 40, 7, expected_idrtmpl_9, .false., .false., 0, expected_gfld(9))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_10, 0, 11041, 40, 7, expected_idrtmpl_10, .false., .false., 0, expected_gfld(10))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_11, 0, 11041, 40, 7, expected_idrtmpl_11, .false., .false., 0, expected_gfld(11))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_12, 0, 11041, 40, 7, expected_idrtmpl_12, .false., .false., 0, expected_gfld(12))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_13, 0, 11041, 40, 7, expected_idrtmpl_13, .false., .false., 0, expected_gfld(13))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_14, 0, 11041, 40, 7, expected_idrtmpl_14, .false., .false., 0, expected_gfld(14))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_15, 0, 11041, 40, 7, expected_idrtmpl_15, .false., .false., 0, expected_gfld(15))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_16, 0, 11041, 40, 7, expected_idrtmpl_16, .false., .false., 0, expected_gfld(16))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_17, 0, 11041, 40, 7, expected_idrtmpl_17, .false., .false., 0, expected_gfld(17))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_18, 0, 11041, 40, 7, expected_idrtmpl_18, .false., .false., 0, expected_gfld(18))
  call init_gribmod(2, IDSECTLEN, expected_idsect1, 0, 1, 0, 36391, 0, 0, 0, 0, 19, expected_igdtmpl, &
       0, 15, expected_ipdtmpl_19, 0, 11041, 40, 7, expected_idrtmpl_19, .false., .false., 0, expected_gfld(19))


  ! Initialize these for the getgb2s() call.
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
  nlen = 5000

  print *, '   testing getg2ir() to generate index records from GRIB2 file.'
  
  ! Open the GDAS GRIB2 file.
  call baopenr(LUGB, TEST_FILE_GDAS, iret)
  if (iret .ne. 0) stop 100

  ! Loop through the 19 messages in this test file, checking that the
  ! correct index information is generated by getg2ir() for each
  ! message.
  nlen_expected = INDEX_REC_LEN * NUM_MESSAGES
  do mnum = 1, NUM_MESSAGES
     call getg2ir(LUGB, BYTES_TO_SEARCH, BYTES_TO_SEARCH, mnum - 1, cbuf, nlen, nnum, nmess, iret)
     print *, 'mnum, iret, nlen, nnum, nmess: ', mnum, iret, nlen, nnum, nmess
     if (iret .ne. 0) stop 101
     if (nlen .ne. nlen_expected .or. nnum .ne. NUM_MESSAGES - (mnum - 1) .or. nmess .ne. NUM_MESSAGES) stop 102
     nlen_expected = nlen_expected - INDEX_REC_LEN

     ! Parse the index record into special type.
     call parse_cbuf(cbuf, idx)
     !call print_index(idx)

     ! Is this what we expected?
     if (cmp_idx(idx, expected_idx(mnum)) .ne. 0) stop 300

     ! Now test getgb2s(), which parses the index data in cbuf, and
     ! fills a gribfield type with the information.
     call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
          jgdt, k, gfld, lpos, iret)
     if (iret .ne. 0) stop 201
!     call print_gribmod(gfld)
     print *, '***************************************'
     if (cmp_gribmod(gfld, expected_gfld(mnum)) .ne. 0) stop 202
     print *, '***************************************'

     ! Free memory.
     call gf_free(gfld)
     deallocate(cbuf)
  end do

  ! Close the GRIB2 file.
  call baclose(LUGB, iret)
  if (iret .ne. 0) stop 199

  ! Free memory.
  do mnum = 1, NUM_MESSAGES  
     call gf_free(expected_gfld(mnum))
  end do

  
  print *, '   ok.'
  print *, '   testing getg2i() to read index records from an index file.'

  ! Open the index file, generated with the utility grb2index.
  call baopenr(LUGI, TEST_FILE_GDAS_INDEX, iret)
  if (iret .ne. 0) stop 3

  ! Check that this is an index file, and read it into buffer cbuf.
  call getg2i(LUGI, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 4
  if (nlen .ne. INDEX_REC_LEN * NUM_MESSAGES .or. nnum .ne. NUM_MESSAGES) stop 5

  ! Loop through each of the 19 index records, making sure they match
  ! the the values we expect.
  do mnum = 0, 18
     ! Parse the index record into special type.
     call parse_cbuf(cbuf(mnum * INDEX_REC_LEN + 1:), idx)
     !call print_index(idx)

     ! Is this what we expected?
     if (cmp_idx(idx, expected_idx(mnum + 1)) .ne. 0) stop 300
  end do

  ! Feee memory.
  deallocate(cbuf)

  ! Close the file.
  call baclose(LUGI, iret)
  if (iret .ne. 0) stop 50

  print *, '   ok.'

  ! Do this with and without the index file, and with the negative of
  ! the index file LU.
  do i = 1, 3
     print *, '   testing getidx() to read index records from either file or index file, i = ', i
     
     ! Open the GDAS GRIB2 file.
     call baopenr(LUGB, TEST_FILE_GDAS, iret)
     if (iret .ne. 0) stop 100

     if (i .gt. 1) then
        ! Open the GDAS GRIB2 INDEX file.
        call baopenr(LUGI, TEST_FILE_GDAS_INDEX, iret)
        if (iret .ne. 0) stop 100
     endif
     
     ! Get the index info, telling getidx() to generate it directly from
     ! the GRIB2 file for i=2.
     if (i .eq. 2) then
        call getidx(LUGB, LUGI, cbuf, nlen, nnum, iret)
     elseif (i .eq. 3) then
        call getidx(LUGB, LUGI * (-1), cbuf, nlen, nnum, iret)
     else
        call getidx(LUGB, 0, cbuf, nlen, nnum, iret)
     endif

     ! Loop through each of the 19 index records, making sure they match
     ! the the values we expect.
     do mnum = 0, 18
        ! Parse the index record into special type.
        call parse_cbuf(cbuf(mnum * INDEX_REC_LEN + 1:), idx)
        !call print_index(idx)

        ! Is this what we expected?
        if (cmp_idx(idx, expected_idx(mnum + 1)) .ne. 0) stop 300
     end do

     ! Close the data file.
     call baclose(LUGB, iret)
     if (iret .ne. 0) stop 50

     ! Close the index file.
     if (i .gt. 1) then
        call baclose(LUGI, iret)
        if (iret .ne. 0) stop 50
     endif
     
     print *, '   ok.'
  end do

  ! Feee memory.
  deallocate(cbuf)

  print *, 'SUCCESS!...'
end program test_index_gdas
