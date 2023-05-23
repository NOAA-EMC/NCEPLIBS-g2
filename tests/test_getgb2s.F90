! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2s.F90
!
! Ed Hartnett 7/28/22
program test_getgb2s
  use bacio_module
  use grib_mod
  implicit none

  integer :: lugi
  integer :: lugb = 3
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  integer :: i
  integer :: nlen, nnum
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nmess
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2022, 7, 18, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, 151, 0, 0, 50000000, &
       210000000, 48, 25000000, 250000000, 166667, 166667, 0 /)
  integer :: expected_ipdtmpl(15) = (/ 0, 5, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_idrtmpl(7) = (/ 1099431936, 0, 2, 9, 0, 0, 255 /)

  ! Interfaces are needed due to pointers in the parameter lists.
  interface
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb, msk1, msk2, mnum
       character(len = 1),pointer,dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
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

  print *, 'Testing the getgb2s() subroutine - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file WW3_Regional_US_West_Coast_20220718_0000.grib2...'
  call baopenr(lugb, "WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  ! Get the index information for the GRIB2 file.
  msk1 = 1000
  msk2 = 1000
  mnum = 0
  nullify(cbuf)
  call getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  if (iret .ne. 0) stop 101
  if (nlen .ne. 137600 .or. nnum .ne. 688 .or. nmess .ne. 688) stop 102
  print *, 'nlen, nnum, nmess: ', nlen, nnum, nmess

  ! Now learn about a field.
  lugi = 0
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
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
     jgdt, k, gfld, lpos, iret)
  if (iret .ne. 0) stop 101
  if (gfld%version .ne. 2 .or. gfld%discipline .ne. 10) stop 102
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

  ! Try again.
  j = 1
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
     jgdt, k, gfld, lpos, iret)
  if (iret .ne. 0) stop 110
  print *, gfld%ipdtmpl

  ! Free memory.
  call gf_free(gfld)

  ! Try again, but will fail because we are looking for a discipline
  ! that is not present in the file.
  j = 0
  jdisc = 12
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)
  if (iret .ne. 1) stop 111
  jdisc = -1

  ! Try again, but will fail because we are looking for an incorrect
  ! id section value.
  jids(1) = 42
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)
  if (iret .ne. 1) stop 112
  jids(1) = -9999

  ! Try again, but will fail because we are looking for an incorrect
  ! GDT section value.
  jgdt(1) = 42
  jgdtn = 6
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)
  if (iret .ne. 1) stop 113
  jgdt(1) = -9999
  jgdtn = -1

  ! Free memory.
  call gf_free(gfld)
  

  ! Try again, but will fail because we are looking for an incorrect
  ! PDT section value.
  jpdt(1) = 42
  jpdtn = 6
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)
  if (iret .ne. 1) stop 113
  jpdt(1) = -9999
  jpdtn = -1

  ! Free memory.
  call gf_free(gfld)

  ! Free memory.
  deallocate(cbuf)

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getgb2s
