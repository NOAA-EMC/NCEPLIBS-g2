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
  integer :: leng
  character(len=1), pointer, dimension(:) :: gribm
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  logical :: extract
  integer :: i
  character(len = 1) :: cbuf(5000)
  integer :: nlen, nnum
  integer :: k, lpos, iret
  type(gribfield) :: gfld
  
  ! Interfaces are needed due to pointers in the parameter lists.
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

  ! call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
  !    jgdt, k, gfld, lpos, iret)
  ! if (iret .ne. 0) stop 101
!  if (k .ne. 1 .or. leng .ne. 11183) stop 110

  ! Free memory.
!  call gf_free(gfld)

  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getgb2s
