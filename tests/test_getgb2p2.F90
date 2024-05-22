! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2p2().
!
! Edward Hartnett 10/21/24
program test_getgb2p2
  use bacio_module
  !use g2logging
  implicit none

  integer :: lugi
  integer :: lugb = 3
  integer :: iret
  integer (kind = 8) :: leng8
  character(len=1), pointer, dimension(:) :: gribm
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  logical :: extract
  integer :: k
  integer :: i
  integer :: idxver, test_idx
  
  ! Interfaces are needed due to pointers in the parameter lists.
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

  print *, 'Testing the getgb2p2() subroutine...'

  ! Initialize search values to find the first message in the file.
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

  ! Test with index version 1 and 2.
  do test_idx = 1, 2
     ! Open a real GRIB2 file.
     print *, 'Indexing a real GRIB2 file WW3_Regional_US_West_Coast_20220718_0000.grib2...'
     call baopenr(lugb, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
     if (iret .ne. 0) stop 100

     !g2_log_level = 3
     extract = .true.
     idxver = test_idx
     print *, 'Try getgb2p2() with extract true, idxver:', idxver

     nullify(gribm)
     call getgb2p2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          extract, idxver, k, gribm, leng8, iret)
     if (iret .ne. 0) then
        print *, iret
        stop 101
     endif
     if (k .ne. 1 .or. leng8 .ne. 11183) then
        print *, k, leng8
        stop 110
     endif

     ! Deallocate buffer that got GRIB message.
     deallocate(gribm)
     print *, 'OK!'

     print *, 'Try getgb2p2() with extract false, idxver:', idxver
     extract = .false.
     nullify(gribm)
     call getgb2p2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          extract, idxver, k, gribm, leng8, iret)
     if (iret .ne. 0) stop 101
     if (k .ne. 1 .or. leng8 .ne. 11183) stop 110

     deallocate(gribm)
     print *, 'OK!'

     call baclose(lugb, iret)
     if (iret .ne. 0) stop 199
  end do
  print *, 'SUCCESS!...'

end program test_getgb2p2
