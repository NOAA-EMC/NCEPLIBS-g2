! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2p.F90 some more.
!
! Ed Hartnett 5/15/23
program test_getgb2p_2
  use bacio_module
  implicit none

  integer :: lugi
#if KIND == 4
  integer :: lugb = 3 ! Use different LU for _4/_d in case tests are run in parallel.
#else
  integer :: lugb = 5
#endif
  integer :: iret
  integer :: leng
  character(len=1), pointer, dimension(:) :: gribm
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  logical :: extract
  integer :: k
  integer :: i
  character(*) :: GDAS_FILE
  parameter(GDAS_FILE = 'gdaswave.t00z.wcoast.0p16.f000.grib2')
  
  ! Interfaces are needed due to pointers in the parameter lists.
  interface
     subroutine getgb2p(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          extract, k, gribm, leng, iret)
       integer, intent(in) :: lugb, lugi, j, jdisc, jpdtn, jgdtn
       integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
       logical, intent(in) :: extract
       integer, intent(out) :: k
       character(len = 1), pointer, dimension(:) :: gribm       
       integer, intent(out) :: iret, leng
     end subroutine getgb2p
  end interface

  print *, 'Testing the getgb2p() subroutine - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  print *, 'Testing getgb2p() some more...'
  call baopenr(lugb, GDAS_FILE, iret)
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

  print *, 'First try with extract true...'
  extract = .true.
  call getgb2p(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       extract, k, gribm, leng, iret)
  print *, iret, k, leng
  if (iret .ne. 0) stop 101
  if (k .ne. 1 .or. leng .ne. 15254) stop 110

  ! Deallocate buffer that got GRIB message.
  deallocate(gribm)

  ! print *, 'Now try with extract false...'
  extract = .false.
  call getgb2p(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       extract, k, gribm, leng, iret)
  if (iret .ne. 0) stop 101
  if (k .ne. 1 .or. leng .ne. 15254) stop 110

  print *, 'Deallocate buffer that got GRIB message.'
  deallocate(gribm)
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getgb2p_2
