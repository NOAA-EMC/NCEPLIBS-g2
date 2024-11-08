! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2p() on the GDAS test file.
!
! Ed Hartnett 5/15/23
program test_getgb2p_gdas
  use bacio_module
  implicit none

  integer :: lugi = 3
  integer :: lugb = 4 
  integer :: leng
  character(len=1), pointer, dimension(:) :: gribm
  integer :: j, jdisc, jpdtn, jgdtn
  integer :: jids(13), jpdt(100), jgdt(250)
  logical :: extract
  integer :: k
  integer :: i
  character(*) :: GDAS_FILE
  parameter(GDAS_FILE = 'data/gdaswave.t00z.wcoast.0p16.f000.grib2')
  character(*) :: GDAS_INDEX_FILE
  parameter(GDAS_INDEX_FILE = 'data/ref_gdaswave.t00z.wcoast.0p16.f000.grb2index')
  integer :: e, iret = 0
  
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

  ! Open a real GRIB2 file.
  print *, 'Testing getgb2p() with file ', GDAS_FILE
  call baopenr(lugb, GDAS_FILE, iret)
  if (iret .ne. 0) stop 100

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
  extract = .false.

  ! Try with extract both true and false. The results are the same for
  ! the GDAS file.
  do e = 1, 2
     if (e .eq. 2) extract = .true.
     print *, 'calling getgb2p() with extract = ', extract
     call getgb2p(lugb, 0, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          extract, k, gribm, leng, iret)
     print *, iret, k, leng
     if (iret .ne. 0) stop 101
     if (k .ne. 1 .or. leng .ne. 15254) stop 110

     ! Deallocate buffer that got GRIB message.
     deallocate(gribm)
  end do

  ! Now try with the index file. This causes a memory leak. See
  ! https://github.com/NOAA-EMC/NCEPLIBS-g2/issues/412.
  print *, 'Testing getgb2p() with index file ', GDAS_INDEX_FILE
  call baopenr(lugi, GDAS_INDEX_FILE, iret)
  if (iret .ne. 0) stop 400

  print *, 'Now try with extract false with index...'
  extract = .false.
  call getgb2p(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
        extract, k, gribm, leng, iret)
  if (iret .ne. 0) stop 201
  if (k .ne. 1 .or. leng .ne. 15254) stop 210

  print *, 'Deallocate buffer that got GRIB message.'
  deallocate(gribm)

  ! Close the index file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 499

  ! Close the GRIB2 file.
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 599

  print *, 'SUCCESS!...'

end program test_getgb2p_gdas
