! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2rp.F90
!
! Ed Hartnett 7/26/22
program test_getgb2rp
  use bacio_module
  use g2logging
  implicit none

  integer :: lugi
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lugb = 3
  integer :: nlen, nnum, iret
  logical :: extract
  integer :: leng
  character(len=1), pointer, dimension(:) :: gribm
  
  ! Interfaces are needed due to pointers in the parameter lists.
  interface
     subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(out) :: nlen, nnum, iret
       character(len = 1), pointer, dimension(:) :: cindex
     end subroutine getidx
  end interface

  interface
     subroutine getgb2rp(lugb, cindex, extract, gribm, leng, iret)
       integer, intent(in) :: lugb
       character(len=1), intent(in) :: cindex(*)
       logical, intent(in) :: extract
       character(len=1), pointer, dimension(:) :: gribm       
       integer, intent(out) :: leng, iret
     end subroutine getgb2rp
  end interface

  print *, 'Testing the getgb2rp() subroutine - expect and ignore error messages during test...'

  ! Open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file WW3_Regional_US_West_Coast_20220718_0000.grib2...'
  call baopenr(lugb, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  lugi = 0
  call getidx(lugb, lugi, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 101
  if (nlen .ne. 137600 .or. nnum .ne. 688) stop 102
  print *, 'nlen, nnum: ', nlen, nnum

  ! Extract the whole message.
  extract = .false.
  nullify(gribm)
  g2_log_level = 3
  call getgb2rp(lugb, cbuf, extract, gribm, leng, iret)
  g2_log_level = 0
  print *, 'leng ', leng
  if (leng .ne. 11183) stop 110
  ! Deallocate buffer that got GRIB message.
  deallocate(gribm)
  
  ! Extract just the field (same result).
  extract = .true.
  call getgb2rp(lugb, cbuf, extract, gribm, leng, iret)
  print *, 'leng ', leng
  if (leng .ne. 11183) stop 110
  ! Deallocate buffer that got GRIB message.
  deallocate(gribm)
  
  ! Deallocate the buffer that holds index.
  deallocate(cbuf)
  
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getgb2rp
