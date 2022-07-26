! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2ir.F90
!
! Ed Hartnett 7/26/22
program test_getg2ir
  use bacio_module
  implicit none

  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: msk1, msk2, mnum
  integer :: nlen, nnum, nmess, iret

  interface
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb, msk1, msk2, mnum
       character(len = 1),pointer,dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
  end interface

  ! Open a real GRIB2 file.
  print *, 'Indexing a real GRIB2 file...'
  call baopenr(lugi, "WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) stop 100

  msk1 = 1000
  msk2 = 1000
  mnum = 0
  call getg2ir(lugi, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  if (iret .ne. 0) stop 101
  if (nlen .ne. 137600 .or. nnum .ne. 688 .or. nmess .ne. 688) stop 102
  print *, 'nlen, nnum, nmess: ', nlen, nnum, nmess
  
  deallocate(cbuf)
  
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 199

  print *, 'SUCCESS!...'

end program test_getg2ir
