! This is a test program for NCEPLIBS-g2.
!
! This program tests ixgb2.F90
!
! Ed Hartnett 7/22/22
program test_ixgb2
  use bacio_module
  implicit none

  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lskip, lgrib, numfld, mlen, iret

  interface
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer lugb, lskip, lgrib, numfld, mlen, iret
       character(len = 1),pointer,dimension(:) :: cbuf
     end subroutine ixgb2
  end interface

  call baopenr(lugi, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
  if (iret .ne. 0) then
     print *, 'baopenr failed with iret value: ', iret
     stop 3
  end if

  lskip = 0
  lgrib = 1000
  call ixgb2(lugi, lskip, lgrib, cbuf, numfld, mlen, iret)

  ! Free allocated memory
  deallocate(cbuf)

  call baclose(lugi, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 5
  end if
  print *, 'Success!...'

end program test_ixgb2
