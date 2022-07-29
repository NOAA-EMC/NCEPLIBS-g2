! This is a test program for project NCEPLIBS-g2.
!
! This program tests getg2i.F90
!
! Brian Curtis 02/07/2022
program test_getg2i
  use bacio_module
  implicit none

  integer :: lugi = 3
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: nlen, nnum, iret

  interface
     subroutine getg2i(lugi,cbuf,nlen,nnum,iret)
       integer,intent(in) :: lugi
       character(len=1),pointer,dimension(:) :: cbuf
       integer,intent(out) :: nlen, nnum, iret
     end subroutine getg2i
  end interface

  call baopenr(lugi, "gdaswave.t00z.wcoast.0p16.f000.grib2.idx", iret)
  if (iret .ne. 0) then
     print *, 'baopenr failed with iret value: ', iret
     stop 3
  end if

  call getg2i(lugi, cbuf, nlen, nnum, iret)
  print *, 'nlen, nnum: ', nlen, nnum
  if (iret .ne. 4) then
      print *, 'getg2i failed with iret value: ', iret
      stop 4
  end if

  call baclose(lugi, iret)
  if (iret .ne. 0) then
     print *, 'baclose failed with iret value: ', iret
     stop 5
  end if
  print *, 'Success!...'

!  deallocate(cbuf)
end program test_getg2i
