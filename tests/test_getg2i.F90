! This is a test program for project NCEPLIBS-g2.
!
! This program tests getg2i.F90
!
! Brian Curtis 02/07/2022
! Ed Hartnett
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

  print *, 'Testing index file reading with getg2i().'
  
  ! Open the test file, generated with the utility grb2index.
  call baopenr(lugi, "data/ref_gdaswave.t00z.wcoast.0p16.f000.grb2index", iret)
  if (iret .ne. 0) stop 3

  ! Check that this is an index file, and read it into buffer cbuf.
  call getg2i(lugi, cbuf, nlen, nnum, iret)
  if (iret .ne. 0) stop 4

  ! Feee memory.
  deallocate(cbuf)

  ! Check results.
  if (nlen .ne. 3800 .or. nnum .ne. 19) stop 5

  ! Close the file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 50

  print *, 'Success!...'

!  deallocate(cbuf)
end program test_getg2i
