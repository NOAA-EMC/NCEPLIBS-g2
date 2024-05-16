! This is a test program for NCEPLIBS-g2.
!
! This program tests getg2rp2.F90
!
! Ed Hartnett 5/14/24
program test_getgb2rp2
  use g2logging
  use bacio_module
  implicit none

  integer :: lugi
  character(len=1), pointer, dimension(:) :: cbuf(:)
  integer :: lugb = 3
  integer :: nlen, nnum, iret
  logical :: extract
  integer (kind = 8) :: leng8
  character(len=1), pointer, dimension(:) :: gribm
  integer :: idxver, i
  
  ! Interfaces are needed due to pointers in the parameter lists.
  interface
     subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(inout) :: idxver
       character(len = 1), pointer, dimension(:) :: cindex
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getidx2
     subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng8, iret)
       integer, intent(in) :: lugb, idxver
       character(len = 1), intent(in) :: cindex(*)
       logical, intent(in) :: extract
       character(len = 1), pointer, dimension(:) :: gribm
       integer(kind = 8), intent(out) :: leng8
       integer, intent(out) :: iret
     end subroutine getgb2rp2
  end interface

  print *, 'Testing the getgb2rp() subroutine - expect and ignore error messages during test...'

  do i = 1, 2
     ! Open a real GRIB2 file.
     print *, 'Indexing a real GRIB2 file WW3_Regional_US_West_Coast_20220718_0000.grib2...'
     call baopenr(lugb, "data/WW3_Regional_US_West_Coast_20220718_0000.grib2", iret)
     if (iret .ne. 0) stop 100

     idxver = i
     lugi = 0
     leng8 = 0
     g2_log_level = 1

     !lugi = lugb   ! Force regeneration of index from GRIB2 file.
     call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, iret)
     if (iret .ne. 0) stop 101
     if (nnum .ne. 688) stop 102
     if (idxver .eq. 1) then
        if (nlen .ne. 137600) then
           print *, nlen
           stop 103
        endif
     else
        if (nlen .ne. 145856) then
           print *, nlen
           stop 104
        endif
     endif
     print *, 'nlen, nnum: ', nlen, nnum

     ! Extract the whole message.
     extract = .false.
     nullify(gribm)
     call getgb2rp2(lugb, idxver, cbuf, extract, gribm, leng8, iret)
     print *, 'leng8 ', leng8
     if (leng8 .ne. 11183) stop 110
     ! Deallocate buffer that got GRIB message.
     deallocate(gribm)

     ! Extract just the field (same result). 
     extract = .true.
     nullify(gribm)
     call getgb2rp2(lugb, idxver, cbuf, extract, gribm, leng8, iret)
     print *, 'leng8 ', leng8
     if (leng8 .ne. 11183) stop 112
     ! Deallocate buffer that got GRIB message.
     deallocate(gribm)

     call baclose(lugb, iret)
     if (iret .ne. 0) stop 199
  end do

  ! Deallocate the buffer that holds index.
  deallocate(cbuf)

  ! Free library memory.
  !  call gf_finalize()

  print *, 'SUCCESS!...'

end program test_getgb2rp2
