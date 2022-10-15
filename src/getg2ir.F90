!> @file
!> @brief This subroutine reads a GRIB file and returns its index
!> content.
!> @author Mark Iredell @date 1995-10-31

!> This subroutine reads a GRIB file and returns its index content.
!>
!> The index buffer returned contains index records with the internal
!> format:
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before GRIB message
!> - byte 009 - 012 bytes to skip in message before lus (local use) set = 0, if no local section.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 GRIB version number (2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within GRIB2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii+1-  jj grid definition section (gds)
!> - byte jj+1-  kk product definition section (pds)
!> - byte kk+1-  ll the data representation section (drs)
!> - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial
!> 1996-10-31 | Mark Iredell | augmented optional definitions to byte 320
!> 2002-01-02 | Stephen Gilbert | modified from getgir to create GRIB2 indexes
!>
!> @param[in] lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] msk1 Number of bytes to search for first message.
!> @param[in] msk2 Number of bytes to search for other messages.
!> @param[in] mnum Number of GRIB messages to skip (usually 0).
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param[out] nlen Total length of index record buffer in bytes.
!> @param[out] nnum Number of index records, =0 if no GRIB
!> messages are found).
!> @param[out] nmess Last GRIB message in file successfully processed
!> @param[out] iret Return code.
!> - 0 all ok
!> - 1 not enough memory available to hold full index buffer
!> - 2 not enough memory to allocate initial index buffer
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 1995-10-31
subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  use re_alloc ! Needed for subroutine realloc.
  implicit none

  integer, parameter :: init = 50000, next = 10000
  character(len = 1), pointer, dimension(:) :: cbuf
  integer, intent(in) :: lugb, msk1, msk2, mnum
  integer, intent(out) :: nlen, nnum, nmess, iret
  character(len = 1), pointer, dimension(:) :: cbuftmp
  integer :: mbuf, istat, iseek, lskip, lgrib, m, numfld, nbytes, iret1, newsize

  interface ! Required for ixbg2 function, which has a cbuf pointer.
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer, intent(in) :: lugb, lskip, lgrib
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: numfld, mlen, iret
     end subroutine ixgb2
  end interface

  ! Initialize.
  iret = 0
  if (associated(cbuf)) nullify(cbuf)
  mbuf = init
  allocate(cbuf(mbuf), stat = istat)    ! Allocate initial space for cbuf.
  if (istat .ne. 0) then
     iret = 2
     return
  endif

  ! Search for first grib message.
  iseek = 0
  call skgb(lugb, iseek, msk1, lskip, lgrib)
  do m = 1, mnum
     if (lgrib .gt. 0) then
        iseek = lskip + lgrib
        call skgb(lugb, iseek, msk2, lskip, lgrib)
     endif
  enddo

  ! Get index records for every grib message found.
  nlen = 0
  nnum = 0
  nmess = mnum
  do while(iret .eq. 0 .and. lgrib .gt. 0)
     call ixgb2(lugb, lskip, lgrib, cbuftmp, numfld, nbytes, iret1)
     if (iret1 .ne. 0) print *, ' sagt ', numfld, nbytes, iret1

     ! Allocate more space, if necessary.
     if (nbytes + nlen .gt. mbuf) then
        newsize = max(mbuf + next, mbuf + nbytes)
        call realloc(cbuf, nlen, newsize, istat)
        if ( istat .ne. 0 ) then
           iret = 1
           return
        endif
        mbuf = newsize
     endif

     ! If index records were returned in cbuftmp from ixgb2, copy
     ! cbuftmp into cbuf, then deallocate cbuftmp when done.
     if (associated(cbuftmp)) then
        cbuf(nlen + 1 : nlen + nbytes) = cbuftmp(1 : nbytes)
        deallocate(cbuftmp, stat = istat)
        if (istat .ne. 0) then
           print *, ' deallocating cbuftmp ... ', istat
           stop 99
        endif
        nullify(cbuftmp)
        nnum = nnum + numfld
        nlen = nlen + nbytes
        nmess = nmess + 1
     endif

     ! Look for next grib message.
     iseek = lskip + lgrib
     call skgb(lugb, iseek, msk2, lskip, lgrib)
  enddo
end subroutine getg2ir
