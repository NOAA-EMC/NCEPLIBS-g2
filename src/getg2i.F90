!> @file
!> @brief This subroutine read a GRIB2 index file and return its
!> content.
!> @author Mark Iredell @date 1995-10-31

!> Read a grib2 index file and return its contents.
!>
!> Version 1 of the index file has the following format:
!> 81-byte s.lord header with 'gb2ix1' in columns 42-47 followed by
!> 81-byte header with number of bytes to skip before index records,
!> total length in bytes of the index records, number of index records,
!> and grib file basename written in format ('ix1form:',3i10,2x,a40).
!> Each following index record corresponds to a grib message
!> and has the internal format:
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before grib message
!> - byte 009 - 012 bytes to skip in message before lus (local use) set = 0, if no local section.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 grib version number (currently 2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within grib2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii+1-  jj grid definition section (gds)
!> - byte jj+1-  kk product definition section (pds)
!> - byte kk+1-  ll the data representation section (drs)
!> - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial.
!> 1996-10-31 | Mark Iredell | Augmented optional definitions to byte 320.
!> 2002-01-03 | Stephen Gilbert | Modified from getgi to work with grib2.
!>
!> @param[in] lugi Integer unit of the unblocked grib index file. Must
!>  be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[out] cbuf Pointer to a buffer that contains index
!> records. Users should free memory that cbuf points to, using
!> deallocate(cbuf) when cbuf is no longer needed.
!> @param[out] nlen Total length of all index records.
!> @param[out] nnum Number of index records.
!> @param[out] iret Return code.
!> - 0 all ok
!> - 2 not enough memory to hold index buffer
!> - 3 error reading index file buffer
!> - 4 error reading index file header
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 2000-05-26
subroutine getg2i(lugi, cbuf, nlen, nnum, iret)
  implicit none
  character(len=1), pointer, dimension(:) :: cbuf
  integer, intent(in) :: lugi
  integer, intent(out) :: nlen, nnum, iret
  character chead*162
  integer :: ios, istat, lbuf, lhead, nskp

  if (associated(cbuf)) nullify(cbuf)

  nlen = 0
  nnum = 0
  iret = 4
  call baread(lugi, 0, 162, lhead, chead)
  if(lhead.eq.162.and.chead(42:47).eq.'gb2ix1') then
     read(chead(82:162), '(8x, 3i10, 2x, a40)', iostat = ios) nskp, nlen, nnum
     if(ios.eq.0) then

        allocate(cbuf(nlen), stat = istat)    ! allocate space for cbuf
        if (istat.ne.0) then
           iret = 2
           return
        endif
        iret = 0
        call baread(lugi, nskp, nlen, lbuf, cbuf)
        if(lbuf.ne.nlen) iret = 3

     endif
  endif
end subroutine getg2i
