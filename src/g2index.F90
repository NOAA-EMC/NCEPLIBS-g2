!> @file
!> @brief Subroutines for dealing with indexes.
!> @author Edward Hartnett @date Jan 31, 2024

!> Create a version 1 or 2 index file for a GRIB2 file.
!>
!> @param[in] lugb Logical unit of opened GRIB2 file.
!> @param[in] lugi Logical unit file opened to write index to.
!> @param[in] idxver Index version.
!> @param[in] filename Name of GRIB2 file. This file must already be
!> open, with logical unit passed to the lugb parameter. The filename
!> is also encoded in one of the index header records.
!> @param[out] iret Return code:
!> - 0 success
!> - 90 problem opening GRIB2 file.
!> - 91 problem opening index file.
!> - 92 no messages found in GRIB2 file.
!>
!> @author Ed Hartnett, Mark Iredell @date Feb 15, 2024
subroutine g2_create_index(lugb, lugi, idxver, filename, iret)
  implicit none
  
  integer, intent(in) :: lugb, lugi, idxver
  character*(*) :: filename
  integer, intent(out) :: iret
  
  integer (kind = 8) :: msk1, msk2
  parameter(msk1 = 32000_8, msk2 = 4000_8)
  character(len=1), pointer, dimension(:) :: cbuf
  integer :: numtot, nnum, nlen, mnum, kw
  integer :: irgi, iw, nmess
  
  interface
     subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, &
          nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: msk1, msk2
       integer, intent(in) :: mnum, idxver
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2i2r
  end interface

  ! Assume success.
  iret = 0
  numtot = 0
  nlen = 0

  ! Generate index records for all messages in file, or until memory
  ! runs out.
  mnum = 0
  call getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, &
       nlen, nnum, nmess, irgi)
  if (irgi .gt. 1 .or. nnum .eq. 0 .or. nlen .eq. 0) then
     iret = 92
     return
  endif
  numtot = numtot + nnum
  mnum = mnum + nmess

  ! Write headers.
  call g2_write_index_headers(lugi, nlen, numtot, idxver, filename)
  iw = 162

  ! Write the index data we have so far.
  call bawrite(lugi, iw, nlen, kw, cbuf)
  iw = iw + nlen

  ! Extend index file if index buffer length too large to hold in memory.
  if (irgi .eq. 1) then
     do while (irgi .eq. 1 .and. nnum .gt. 0)
        if (associated(cbuf)) then
           deallocate(cbuf)
           nullify(cbuf)
        endif
        call getg2i2r(11, msk1, msk2, mnum, idxver, cbuf, &
             nlen, nnum, nmess, irgi)
        if (irgi .le. 1 .and. nnum .gt. 0) then
           numtot = numtot + nnum
           mnum = mnum + nmess
           call bawrite(lugi, iw, nlen, kw, cbuf)
           iw = iw + nlen
        endif
     enddo
     ! Go back and overwrite headers with new info.
     call g2_write_index_headers(lugi, iw, numtot, idxver, filename)
  endif
  deallocate(cbuf)  

end subroutine g2_create_index

!> Write index headers.
!>
!> @param[in] lugi integer logical unit of output index file
!> @param[in] nlen integer total length of index records
!> @param[in] nnum integer number of index records
!> @param[in] idxver Index version, 1 for legacy, 2 for files that may
!> be > 2 GB.
!> @param[in] filename character name of GRIB file
!>
!> @author Iredell, Ed Hartnett @date 93-11-22
subroutine g2_write_index_headers(lugi, nlen, nnum, idxver, filename)
  implicit none

  integer, intent(in) :: lugi, nlen, nnum, idxver
  character, intent(in) :: filename*(*)
  
  character cd8*8, ct10*10, hostname*15
#ifdef __GFORTRAN__
  integer istat
#else
  character hostnam*15
  integer hostnm
#endif
  character chead(2)*81
  integer :: kw

  !  fill first 81-byte header
  call date_and_time(cd8, ct10)
  chead(1) = '!GFHDR!'
  chead(1)(9:10) = ' 1'
  chead(1)(12:14) = '  1'
  write(chead(1)(16:20),'(i5)') 162
  chead(1)(22:31) = cd8(1:4) // '-' // cd8(5:6) // '-' // cd8(7:8)
  chead(1)(33:40) = ct10(1:2) // ':' // ct10(3:4) // ':' // ct10(5:6)
  chead(1)(42:47) = 'GB2IX1'
  chead(1)(49:54) = '      '
#ifdef __GFORTRAN__
  istat = hostnm(hostname)
  if (istat .eq. 0) then
     chead(1)(56:70) = '0000'
  else
     chead(1)(56:70) = '0001'
  endif
#else
  chead(1)(56:70) = hostnam(hostname)
#endif
  chead(1)(72:80) = 'grb2index'
  chead(1)(81:81) = char(10)

  !  fill second 81-byte header
  if (idxver .eq. 1) then
     chead(2) = 'IX1FORM:'
  else
     chead(2) = 'IX2FORM:'
  endif
  write(chead(2)(9:38),'(3i10)') 162, nlen, nnum
  chead(2)(41:80) = filename
  chead(2)(81:81) = char(10)

  !  write headers at beginning of index file
  call bawrite(lugi, 0, 162, kw, chead)

  return
end subroutine g2_write_index_headers

!> Find, read or generate a version 1 GRIB2 index for a GRIB2 file
!> (which must be < 2 GB).
!>
!> If the index already exists in library memory, it is returned,
!> otherwise, the index is read from an existing index file associated
!> with unit lugi or generated from the GRIB2 file lugb.
!>
!> Users can force a regeneration of an index: if lugi equals lugb,
!> the index will be regenerated from the data in file lugb. If lugi
!> is less than zero, then the index is re-read from index file
!> abs(lugi).
!>
!> This subroutine allocates memory and stores the resulting pointers
!> in an array that is a Fortran "save" variable. The result is that
!> the memory will not be freed by the library and cannot be reached
!> by the caller. To free this memory call gf_finalize() after all
!> library operations are complete.
!>
!> @note The file unit numbers must be in range 1 - 9999.
!>
!> @param[in] lugb integer unit of the GRIB2 data file.  File must
!> have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine. If 0, then all saved memory will be released (necessary
!> for g2_finalize()).
!> @param[in] lugi integer unit of the GRIB2 index file.
!> If nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. Set to 0 to get index information from the GRIB2 file.
!> @param[inout] cindex character*1 Pointer to a buffer that will get
!> index records.
!> @param[out] nlen integer Total length of all index records.
!> @param[out] nnum integer Number of index records.
!> @param[out] iret integer Return code:
!> - 0 No error.
!> - 90 Unit number out of range.
!> - 96 Error reading/creating index file.
!> - 97 Index version 2 detected.
!>
!> @author Stephen Gilbert, Ed Hartnett @date 2005-03-15
subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
  implicit none

  integer, intent(in) :: lugb, lugi
  character(len = 1), pointer, dimension(:) :: cindex
  integer, intent(out) :: nlen, nnum, iret
  integer :: idxver

  interface
     subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi
       integer, intent(inout) :: idxver
       character(len = 1), pointer, dimension(:) :: cindex
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getidx2
  end interface

  ! When getidx() is called, always use index version 1. Call
  ! getidx2() for a chance to set the index version.
  idxver = 1
  call getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)

  ! If index version 2 is being used, return error.
  if (iret .eq. 0 .and. idxver .eq. 2) iret = 97

end subroutine getidx

!> Find, read or generate a version 1 or 2 GRIB2 index for a GRIB2
!> file (which may be > 2 GB).
!>
!> If the index already exists in library memory, it is returned,
!> otherwise, the index is read from an existing index file associated
!> with unit lugi or generated from the GRIB2 file lugb.
!>
!> Users can force a regeneration of an index: if lugi equals lugb,
!> the index will be regenerated from the data in file lugb. If lugi
!> is less than zero, then the index is re-read from index file
!> abs(lugi).
!>
!> This subroutine allocates memory and stores the resulting pointers
!> in an array that is a Fortran "save" variable. The result is that
!> the memory will not be freed by the library and cannot be reached
!> by the caller. To free this memory call gf_finalize() after all
!> library operations are complete.
!>
!> @note The file unit numbers must be in range 1 - 9999.
!>
!> @param[in] lugb integer unit of the GRIB2 data file.  File must
!> have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine. If 0, then all saved memory will be released (necessary
!> for g2_finalize()).
!> @param[in] lugi integer unit of the GRIB2 index file.
!> If nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. Set to 0 to get index information from the GRIB2 file.
!> @param[in] idxver Index version, 1 for legacy, 2 if files may be > 2 GB.
!> index records.
!> @param[inout] cindex character*1 Pointer to a buffer that will get
!> index records.
!> @param[out] nlen integer Total length of all index records.
!> @param[out] nnum integer Number of index records.
!> @param[out] iret integer Return code:
!> - 0 No error.
!> - 90 Unit number out of range.
!> - 96 Error reading/creating index file.
!>
!> @author Stephen Gilbert, Ed Hartnett @date Feb 9, 2024
subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
  use g2logging
  implicit none

  integer, intent(in) :: lugb, lugi
  integer, intent(inout) :: idxver
  character(len = 1), pointer, dimension(:) :: cindex
  integer, intent(out) :: nlen, nnum, iret
  
  integer, parameter :: maxidx = 10000
  integer (kind = 8), parameter :: msk1 = 32000_8, msk2 = 4000_8
  integer :: lux
  integer :: irgi, mskp, nmess, i

  type gindex
     integer :: nlen
     integer :: nnum
     integer :: idxver
     character(len = 1), pointer, dimension(:) :: cbuf
  end type gindex

  type(gindex), save :: idxlist(10000)

  data lux/0/

  !  declare interfaces (required for cbuf pointer)
  interface
     subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
       integer, intent(in) :: lugi
       character(len=1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: idxver, nlen, nnum, iret
     end subroutine getg2i2
     subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, &
          nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: msk1, msk2
       integer, intent(in) :: mnum, idxver
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2i2r
  end interface

#ifdef LOGGING
  ! Log results for debugging.
  write(g2_log_msg, '(a, i2, a, i2, a, i1)') 'getidx2: lugb ', lugb, ' lugi ', lugi, &
       ' idxver ', idxver
  call g2_log(1)
#endif

  ! Free all associated memory and exit.
  if (lugb .eq. 0) then
     !print *, 'getidx: Freeing all memory'
     do i = 1, 10000
        if (associated(idxlist(i)%cbuf)) then
           !print *, 'deallocating ', loc(idxlist(i)%cbuf)
           deallocate(idxlist(i)%cbuf)
           nullify(idxlist(i)%cbuf)
        endif
     end do
     iret = 0
     return
  endif

  ! Determine whether index buffer needs to be initialized.
  lux = 0
  iret = 0
  if (lugb .le. 0 .or. lugb .gt. 9999) then
     print *, ' file unit number out of range'
     print *, ' use unit numbers in range: 0 - 9999 '
     iret = 90
     return
  endif

  ! Force regeneration of index from GRIB2 file.
  if (lugi .eq. lugb) then      
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     !print *, 'Force regeneration'
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = 0
  endif

  ! Force re-read of index from indexfile.
  if (lugi .lt. 0) then      
     ! associated with unit abs(lugi)
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     !print *, 'Force re-read'
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = abs(lugi)
  endif

  !  Check if index already exists in memory.
  if (associated(idxlist(lugb)%cbuf)) then
     !print *, 'Index exists in memory!'
     cindex => idxlist(lugb)%cbuf
     nlen = idxlist(lugb)%nlen
     nnum = idxlist(lugb)%nnum
     idxver = idxlist(lugb)%idxver
     return
  endif

  ! Either read index from index file, or generate it from the GRIB2
  ! file. This is an index for all messages in the file, each message
  ! gets an index record, all stuffed into idxlist(lugbb)%cbuf.
  irgi = 0
  if (lux .gt. 0) then
     call getg2i2(lux, idxlist(lugb)%cbuf, idxver, nlen, nnum, irgi)
  elseif (lux .le. 0) then
     mskp = 0
     call getg2i2r(lugb, msk1, msk2, mskp, idxver, idxlist(lugb)%cbuf, &
          nlen, nnum, nmess, irgi)
  endif

  ! Handle errors.
  if (irgi .ne. 0) then
     nlen = 0
     nnum = 0
     print *, ' error reading index file '
     iret = 96
     return
  endif

  ! Fill these values.
  cindex => idxlist(lugb)%cbuf
  idxlist(lugb)%nlen = nlen
  idxlist(lugb)%nnum = nnum
  idxlist(lugb)%idxver = idxver

end subroutine getidx2

!> Read a version 1 index file and return its contents.
!>
!> The index file may be generated by the grb2index utility of the
!> [NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util)
!> project.
!>
!> For the contents of the index file, see getgi2().
!>
!> This subroutine is maintained for backward compatibility, and may
!> only be used with version 1 of the index files. To handle both
!> version 1 and 2 index files, use getg2i2().
!>
!> @param[in] lugi Integer unit of the GRIB index file. Must be opened
!>  by [baopen() or baopenr()]
!>  (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. Memory will be allocated within this function, so callers
!> must free the memory that cbuf points to, using deallocate(cbuf)
!> when cbuf is no longer needed.
!> @param[out] nlen Total length of all index records.
!> @param[out] nnum Number of index records.
!> @param[out] iret Return code.
!> - 0 No error.
!> - 2 not enough memory to hold index buffer
!> - 3 error reading index file buffer
!> - 4 error reading index file header
!> - 5 index format 2 detected
!>
!> @author Mark Iredell, Ed Hartnett @date 2000-05-26
subroutine getg2i(lugi, cbuf, nlen, nnum, iret)
  implicit none
  
  integer, intent(in) :: lugi
  character(len=1), pointer, dimension(:) :: cbuf
  integer, intent(out) :: nlen, nnum, iret
  integer :: idxver

  interface
     subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
       integer, intent(in) :: lugi
       character(len=1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: idxver, nlen, nnum, iret
     end subroutine getg2i2
  end interface

  ! Call the version of this subroutine that handles version 1 and
  ! version 2. If getg2i() is called on a version 2 index, return
  ! error.
  call getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
  if (idxver .eq. 2) iret = 5

end subroutine getg2i
  
!> Read a version 1 or 2 index file and return its contents.
!>
!> The index file may be generated by the grb2index utility of the
!> [NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util)
!> project.
!>
!> The index file has two header records:
!> 1. an 81-byte header with 'GB2IX1' in columns 42-47
!> 2. an 81-byte header with the index version number, the number of
!> bytes to skip before index records, total length in bytes of the
!> index records, number of index records, and the GRIB file basename.
!>
!> Each record in the index table contains the following fields. All
!> integers are in big-endian format in the file. The only difference
!> between index version 1 and index version 2 is the size of the
!> field containing the number of bytes to skip in file before
!> message. To accomodate files > 2 GB, this must be a 64-bit int.
!>
!> Index Version 1 | Index Version 2 | Contents
!> ----------------|-----------------|---------
!> 001 - 004 | 001 - 004 | length of index record (4 bytes)
!> 005 - 008 | 005 - 012 | bytes to skip in data file before grib message (4/8 bytes)
!> 009 - 012 | 013 - 020 | bytes to skip in message before lus (local use) set = 0, if no local section. (4/8 bytes)
!> 013 - 016 | 021 - 028 | bytes to skip in message before gds (4/8 bytes)
!> 017 - 020 | 029 - 036 | bytes to skip in message before pds (4/8 bytes)
!> 021 - 024 | 037 - 040 | bytes to skip in message before drs (4 bytes)
!> 025 - 028 | 041 - 044 | bytes to skip in message before bms (4 bytes)
!> 029 - 032 | 045 - 048 | bytes to skip in message before data section (4 bytes)
!> 033 - 040 | 049 - 056 | bytes total in the message (8 bytes)
!> 041 - 041 | 057 - 057 | grib version number (always 2) (1 byte)
!> 042 - 042 | 058 - 058 | message discipline (1 byte)
!> 043 - 044 | 059 - 060 | field number within grib2 message (2 bytes)
!> 045 -  ii | 061 -  ii | identification section (ids) (character)
!> ii+1-  jj | ii+1-  jj | grid definition section (gds) (character)
!> jj+1-  kk | jj+1-  kk | product definition section (pds) (character)
!> kk+1-  ll | kk+1-  ll | the data representation section (drs) (character)
!> ll+1-ll+6 | ll+1-ll+6 | first 6 bytes of the bit map section (bms) (character)
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @param[in] lugi Integer unit of the unblocked GRIB index file. Must
!>  be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. Memory will be allocated within this function, so callers
!> must free the memory that cbuf points to, using deallocate(cbuf)
!> when cbuf is no longer needed.
!> @param[out] idxver Index version of this index, will be 1 or 2.
!> @param[out] nlen Total length of all index records.
!> @param[out] nnum Number of index records.
!> @param[out] iret Return code.
!> - 0 No error.
!> - 2 not enough memory to hold index buffer
!> - 3 error reading index file buffer
!> - 4 error reading index file header
!>
!> @author Ed Hartnett, Mark Iredell @date Feb 9, 2024
subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
  use g2logging
  implicit none
  
  integer, intent(in) :: lugi
  character(len=1), pointer, dimension(:) :: cbuf
  integer, intent(out) :: idxver, nlen, nnum, iret
  
  character chead*162
  integer :: ios, istat, lbuf, lhead, nskp

#ifdef LOGGING
  ! Log results for debugging.
  write(g2_log_msg, *) 'getg2i2: lugi ', lugi
  call g2_log(1)
#endif

  nullify(cbuf)
  nlen = 0
  nnum = 0
  iret = 4
  call baread(lugi, 0, 162, lhead, chead)
  if (lhead .eq. 162 .and. chead(42:47) .eq. 'GB2IX1') then
     read(chead(82:162), '(2x, i1, 5x, 3i10, 2x, a40)', iostat = ios) idxver, nskp, nlen, nnum
     if (ios .eq. 0) then
        allocate(cbuf(nlen), stat = istat)    ! Allocate space for cbuf.
        if (istat .ne. 0) then
           iret = 2
           return
        endif
        iret = 0
        call baread(lugi, nskp, nlen, lbuf, cbuf)
        if (lbuf .ne. nlen) iret = 3
     endif
  endif
end subroutine getg2i2

!> Generate a version 1 index record for each message in a GRIB2 file.
!>
!> The index record contains byte offsets to the message, it's length,
!> and byte offsets within the message to each section. The index file
!> record format is documented in subroutine ixgb2().
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
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
!> @param[out] nnum Number of index records, zero if no GRIB
!> messages are found.
!> @param[out] nmess Last GRIB message in file successfully processed
!> @param[out] iret Return code.
!> - 0 No error.
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 Not enough memory to allocate initial index buffer.
!> - 3 Error deallocating memory.
!>
!> @author Mark Iredell, Ed Hartnett @date 1995-10-31
subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  use re_alloc              ! needed for subroutine realloc
  implicit none

  integer, intent(in) :: lugb
  integer, intent(in) :: msk1, msk2
  integer, intent(in) :: mnum
  character(len = 1), pointer, dimension(:) :: cbuf
  integer, intent(out) :: nlen, nnum, nmess, iret

  integer (kind = 8) :: msk1_8, msk2_8
  
  interface
     subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, &
          nlen, nnum, nmess, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: msk1, msk2
       integer, intent(in) :: mnum, idxver
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2i2r
  end interface

  msk1_8 = msk1
  msk2_8 = msk2
  call getg2i2r(lugb, msk1_8, msk2_8, mnum, 1, cbuf, nlen, nnum, nmess, iret)
end subroutine getg2ir
     
!> Generate a version 1 or 2 index record for each message in a GRIB2
!> file.
!>
!> The index record contains byte offsets to the message, it's length,
!> and byte offsets within the message to each section. The index file
!> record format is documented in subroutine ixgb2().
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @param[in] lugb Unit of the GRIB file. Must be opened by [baopen()
!> or baopenr()] (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] msk1 Number of bytes to search for first message.
!> @param[in] msk2 Number of bytes to search for other messages.
!> @param[in] mnum Number of GRIB messages to skip (usually 0).
!> @param[in] idxver Index version number. Use 1 for legacy, 2 for files > 2 GB.
!> @param[out] cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param[out] nlen Total length of index record buffer in bytes.
!> @param[out] nnum Number of index records, zero if no GRIB
!> messages are found.
!> @param[out] nmess Last GRIB message in file successfully processed
!> @param[out] iret Return code.
!> - 0 No error.
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 Not enough memory to allocate initial index buffer.
!> - 3 Error deallocating memory.
!>
!> @author Mark Iredell, Ed Hartnett @date 1995-10-31
subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, nlen, nnum, nmess, iret)
  use g2logging
  use re_alloc              ! needed for subroutine realloc
  implicit none

  integer, intent(in) :: lugb
  integer (kind = 8), intent(in) :: msk1, msk2
  integer, intent(in) :: mnum, idxver
  character(len = 1), pointer, dimension(:) :: cbuf
  integer, intent(out) :: nlen, nnum, nmess, iret
  
  character(len = 1), pointer, dimension(:) :: cbuftmp
  integer :: nbytes, newsize, next, numfld, m, mbuf
  integer (kind = 8) :: iseek, lskip, lgrib
  integer :: istat, init, iret1, lgrib4
  parameter(init = 50000, next = 10000)

  interface      ! required for cbuf pointer
     subroutine ix2gb2(lugb, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: lskip8
       integer, intent(in)  :: idxver
       integer (kind = 8), intent(in) :: lgrib8
       character(len = 1), pointer, intent(inout), dimension(:) :: cbuf
       integer, intent(out)  :: numfld, mlen, iret
     end subroutine ix2gb2
  end interface

#ifdef LOGGING
  ! Log results for debugging.
  write(g2_log_msg, '(a, i2, a, i7, a, i7, a, i5, a, i1)') 'getg2i2r: lugb ', lugb, ' msk1 ', msk1, ' msk2 ', msk2, &
       ' mnum ', mnum, ' idxver ', idxver
  call g2_log(1)
#endif

  ! Initialize.
  iret = 0
  nullify(cbuf)
  mbuf = init
  allocate(cbuf(mbuf), stat = istat)    ! allocate initial space for cbuf.
  if (istat .ne. 0) then
     iret = 2
     return
  endif

  ! Search for first grib message.
  iseek = 0_8
  call skgb8(lugb, iseek, msk1, lskip, lgrib)
  do m = 1, mnum
     if (lgrib .gt. 0) then
        iseek = lskip + lgrib
        call skgb8(lugb, iseek, msk2, lskip, lgrib)
     endif
  enddo

  ! Get index records for every grib message found.
  nlen = 0
  nnum = 0
  nmess = mnum
  do while (iret .eq. 0 .and. lgrib .gt. 0)
     lgrib4 = int(lgrib, kind(4))
     call ix2gb2(lugb, lskip, idxver, lgrib, cbuftmp, numfld, nbytes, iret1)
     if (iret1 .ne. 0) print *, ' SAGT ', numfld, nbytes, iret1
     if (nbytes + nlen .gt. mbuf) then             ! Allocate more space, if necessary.
        newsize = max(mbuf + next, mbuf + nbytes)
        call realloc(cbuf, nlen, newsize, istat)
        if (istat .ne. 0) then
           iret = 1
           return
        endif
        mbuf = newsize
     endif

     ! If index records were returned in cbuftmp from ixgb2, 
     ! copy cbuftmp into cbuf, then deallocate cbuftmp when done.
     if (associated(cbuftmp)) then
        cbuf(nlen + 1 : nlen + nbytes) = cbuftmp(1 : nbytes)
        deallocate(cbuftmp, stat = istat)
        if (istat .ne. 0) then
           print *, ' deallocating cbuftmp ... ', istat
           iret = 3
           return
        endif
        nullify(cbuftmp)
        nnum = nnum + numfld
        nlen = nlen + nbytes
        nmess = nmess + 1
     endif

     ! Look for next grib message.
     iseek = lskip + lgrib
     call skgb8(lugb, iseek, msk2, lskip, lgrib)
  enddo
end subroutine getg2i2r

!> Find information about a GRIB field from the index and fill a @ref
!> grib_mod::gribfield.
!>
!> For a description of the index record see getg2i2().
!>
!> Users of this routine will need to include the line "use grib_mod"
!> in their calling routine.
!>
!> The unpacked bitmap and bitmap data field are the only components
!> of the @ref grib_mod::gribfield type not set by this routine.
!>
!> @note This subprogram is intended for private use by getgb2()
!> routines only. Note that derived type @ref grib_mod::gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set. Users must free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @param[in] cbuf Buffer (of size nlen bytes) containing index data.
!> @param[in] nlen Total length of all index records.
!> @param[in] nnum Number of index records.
!> @param[in] j Number of fields to skip (0 to search from beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field. See
!> [GRIB2 - TABLE 0.0 -
!> DISCIPLINE](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml).
!> Use -1 to accept any discipline.
!> @param[in] jids Array of values in the identification
!> section. (Set to -9999 for wildcard.)
!> - jids(1) Identification of originating centre. See [TABLE 0 -
!>   NATIONAL/INTERNATIONAL ORIGINATING
!>   CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html).
!> - jids(2) Identification of originating sub-centre. See [TABLE C -
!>   NATIONAL
!>   SUB-CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html).
!> - jids(3) GRIB master tables version number. See [GRIB2 - TABLE 1.0
!>   - GRIB Master Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml).
!> - jids(4) GRIB local tables version number. See [GRIB2 - TABLE 1.1
!>   - GRIB Local Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml).
!> - jids(5) Significance of reference time. See [GRIB2 - TABLE 1.2 -
!>   Significance of Reference
!>   Time](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-2.shtml).
!> - jids(6) year (4 digits)
!> - jids(7) month
!> - jids(8) day
!> - jids(9) hour
!> - jids(10) minute
!> - jids(11) second
!> - jids(12) Production status of processed data. See [GRIB2 - TABLE
!>   1.3 - Production Status of
!>   Data](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml).
!> - jids(13) Type of processed data. See [GRIB2 - TABLE 1.4 - TYPE OF
!>   DATA](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml).
!> @param[in] jpdtn Product Definition Template (PDT) number (n)
!> (if = -1, don't bother matching PDT - accept any).
!> @param[in] jpdt Array of values defining the Product Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[out] k Field number unpacked.
!> @param[out] gfld Derived type @ref grib_mod::gribfield.
!> @param[out] lpos Starting position of the found index record
!> within the complete index buffer, CBUF. = 0, if request not found.
!> @param[out] iret integer return code:
!> - 0 No error.
!> - 97 Error reading GRIB file.
!> - other gf_getfld GRIB2 unpacker return code.
!>
!> @author Stephen Gilbert, Ed Hartnett @date 2002-01-15
subroutine getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
     jgdt, k, gfld, lpos, iret)
  use grib_mod
  implicit none

  character(len = 1), intent(in) :: cbuf(nlen)
  integer, intent(in) :: nlen, nnum, j, jdisc
  integer, dimension(:) :: jids(*)
  integer, intent(in) :: jpdtn
  integer, dimension(:) :: jpdt(*)
  integer, intent(in) :: jgdtn
  integer, dimension(:) :: jgdt(*)
  integer, intent(out) :: k
  type(gribfield), intent(out) :: gfld
  integer, intent(out) :: lpos, iret

  interface
     subroutine getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
          jgdt, k, gfld, lpos, iret)
       import gribfield
       character(len = 1), intent(in) :: cbuf(nlen)
       integer, intent(in) :: idxver, nlen, nnum, j, jdisc
       integer, dimension(:) :: jids(*)
       integer, intent(in) :: jpdtn
       integer, dimension(:) :: jpdt(*)
       integer, intent(in) :: jgdtn
       integer, dimension(:) :: jgdt(*)
       integer, intent(out) :: k
       type(gribfield), intent(out) :: gfld
       integer, intent(out) :: lpos, iret
     end subroutine getgb2s2
  end interface

  ! When getgb2s() is called, always use index version 1. Call
  ! getgb2s2() to handle version 1 or 2.
  call getgb2s2(cbuf, 1, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
       jgdt, k, gfld, lpos, iret)

end subroutine getgb2s

!> Find information about a GRIB field from the index and fill a @ref
!> grib_mod::gribfield.
!>
!> For a description of the index record see getg2i2().
!>
!> Users of this routine will need to include the line "use grib_mod"
!> in their calling routine.
!>
!> The unpacked bitmap and bitmap data field are the only components
!> of the @ref grib_mod::gribfield type not set by this routine.
!>
!> @note This subprogram is intended for private use by getgb2()
!> routines only. Note that derived type @ref grib_mod::gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set. Users must free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @param[in] cbuf Buffer (of size nlen bytes) containing index data.
!> @param[in] idxver Index version, 1 for legacy, 2 if files may be > 2 GB.
!> @param[in] nlen Total length of all index records.
!> @param[in] nnum Number of index records.
!> @param[in] j Number of fields to skip (0 to search from beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field. See
!> [GRIB2 - TABLE 0.0 -
!> DISCIPLINE](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml).
!> Use -1 to accept any discipline.
!> @param[in] jids Array of values in the identification
!> section. (Set to -9999 for wildcard.)
!> - jids(1) Identification of originating centre. See [TABLE 0 -
!>   NATIONAL/INTERNATIONAL ORIGINATING
!>   CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html).
!> - jids(2) Identification of originating sub-centre. See [TABLE C -
!>   NATIONAL
!>   SUB-CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html).
!> - jids(3) GRIB master tables version number. See [GRIB2 - TABLE 1.0
!>   - GRIB Master Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml).
!> - jids(4) GRIB local tables version number. See [GRIB2 - TABLE 1.1
!>   - GRIB Local Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml).
!> - jids(5) Significance of reference time. See [GRIB2 - TABLE 1.2 -
!>   Significance of Reference
!>   Time](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-2.shtml).
!> - jids(6) year (4 digits)
!> - jids(7) month
!> - jids(8) day
!> - jids(9) hour
!> - jids(10) minute
!> - jids(11) second
!> - jids(12) Production status of processed data. See [GRIB2 - TABLE
!>   1.3 - Production Status of
!>   Data](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml).
!> - jids(13) Type of processed data. See [GRIB2 - TABLE 1.4 - TYPE OF
!>   DATA](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml).
!> @param[in] jpdtn Product Definition Template (PDT) number (n)
!> (if = -1, don't bother matching PDT - accept any).
!> @param[in] jpdt Array of values defining the Product Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[out] k Field number unpacked.
!> @param[out] gfld Derived type @ref grib_mod::gribfield.
!> @param[out] lpos Starting position of the found index record
!> within the complete index buffer, CBUF. = 0, if request not found.
!> @param[out] iret integer return code:
!> - 0 No error.
!> - 97 Error reading GRIB file.
!> - other gf_getfld GRIB2 unpacker return code.
!>
!> @author Stephen Gilbert, Ed Hartnett @date Feb 9 2024
subroutine getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
     jgdt, k, gfld, lpos, iret)
  use grib_mod
  use g2logging
  implicit none

  character(len = 1), intent(in) :: cbuf(nlen)
  integer, intent(in) :: idxver, nlen, nnum, j, jdisc
  integer, dimension(:) :: jids(*)
  integer, intent(in) :: jpdtn
  integer, dimension(:) :: jpdt(*)
  integer, intent(in) :: jgdtn
  integer, dimension(:) :: jgdt(*)
  integer, intent(out) :: k
  type(gribfield), intent(out) :: gfld
  integer, intent(out) :: lpos, iret

  integer :: kgds(5)
  logical :: match1, match3, match4
  integer :: i, icnd, inlen, iof, ipos, jpos, lsec1, lsec3, lsec4, lsec5, numgdt, numpdt, inc

  interface
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
     subroutine gf_unpack1(cgrib, lcgrib, iofst, ids, idslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: ids
       integer, intent(out) :: ierr, idslen
     end subroutine gf_unpack1
     subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
          mapgridlen, ideflist, idefnum, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: igdstmpl, ideflist
       integer, intent(out) :: igds(5)
       integer, intent(out) :: ierr, idefnum
     end subroutine gf_unpack3
     subroutine gf_unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
          mappdslen, coordlist, numcoord, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       real, pointer, dimension(:) :: coordlist
       integer, pointer, dimension(:) :: ipdstmpl
       integer, intent(out) :: ipdsnum
       integer, intent(out) :: ierr, numcoord
     end subroutine gf_unpack4
     subroutine gf_unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum, &
          idrstmpl, mapdrslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: ndpts, idrsnum
       integer, pointer, dimension(:) :: idrstmpl
       integer, intent(out) :: ierr
     end subroutine gf_unpack5
  end interface

#ifdef LOGGING
  ! Log results for debugging.
  write(g2_log_msg, '(a, i1, a, i5, a, i7, a, i3, a, i3)') 'getgb2s2: idxver ', idxver, ' nlen ', nlen, &
       ' nnum ', nnum, ' j ', j, ' jdisc ', jdisc
  call g2_log(1)
#endif

  ! Initialize.
  k = 0
  lpos = 0
  iret = 1
  ipos = 0
  nullify(gfld%idsect, gfld%local)
  nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
  nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)
  if (idxver .eq. 1) then
     inc = 0
  else
     ! Add the extra 8 bytes in the version 2 index record, starting
     ! at byte 9.
     inc = 16
  endif

  ! Search for request.
  do while (iret .ne. 0 .and. k .lt. nnum)
     k = k + 1
     ! Get length of current index record.
     call g2_gbytec1(cbuf, inlen, ipos * 8, 4 * 8)    
     if (k .le. j) then           ! skip this index
        ipos = ipos + inlen
        cycle
     endif

     ! Check if grib2 discipline is a match.
     call g2_gbytec1(cbuf, gfld%discipline, (ipos + inc + 41) * 8, 1 * 8)
     if (jdisc .ne. -1 .and. jdisc .ne. gfld%discipline) then
        ipos = ipos + inlen
        cycle
     endif

     ! Check if identification section is a match.
     match1 = .false.
     ! Get length of ids.
     call g2_gbytec1(cbuf, lsec1, (ipos + inc + 44) * 8, 4 * 8)  
     iof = 0
     call gf_unpack1(cbuf(ipos + inc + 45), lsec1, iof, gfld%idsect, gfld%idsectlen, icnd)
     if (icnd .eq. 0) then
        match1 = .true.
        do i = 1, gfld%idsectlen
           if (jids(i) .ne. -9999 .and. jids(i) .ne. gfld%idsect(i)) then
              match1 = .false.
              exit
           endif
        enddo
     endif
     if (.not. match1) then
        deallocate(gfld%idsect)
        ipos = ipos + inlen
        cycle
     endif

     ! Check if grid definition template is a match.
     jpos = ipos + 44 + inc + lsec1
     match3 = .false.
     call g2_gbytec1(cbuf, lsec3, jpos * 8, 4 * 8)  ! get length of gds 
     if (jgdtn .eq. -1) then
        match3 = .true.
     else
        call g2_gbytec1(cbuf, numgdt, (jpos + 12) * 8, 2 * 8)  ! get gdt template no.
        if (jgdtn .eq. numgdt) then
           iof = 0
           call gf_unpack3(cbuf(jpos + 1), lsec3, iof, kgds, gfld%igdtmpl, &
                gfld%igdtlen, gfld%list_opt, gfld%num_opt, icnd)
           if (icnd .eq. 0) then
              match3 = .true.
              do i = 1, gfld%igdtlen
                 if (jgdt(i) .ne. -9999 .and. jgdt(i).ne.gfld%igdtmpl(i)) then
                    match3 = .false.
                    exit
                 endif
              enddo
           endif
        endif
     endif
     if (.not. match3) then
        if (associated(gfld%idsect)) deallocate(gfld%idsect)
        if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
        if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
        ipos = ipos + inlen
        cycle
     else
        gfld%griddef = kgds(1)
        gfld%ngrdpts = kgds(2)
        gfld%numoct_opt = kgds(3)
        gfld%interp_opt = kgds(4)
        gfld%igdtnum = kgds(5)
     endif

     ! Check if product definition template is a match.
     jpos = jpos + lsec3
     match4 = .false.

     ! Get length of pds.      
     call g2_gbytec1(cbuf, lsec4, jpos * 8, 4 * 8)  
     if (jpdtn .eq. -1) then
        match4 = .true.
     else
        ! Get pdt template no.        
        call g2_gbytec1(cbuf, numpdt, (jpos + 7) * 8, 2 * 8)  
        if (jpdtn .eq. numpdt) then
           iof = 0
           call gf_unpack4(cbuf(jpos + 1), lsec4, iof, gfld%ipdtnum, &
                gfld%ipdtmpl, gfld%ipdtlen, gfld%coord_list, gfld%num_coord, icnd)
           if (icnd .eq. 0) then
              match4 = .true.
              do i = 1, gfld%ipdtlen
                 if (jpdt(i) .ne. -9999 .and. jpdt(i) .ne. gfld%ipdtmpl(i)) then
                    match4 = .false.
                    exit
                 endif
              enddo
           endif
        endif
     endif
     if (.not. match4) then
        if (associated(gfld%idsect)) deallocate(gfld%idsect)
        if (associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
        if (associated(gfld%coord_list)) deallocate(gfld%coord_list)
     endif

     ! If request is found set values for derived type gfld and return.
     if (match1 .and. match3 .and. match4) then
        lpos = ipos + 1
        call g2_gbytec1(cbuf, gfld%version, (ipos + inc + 40) * 8, 1 * 8)
        call g2_gbytec1(cbuf, gfld%ifldnum, (ipos + inc + 42) * 8, 2 * 8)
        gfld%unpacked = .false.
        jpos = ipos + 44 + inc + lsec1
        if (jgdtn .eq. -1) then     ! unpack gds, if not done before
           iof = 0
           call gf_unpack3(cbuf(jpos + 1), lsec3, iof, kgds, gfld%igdtmpl, &
                gfld%igdtlen, gfld%list_opt, gfld%num_opt, icnd)
           gfld%griddef = kgds(1)
           gfld%ngrdpts = kgds(2)
           gfld%numoct_opt = kgds(3)
           gfld%interp_opt = kgds(4)
           gfld%igdtnum = kgds(5)
        endif
        jpos = jpos + lsec3
        if (jpdtn .eq. -1) then     ! unpack pds, if not done before
           iof = 0
           call gf_unpack4(cbuf(jpos + 1), lsec4, iof, gfld%ipdtnum, &
                gfld%ipdtmpl, gfld%ipdtlen, gfld%coord_list, gfld%num_coord, icnd)
        endif
        jpos = jpos + lsec4
        call g2_gbytec1(cbuf, lsec5, jpos * 8, 4 * 8)  ! get length of drs 
        iof = 0
        call gf_unpack5(cbuf(jpos + 1), lsec5, iof, gfld%ndpts, &
             gfld%idrtnum, gfld%idrtmpl, gfld%idrtlen, icnd)
        jpos = jpos + lsec5
        call g2_gbytec1(cbuf, gfld%ibmap, (jpos + 5) * 8, 1 * 8)  ! get ibmap
        iret = 0
     else      ! pdt did not match
        ipos = ipos + inlen
     endif
  enddo
end subroutine getgb2s2

!> Generate a version 1 index record for each field in a GRIB2 message.
!>
!> The index records are written to index buffer pointed to by
!> cbuf. All integers in the index are in big-endian format.
!>
!> This subroutine is called by getg2ir(), which packages the index
!> records into an index file.
!>
!> See getg2i2() for thr format of the index buffer records.
!>
!> @param lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param lskip Number of bytes to skip before GRIB message.
!> @param lgrib Number of bytes in GRIB message. When subroutine is
!> called, this must be set to the size of the cbuf buffer.
!> @param cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param numfld Number of index records created.
!> @param mlen Total length of all index records.
!> @param iret Return code
!> - 0 No error
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 I/O error in read.
!> - 3 GRIB message is not edition 2.
!> - 4 Not enough memory to allocate extent to index buffer.
!> - 5 Unidentified GRIB section encountered.
!>
!> @author Mark Iredell, Ed Hartnett @date 1995-10-31
subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
  use re_alloc              ! needed for subroutine realloc
  implicit none

  integer :: lugb, lskip, lgrib
  character(len = 1), pointer, dimension(:) :: cbuf
  integer :: numfld, mlen, iret
  integer (kind = 8) :: lskip8, lgrib8

  interface
     subroutine ix2gb2(lugb, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
       integer, intent(in) :: lugb
       integer (kind = 8), intent(in) :: lskip8
       integer, intent(in)  :: idxver
       integer (kind = 8), intent(in) :: lgrib8
       character(len = 1), pointer, intent(inout), dimension(:) :: cbuf
       integer, intent(out)  :: numfld, mlen, iret
     end subroutine ix2gb2
  end interface

  ! Always use index version 1 from this subroutine.
  lskip8 = lskip
  lgrib8 = lgrib
  call ix2gb2(lugb, lskip8, 1, lgrib8, cbuf, numfld, mlen, iret)
  lgrib = int(lgrib8, 4)
end subroutine ixgb2

!> Generate a version 1 or 2 index record for each field in a GRIB2
!> message.
!>
!> The index records are written to index buffer pointed to by
!> cbuf. All integers in the index are in big-endian format.
!>
!> This subroutine is called by getg2ir(), which packages the index
!> records into an index file.
!>
!> See getg2i2() for thr format of the index buffer records.
!>
!> @param[in] lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] lskip8 Number of bytes to skip before GRIB message.
!> @param[in] idxver Index version, use 1 for legacy, 2 for GRIB2 files > 2 GB.
!> @param lgrib8 Number of bytes in GRIB message. When subroutine is
!> called, if this is < 5000, it will be used as the number of bytes
!> initially read from the file. So for GRIB2 messages of < 5000 in
!> size, this should be set to the size of the GRIB2 message.
!> @param[inout] cbuf Pointer to a buffer that will get the index
!> records. If any memory is associated with cbuf when this subroutine
!> is called, cbuf will be nullified in the subroutine. Initially cbuf
!> will get an allocation of 5000 bytes. realloc() will be used to
!> increase the size if necessary. Users must free memory that cbuf
!> points to when cbuf is no longer needed.
!> @param[out] numfld Number of index records created.
!> @param[out] mlen Total length of all index records.
!> @param[out] iret Return code
!> - 0 No error
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 I/O error in read.
!> - 3 GRIB message is not edition 2.
!> - 4 Not enough memory to allocate extent to index buffer.
!> - 5 Unidentified GRIB section encountered.
!>
!> @author Ed Hartnett, Mark Iredell @date Feb 5, 2024
subroutine ix2gb2(lugb, lskip8, idxver, lgrib8, cbuf, numfld, mlen, iret)
  use re_alloc              ! needed for subroutine realloc
  use g2logging
  implicit none

  ! Subroutine parameters.
  integer, intent(in) :: lugb
  integer (kind = 8), intent(in) :: lskip8
  integer, intent(in)  :: idxver
  integer (kind = 8), intent(in) :: lgrib8
  character(len = 1), pointer, intent(inout), dimension(:) :: cbuf
  integer, intent(out)  :: numfld, mlen, iret
  
  character cver, cdisc
  character(len = 4) :: ctemp
  integer (kind = 8) :: loclus8, locgds8
  integer locgds, locbms, loclus
  integer :: indbmp, numsec, newsize, g2_mova2i, mbuf, lindex
  integer :: lskip
  integer :: ilndrs, ilnpds, istat
  integer (kind = 8) :: ibread8, lbread8, ibskip8, lengds8
  integer (kind = 8) :: ilnpds8, ilndrs8
  integer :: lensec, lensec1
  integer :: mypos, inc

  ! Parameters.
  ! Size of the internal char buffers used in this subroutine.
  integer :: LINMAX
  parameter(LINMAX = 5000)
  ! Initial size of buffer to hold index record in cbuf array.
  integer :: INIT
  parameter(INIT = 50000)
  ! If more space is needed in cbuf, it will get this much more.
  integer :: NEXT
  parameter(NEXT = 10000)
  ! Number of bytes of the BMS section put into index record.
  integer :: MXBMS
  parameter(MXBMS = 6)
  integer :: IXDS
  ! Bytes to skip in (version 1) index record to get to section 0.  
  integer :: IXIDS
  parameter(IXIDS = 44) 
  integer :: IXSDR
  parameter(IXSDR = 20, IXDS = 28)
  ! Bytes to skip in (version 1) index record to get to bms.    
  integer :: IXBMS
  parameter(IXBMS = 24)
  ! Sizes of integers in bits.
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)
  
  ! Buffers.
  character cbread(LINMAX), cindex(LINMAX)
  character cids(LINMAX), cgds(LINMAX)

  interface
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
  end interface

#ifdef LOGGING
  ! Log results for debugging.
  write(g2_log_msg, *) 'ix2gb2: lugb ', lugb, ' lskip8 ', lskip8, ' idxver ', idxver
  call g2_log(1)
#endif

  ! Are we using index version 1 (legacy), or version 2 (introduced to
  ! handle files > 2 GB).
  if (idxver .eq. 1) then
     inc = 0
  else
     ! Add the extra bytes in the version 2 index record, starting at
     ! byte 9. This is because some values early in the index record
     ! changed from 4-byte ints to 8-byte ints. This is the total
     ! extra bytes that were added to the beginning of the index
     ! record in version 2.
     inc = 16
  endif

  ! Initialize values and allocate buffer (at the user-provided cbuf
  ! pointer) where the index data will be written. When subroutine is
  ! complete, cbuf will hold either version 1 or 2 index data.
  loclus = 0
  loclus8 = 0
  iret = 0
  mlen = 0
  numfld = 0
  nullify(cbuf)
  mbuf = INIT
  allocate(cbuf(mbuf), stat = istat)    ! allocate initial space for cbuf
  if (istat .ne. 0) then
     iret = 1
     return
  endif

  ! Read up to 5000 bytes from the file into buffer cbread. lskip8
  ! must be set to the beginning of a GRIB2 message in the file.
  ibread8 = min(lgrib8, LINMAX)
  call bareadl(lugb, lskip8, ibread8, lbread8, cbread)
  if (lbread8 .ne. ibread8) then
     iret = 2
     return
  endif

  ! Check GRIB version from section 0, must be 2.
  if (cbread(8) .ne. char(2)) then          !  not grib edition 2
     iret = 3
     return
  endif

  ! Remember the GRIB version and discipline.
  cver = cbread(8)
  cdisc = cbread(7)

  ! Read the length of section 1 from the file data buffer.
  call g2_gbytec1(cbread, lensec1, 16 * 8, INT4_BITS)
  lensec1 = min(lensec1, int(ibread8, kind(lensec1)))

  ! Copy section 1 values into cids array.
  cids(1:lensec1) = cbread(17:16 + lensec1)
  ! do i = 1, lensec1
  !    print *, i, ichar(cids(i))
  ! end do

  ! Skip past section 1 in the data buffer.
  ibskip8 = lskip8 + 16_8 + int(lensec1, kind(8))

  ! Loop through remaining sections creating an index for each
  ! field. This overwrites the cbread data buffer.
  ibread8 = max(5, MXBMS)
  do
     ! Read 6 bytes from file into cbread buffer. (Why 6? Should this
     ! be 5?)
     call bareadl(lugb, ibskip8, ibread8, lbread8, cbread)

     ! Check if the first 4 bytes are '7777', indicating end of
     ! message. If we found end of message, we are done.
     ctemp = cbread(1)//cbread(2)//cbread(3)//cbread(4)
     if (ctemp .eq. '7777') return        ! end of message found

     ! If we did not find end of message, check that we read 6 bytes.
     if (lbread8 .ne. ibread8) then
        iret = 2
        return
     endif

     ! Read the 4-byte section length, and then the 1-byte section
     ! number.
     call g2_gbytec1(cbread, lensec, 0, INT4_BITS)
     call g2_gbytec1(cbread, numsec, INT4_BITS, INT1_BITS)

     ! Based on the section number, generate index data for each
     ! section.
     if (numsec .eq. 2) then
        ! Save the location of the local use section in the message.
        loclus8 = ibskip8 - lskip8
        loclus = int(ibskip8 - lskip8, kind(4))
     elseif (numsec .eq. 3) then                 
        ! For the GDS section, read the whole section into the cgds
        ! buffer.
        lengds8 = lensec
        cgds = char(0)
        call bareadl(lugb, ibskip8, lengds8, lbread8, cgds)
        if (lbread8 .ne. lengds8) then
           iret = 2
           return
        endif
        ! Remember the GDS location in the message.        
        locgds = int(ibskip8 - lskip8, kind(4))
        locgds8 = ibskip8 - lskip8
     elseif (numsec .eq. 4) then
        ! Having found the PDS, we write the beginning of the index
        ! record into the cindex buffer.
        cindex = char(0)

        ! The first 4 bytes are the length of the index record. We
        ! don't know that yet, so skip it for now,
        mypos = INT4_BITS

        ! Write the beginning of the index record. It contains bytes
        ! to skip in the file to reach the message, and offsets to
        ! each section within the message. Index version 1 uses 4-byte
        ! ints for these values, index version 2 uses 8-byte ints.
        if (idxver .eq. 1) then
           inc = 0
           lskip = int(lskip8, kind(4))
           call g2_sbytec(cindex, lskip, mypos, INT4_BITS)    ! bytes to skip
           !print '(i3, a7, i4)', mypos/8, ' lskip ', lskip, mypos
           mypos = mypos + INT4_BITS
           call g2_sbytec(cindex, loclus, mypos, INT4_BITS)   ! location of local use
           !print '(i3, a8, i4)', mypos/8, ' loclus ', loclus
           mypos = mypos + INT4_BITS
           call g2_sbytec(cindex, locgds, mypos, INT4_BITS)   ! location of gds
           !print '(i3, a8, i4)', mypos/8, ' locgds ', locgds
           mypos = mypos + INT4_BITS
           call g2_sbytec(cindex, int(ibskip8 - lskip8, kind(4)), mypos, INT4_BITS)  ! location of pds
#ifdef LOGGING
           write(g2_log_msg, *) ' writing pds location to index: mypos/8 ', mypos/8, &
                ' loc ', int(ibskip8 - lskip8, kind(4))
           call g2_log(2)
#endif
           !print '(i3, a8, i4)', mypos/8, ' locpds ', int(ibskip8 - lskip8, kind(4))
           mypos = mypos + INT4_BITS
        else
           inc = 16
           call g2_sbytec8(cindex, lskip8, mypos, INT8_BITS)    ! bytes to skip
           !print '(i3, a7, i4)', mypos/8, ' lskip ', lskip
           mypos = mypos + INT8_BITS
           call g2_sbytec8(cindex, loclus8, mypos, INT8_BITS)   ! location of local use
           !print '(i3, a8, i4)', mypos/8, ' loclus ', loclus
           mypos = mypos + INT8_BITS
           call g2_sbytec8(cindex, locgds8, mypos, INT8_BITS)   ! location of gds
           !print '(i3, a8, i4)', mypos/8, ' locgds ', locgds
           mypos = mypos + INT8_BITS
           call g2_sbytec8(cindex, ibskip8 - lskip8, mypos, INT8_BITS)  ! location of pds
           !print '(i3, a8, i4)', mypos/8, ' locpds ', int(ibskip8 - lskip8, kind(4))
#ifdef LOGGING
           write(g2_log_msg, *) ' writing pds location to index: mypos/8 ', mypos/8, &
                ' loc ', ibskip8 - lskip8
           call g2_log(2)
#endif
           mypos = mypos + INT8_BITS
        endif

        ! These ints are the same size in index version 1 and 2. The
        ! mypos variable contains the proper offset, which is
        mypos = mypos + INT4_BITS * 3 ! skip ahead in cbuf
        call g2_sbytec8(cindex, lgrib8, mypos, INT8_BITS)    ! len of grib2
        !print '(i3, a8, i4)', mypos/8, ' lgrib8 ', lgrib8
        mypos = mypos + INT8_BITS
        cindex((mypos / 8) + 1) = cver
        !print '(i3, a6, z1)', mypos/8, ' cver ', cver
        mypos = mypos + INT1_BITS
        cindex((mypos / 8) + 1) = cdisc
        !print '(i3, a7, z2)', mypos/8, ' cdisc ', cdisc
        mypos = mypos + INT1_BITS
        call g2_sbytec(cindex, numfld + 1, mypos, INT2_BITS)   ! field num
        !print '(i3, a8, i4)', mypos/8, ' numfld ', numfld + 1
        mypos = mypos + INT2_BITS

        ! Copy the section 1 values into the cindex buffer.
        cindex(IXIDS + 1 + inc:IXIDS + lensec1 + inc) = cids(1:lensec1)
        lindex = IXIDS + lensec1 + inc
        ! print *, 'section 1:', IXIDS + inc, IXIDS + lensec1 + inc
        ! do i=1, lensec1
        !    print *, i, ichar(cids(i))
        ! end do

        ! Copy the GDS values into the cindex buffer.
        cindex(lindex + 1:lindex + lengds8) = cgds(1:lengds8)
        !print *, 'gds:', lindex, lindex + lengds8
        lindex = lindex + int(lengds8, kind(lindex))
        ! print *, 'gds:', lindex, lengds8
        ! do i=1, lengds8
        !    print *, i, ichar(cgds(i))
        ! end do

        ! Now read the PDS values from the file directly into cindex.
        ilnpds = lensec
        ilnpds8 = ilnpds        
        call bareadl(lugb, ibskip8, ilnpds8, lbread8, cindex(lindex + 1))
        if (lbread8 .ne. ilnpds8) then
           iret = 2
           return
        endif
        !print *, 'pds:', lindex, lindex + ilnpds
        lindex = lindex + ilnpds
     elseif (numsec .eq. 5) then
        ! Write the byte offset to the DRS section into the cindex buffer.
        mypos = (IXSDR + inc) * INT1_BITS
        call g2_sbytec(cindex, int(ibskip8 - lskip8, kind(4)), mypos, INT4_BITS)  ! location of drs
        !print '(i3, a8, i5)', mypos/8, ' locdrs ', int(ibskip8 - lskip8, kind(4))
        
        ! Read the DRS section directly into the cindex buffer.
        ilndrs = lensec
        ilndrs8 = ilndrs
        call bareadl(lugb, ibskip8, ilndrs8, lbread8, cindex(lindex + 1))
        if (lbread8 .ne. ilndrs8) then
           iret = 2
           return
        endif
        !print *, 'drs:', lindex, lindex + ilndrs
        lindex = lindex + ilndrs
     elseif (numsec .eq. 6) then    
        ! Write the location of the BMS section in the message into
        ! the cindex buffer.
        indbmp = g2_mova2i(cbread(6))
        mypos = (IXBMS + inc) * INT1_BITS           
        if (indbmp .lt. 254) then
           locbms = int(ibskip8 - lskip8, kind(4))
           call g2_sbytec(cindex, locbms, mypos, INT4_BITS)  ! loc. of bms
           !print '(i3, a8, i5)', mypos/8, ' locbms ', int(ibskip8 - lskip8, kind(4))           
        elseif (indbmp .eq. 254) then
           call g2_sbytec(cindex, locbms, mypos, INT4_BITS)  ! loc. of bms
        elseif (indbmp .eq. 255) then
           call g2_sbytec(cindex, int(ibskip8 - lskip8, kind(4)), mypos, INT4_BITS)  ! loc. of bms
        endif
        
        ! Copy 6 bytes of the BMS from data buffer to the cindex buffer.
        cindex(lindex + 1:lindex + MXBMS) = cbread(1:MXBMS)
        ! print *, 'bms:', lindex, lindex + MXBMS        
        ! do i=1, MXBMS
        !    print *, i, ichar(cindex(lindex + i))
        ! end do
        lindex = lindex + MXBMS

        ! The size of the index record is now known, so write it to
        ! the cindex buffer.
        call g2_sbytec(cindex, lindex, 0, INT4_BITS)    ! num bytes in index record
        !print '(i3, a8, i5)', 0, ' lindex ', lindex
     elseif (numsec .eq. 7) then                 ! found data section
        ! Write the offset to the data section in the cindex buffer.
        mypos = (IXDS + inc) * INT1_BITS           
        call g2_sbytec(cindex, int(ibskip8 - lskip8, kind(4)), mypos, INT4_BITS)   ! loc. of data sec.
        !print '(i3, a8, i5)', mypos/8, ' locdata ', int(ibskip8 - lskip8, kind(4))

        ! Increment the field count.
        numfld = numfld + 1

        ! Allocate more space in cbuf if necessary. The index record
        ! will be copied there.
        if (lindex + mlen .gt. mbuf) then 
           newsize = max(mbuf + NEXT, mbuf + lindex)
           call realloc(cbuf, mlen, newsize, istat)
           if (istat .ne. 0) then
              numfld = numfld - 1
              iret = 4
              return
           endif
           mbuf = newsize
        endif

        ! Copy the index record into cbuf.
        cbuf(mlen + 1:mlen + lindex) = cindex(1:lindex)
        mlen = mlen + lindex
     else
        ! Unrecognized section.
        iret = 5
        return
     endif

     ! Skip past this section in the data buffer.
     ibskip8 = ibskip8 + lensec
  enddo ! next section
end subroutine ix2gb2

!> Free all memory associated with the library.
!>
!> @param[out] iret integer Return code:
!> - 0 No error.
!> - otherwise Error freeing internal resources.
!> @author Ed Hartnett @date 7/16/23
subroutine gf_finalize(iret)
  implicit none

  integer, intent(out) :: iret  
  character(len = 1), pointer, dimension(:) :: cindex
  integer :: nlen, nnum

  ! Declare interfaces (required for cbuf pointer).
  interface
     subroutine getidx(lugb, lugi, cbuf, nlen, nnum, irgi)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugb, lugi
       integer, intent(out) :: nlen, nnum, irgi
     end subroutine getidx
  end interface

  ! Call getidx with 0 for the first parameter, ensuring that the
  ! internal memory is freed.
  call getidx(0, 0, cindex, nlen, nnum, iret)

end subroutine gf_finalize

