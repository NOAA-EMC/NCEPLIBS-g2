!> @file
!> @brief Subroutines for dealing with indexes.
!> @author Edward Hartnett @date Jan 31, 2024

!> Find, read or generate a GRIB2 index for a GRIB2 file.
!>
!> If the index already exists in library memory, it is returned,
!> otherwise, the index is read from an existing indexfile associated
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
!>
!> @author Stephen Gilbert @date 2005-03-15
subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
  implicit none

  integer, intent(in) :: lugb, lugi
  integer, intent(out) :: nlen, nnum, iret
  character(len = 1), pointer, dimension(:) :: cindex
  integer, parameter :: maxidx = 10000
  integer, parameter :: msk1 = 32000, msk2 = 4000

  integer :: lux
  integer :: irgi, mskp, nmess, i

  type gindex
     integer :: nlen
     integer :: nnum
     character(len = 1), pointer, dimension(:) :: cbuf
  end type gindex

  type(gindex), save :: idxlist(10000)

  data lux/0/

  !  declare interfaces (required for cbuf pointer)
  interface
     subroutine getg2i(lugi, cbuf, nlen, nnum, iret)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugi
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getg2i
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, &
          nmess, iret)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugb, msk1, msk2, mnum
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
  end interface

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

  !  determine whether index buffer needs to be initialized
  lux = 0
  iret = 0
  if (lugb .le. 0 .or. lugb .gt. 9999) then
     print *, ' file unit number out of range'
     print *, ' use unit numbers in range: 0 - 9999 '
     iret = 90
     return
  endif
  if (lugi .eq. lugb) then      ! force regeneration of index from grib2 file
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     !print *, 'Force regeneration'
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = 0
  endif

  if (lugi .lt. 0) then      ! force re-read of index from indexfile
     ! associated with unit abs(lugi)
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     !print *, 'Force re-read'
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = abs(lugi)
  endif

  !  check if index already exists in memory
  if (associated(idxlist(lugb)%cbuf)) then
     !print *, 'Index exists in memory!'
     cindex => idxlist(lugb)%cbuf
     nlen = idxlist(lugb)%nlen
     nnum = idxlist(lugb)%nnum
     return
  endif

  irgi = 0
  if (lux .gt. 0) then
     call getg2i(lux, idxlist(lugb)%cbuf, nlen, nnum, irgi)
  elseif (lux .le. 0) then
     mskp = 0
     call getg2ir(lugb, msk1, msk2, mskp, idxlist(lugb)%cbuf, &
          nlen, nnum, nmess, irgi)
  endif
  if (irgi .eq. 0) then
     cindex => idxlist(lugb)%cbuf
     idxlist(lugb)%nlen = nlen
     idxlist(lugb)%nnum = nnum
  else
     nlen = 0
     nnum = 0
     print *, ' error reading index file '
     iret = 96
     return
  endif
end subroutine getidx

!> Read a GRIB2 index file and return its contents.
!>
!> The index file may be generated by the grb2index utility of the
!> [NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util)
!> project.
!>
!> The index file has two header records:
!> 1. an 81-byte header with 'GB2IX1' in columns 42-47
!> 2. an 81-byte header with number of bytes to skip before index
!> records, total length in bytes of the index records, number of
!> index records, and GRIB file basename written in format
!> ('IX1FORM:',3i10,2x,a40).
!>
!> Each record in the index table contains the following fields. All
!> integers are in big-endian format in the file.
!>
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
!> @param[out] nlen Total length of all index records.
!> @param[out] nnum Number of index records.
!> @param[out] iret Return code.
!> - 0 No error.
!> - 2 not enough memory to hold index buffer
!> - 3 error reading index file buffer
!> - 4 error reading index file header
!>
!> @author Mark Iredell @date 2000-05-26
SUBROUTINE GETG2I(LUGI, CBUF, NLEN, NNUM, IRET)
  implicit none
  
  CHARACTER(LEN=1), POINTER, DIMENSION(:) :: CBUF
  INTEGER, INTENT(IN) :: LUGI
  INTEGER, INTENT(OUT) :: NLEN, NNUM, IRET
  CHARACTER CHEAD*162
  integer :: ios, istat, lbuf, lhead, nskp
  integer :: index_version

  NULLIFY(CBUF)
  NLEN = 0
  NNUM = 0
  IRET = 4
  CALL BAREAD(LUGI, 0, 162, LHEAD, CHEAD)
  IF (LHEAD .EQ. 162 .AND. CHEAD(42:47) .EQ. 'GB2IX1') THEN
     READ(CHEAD(82:162), '(2X, I1, 5X, 3I10, 2X, A40)', IOSTAT = IOS) index_version, NSKP, NLEN, NNUM
     IF (IOS .EQ. 0) THEN
        ALLOCATE(CBUF(NLEN), STAT = ISTAT)    ! ALLOCATE SPACE FOR CBUF
        IF (ISTAT .NE. 0) THEN
           IRET = 2
           RETURN
        ENDIF
        IRET = 0
        CALL BAREAD(LUGI, NSKP, NLEN, LBUF, CBUF)
        IF (LBUF .NE. NLEN) IRET = 3
     ENDIF
  ENDIF
END SUBROUTINE GETG2I

!> Generate an index record for a message in a GRIB2 file.
!>
!> The index record contains byte offsets to the message, it's length,
!> and byte offsets within the message to each section. The index file
!> record format is documented in subroutine ixgb2().
!>
!> This is the legacy function, which always creates version 1 index
!> files. To create index files of version 1 or 2, use getg2i2r().
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
!> @author Mark Iredell @date 1995-10-31
subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, nmess, iret)
  use re_alloc              ! needed for subroutine realloc
  implicit none
  character(len = 1), pointer, dimension(:) :: cbuf
  integer, intent(in) :: lugb, msk1, msk2, mnum
  integer, intent(out) :: nlen, nnum, nmess, iret
  
  interface      ! required for cbuf pointer
     subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, nlen, &
          nnum, nmess, iret)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugb, msk1, msk2, mnum, idxver
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2i2r
  end interface

  call getg2i2r(lugb, msk1, msk2, mnum, 1, cbuf, nlen, nnum, nmess, iret)
end subroutine getg2ir

!> Generate an index record for a message in a GRIB2 file.
!>
!> The index record contains byte offsets to the message, it's length,
!> and byte offsets within the message to each section. The index file
!> record format is documented in subroutine ixgb2().
!>
!> This subroutine is like getg2ir(), but this subroutine can generate
!> index files in version 1 or 2 format. Use index version 2 for files
!> > 2 GB.
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> @param[in] lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] msk18 Number of bytes to search for first message.
!> @param[in] msk28 Number of bytes to search for other messages.
!> @param[in] mnum Number of GRIB messages to skip (usually 0).
!> @param[in] idxver Index version, 1 for legacy, 2 for files > 2 GB.
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
!> @author Mark Iredell, Edward Hartnett @date Feb 4, 2024
subroutine getg2i2r(lugb, msk18, msk28, mnum, idxver, cbuf, nlen, &
     nnum, nmess, iret)
  use re_alloc              ! needed for subroutine realloc
  implicit none

  character(len = 1), pointer, dimension(:) :: cbuf
  integer, intent(in) :: lugb
  integer (kind = 8), intent(in) :: msk18, msk28
  integer :: msk1, msk2
  integer, intent(in) :: mnum, idxver
  integer, intent(out) :: nlen, nnum, nmess, iret
  character(len = 1), pointer, dimension(:) :: cbuftmp
  integer :: nbytes, newsize, next, numfld, m, mbuf, lskip, lgrib
  integer :: istat, iseek, init, iret1
  parameter(init = 50000, next = 10000)

  interface      ! required for cbuf pointer
     subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
       integer :: lugb, lskip, lgrib
       character(len = 1), pointer, dimension(:) :: cbuf
       integer :: numfld, mlen, iret
     end subroutine ixgb2
  end interface

  msk1 = msk18
  msk2 = msk28

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
  iseek = 0
  call skgb(lugb, iseek, msk1, lskip, lgrib)
  do m = 1, mnum
     if(lgrib.gt.0) then
        iseek = lskip + lgrib
        call skgb(lugb, iseek, msk2, lskip, lgrib)
     endif
  enddo

  ! Get index records for every grib message found.
  NLEN = 0
  NNUM = 0
  NMESS = MNUM
  DO WHILE(IRET .EQ. 0 .AND. LGRIB .GT. 0)
     CALL IXGB2(LUGB, LSKIP, LGRIB, CBUFTMP, NUMFLD, NBYTES, IRET1)
     IF (IRET1 .NE. 0) PRINT *, ' SAGT ', NUMFLD, NBYTES, IRET1
     IF((NBYTES + NLEN) .GT. MBUF) THEN             ! Allocate more space, if necessary.
        NEWSIZE = MAX(MBUF + NEXT, MBUF + NBYTES)
        CALL REALLOC(CBUF, NLEN, NEWSIZE, ISTAT)
        IF (ISTAT .NE. 0) THEN
           IRET = 1
           RETURN
        ENDIF
        MBUF = NEWSIZE
     ENDIF

     ! If index records were returned in cbuftmp from ixgb2, 
     ! copy cbuftmp into cbuf, then deallocate cbuftmp when done.
     IF (ASSOCIATED(CBUFTMP)) THEN
        CBUF(NLEN + 1 : NLEN + NBYTES) = CBUFTMP(1 : NBYTES)
        DEALLOCATE(CBUFTMP, STAT = ISTAT)
        IF (ISTAT.NE.0) THEN
           PRINT *, ' deallocating cbuftmp ... ', istat
           IRET = 3
           RETURN
        ENDIF
        NULLIFY(CBUFTMP)
        NNUM = NNUM + NUMFLD
        NLEN = NLEN + NBYTES
        NMESS = NMESS + 1
     ENDIF

     ! Look for next grib message.
     ISEEK = LSKIP + LGRIB
     CALL SKGB(LUGB, ISEEK, MSK2, LSKIP, LGRIB)
  ENDDO
END SUBROUTINE GETG2I2R

!> Find information about a GRIB field from the index and fill a @ref
!> grib_mod::gribfield.
!>
!> For a description of the index record see getg2i().
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
!> @author Stephen Gilbert @date 2002-01-15
SUBROUTINE GETGB2S(CBUF, NLEN, NNUM, J, JDISC, JIDS, JPDTN, JPDT, JGDTN, &
     JGDT, K, GFLD, LPOS, IRET)
  USE GRIB_MOD
  implicit none

  CHARACTER(LEN = 1), INTENT(IN) :: CBUF(NLEN)
  INTEGER, INTENT(IN) :: NLEN, NNUM, J, JDISC, JPDTN, JGDTN
  INTEGER, DIMENSION(:) :: JIDS(*), JPDT(*), JGDT(*)
  INTEGER, INTENT(OUT) :: K, LPOS, IRET
  TYPE(GRIBFIELD), INTENT(OUT) :: GFLD

  INTEGER :: KGDS(5)
  LOGICAL :: MATCH1, MATCH3, MATCH4
  integer :: i, icnd, inlen, iof, ipos, jpos, lsec1, lsec3, lsec4, lsec5, numgdt, numpdt

  interface
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

  !     INITIALIZE
  K = 0
  LPOS = 0
  IRET = 1
  IPOS = 0
  nullify(gfld%idsect, gfld%local)
  nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
  nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)

  !     SEARCH FOR REQUEST
  DO WHILE(IRET.NE.0 .and. K.LT.NNUM)
     K = K + 1
     CALL G2_GBYTEC(CBUF, INLEN, IPOS * 8, 4 * 8)    ! GET LENGTH OF CURRENT
     ! INDEX RECORD
     IF (K.LE.J) THEN           ! SKIP THIS INDEX
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF GRIB2 DISCIPLINE IS A MATCH
     CALL G2_GBYTEC(CBUF, GFLD%DISCIPLINE, (IPOS + 41)*8, 1*8)
     IF ((JDISC.NE.-1) .and. (JDISC.NE.GFLD%DISCIPLINE)) THEN
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF IDENTIFICATION SECTION IS A MATCH
     MATCH1 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC1, (IPOS + 44) * 8, 4 * 8)  ! GET LENGTH OF IDS 
     IOF = 0
     CALL GF_UNPACK1(CBUF(IPOS + 45), LSEC1, IOF, GFLD%IDSECT, GFLD%IDSECTLEN, ICND)
     IF (ICND .eq. 0) THEN
        MATCH1 = .TRUE.
        DO I = 1, GFLD%IDSECTLEN
           IF ((JIDS(I).NE.-9999) .and.  (JIDS(I).NE.GFLD%IDSECT(I))) THEN
              MATCH1 = .FALSE.
              EXIT
           ENDIF
        ENDDO
     ENDIF
     IF (.NOT. MATCH1) THEN
        DEALLOCATE(GFLD%IDSECT)
        IPOS = IPOS + INLEN
        CYCLE
     ENDIF

     !     CHECK IF GRID DEFINITION TEMPLATE IS A MATCH
     JPOS = IPOS + 44 + LSEC1
     MATCH3 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC3, JPOS * 8, 4 * 8)  ! GET LENGTH OF GDS 
     IF (JGDTN .eq. -1) THEN
        MATCH3 = .TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF, NUMGDT, (JPOS + 12) * 8, 2 * 8)  ! GET GDT TEMPLATE NO.
        IF (JGDTN .eq. NUMGDT) THEN
           IOF = 0
           CALL GF_UNPACK3(CBUF(JPOS + 1), LSEC3, IOF, KGDS, GFLD%IGDTMPL, &
                GFLD%IGDTLEN, GFLD%LIST_OPT, GFLD%NUM_OPT, ICND)
           IF (ICND .eq. 0) THEN
              MATCH3 = .TRUE.
              DO I = 1, GFLD%IGDTLEN
                 IF ((JGDT(I).NE.-9999) .and.  (JGDT(I).NE.GFLD%IGDTMPL(I))) THEN
                    MATCH3 = .FALSE.
                    EXIT
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     IF (.NOT. MATCH3) THEN
        IF (ASSOCIATED(GFLD%IDSECT)) DEALLOCATE(GFLD%IDSECT)
        IF (ASSOCIATED(GFLD%IGDTMPL)) DEALLOCATE(GFLD%IGDTMPL)
        IF (ASSOCIATED(GFLD%LIST_OPT)) DEALLOCATE(GFLD%LIST_OPT)
        IPOS = IPOS + INLEN
        CYCLE
     ELSE
        GFLD%GRIDDEF = KGDS(1)
        GFLD%NGRDPTS = KGDS(2)
        GFLD%NUMOCT_OPT = KGDS(3)
        GFLD%INTERP_OPT = KGDS(4)
        GFLD%IGDTNUM = KGDS(5)
     ENDIF

     !     CHECK IF PRODUCT DEFINITION TEMPLATE IS A MATCH
     JPOS = JPOS + LSEC3
     MATCH4 = .FALSE.
     CALL G2_GBYTEC(CBUF, LSEC4, JPOS * 8, 4 * 8)  ! GET LENGTH OF PDS 
     IF (JPDTN .eq. -1) THEN
        MATCH4 = .TRUE.
     ELSE
        CALL G2_GBYTEC(CBUF, NUMPDT, (JPOS + 7) * 8, 2 * 8)  ! GET PDT TEMPLATE NO.
        IF (JPDTN .eq. NUMPDT) THEN
           IOF = 0
           CALL GF_UNPACK4(CBUF(JPOS + 1), LSEC4, IOF, GFLD%IPDTNUM, &
                GFLD%IPDTMPL, GFLD%IPDTLEN, GFLD%COORD_LIST, GFLD%NUM_COORD, ICND)
           IF (ICND .eq. 0) THEN
              MATCH4 = .TRUE.
              DO I = 1, GFLD%IPDTLEN
                 IF ((JPDT(I).NE.-9999) .and.  (JPDT(I).NE.GFLD%IPDTMPL(I))) THEN
                    MATCH4 = .FALSE.
                    EXIT
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     IF (.NOT. MATCH4) THEN
        IF (ASSOCIATED(GFLD%IDSECT)) DEALLOCATE(GFLD%IDSECT)
        IF (ASSOCIATED(GFLD%IPDTMPL)) DEALLOCATE(GFLD%IPDTMPL)
        IF (ASSOCIATED(GFLD%COORD_LIST)) DEALLOCATE(GFLD%COORD_LIST)
     ENDIF

     !     IF REQUEST IS FOUND
     !     SET VALUES FOR DERIVED TYPE GFLD AND RETURN
     IF(MATCH1 .and. MATCH3 .and. MATCH4) THEN
        LPOS = IPOS + 1
        CALL G2_GBYTEC(CBUF, GFLD%VERSION, (IPOS + 40) * 8, 1 * 8)
        CALL G2_GBYTEC(CBUF, GFLD%IFLDNUM, (IPOS + 42) * 8, 2 * 8)
        GFLD%UNPACKED = .FALSE.
        JPOS = IPOS + 44 + LSEC1
        IF (JGDTN.EQ.-1) THEN     ! UNPACK GDS, IF NOT DONE BEFORE
           IOF = 0
           CALL GF_UNPACK3(CBUF(JPOS + 1), LSEC3, IOF, KGDS, GFLD%IGDTMPL, &
                GFLD%IGDTLEN, GFLD%LIST_OPT, GFLD%NUM_OPT, ICND)
           GFLD%GRIDDEF = KGDS(1)
           GFLD%NGRDPTS = KGDS(2)
           GFLD%NUMOCT_OPT = KGDS(3)
           GFLD%INTERP_OPT = KGDS(4)
           GFLD%IGDTNUM = KGDS(5)
        ENDIF
        JPOS = JPOS + LSEC3
        IF (JPDTN.EQ.-1 ) THEN     ! UNPACK PDS, IF NOT DONE BEFORE
           IOF = 0
           CALL GF_UNPACK4(CBUF(JPOS + 1), LSEC4, IOF, GFLD%IPDTNUM, &
                GFLD%IPDTMPL, GFLD%IPDTLEN, GFLD%COORD_LIST, GFLD%NUM_COORD, ICND)
        ENDIF
        JPOS = JPOS + LSEC4
        CALL G2_GBYTEC(CBUF, LSEC5, JPOS * 8, 4 * 8)  ! GET LENGTH OF DRS 
        IOF = 0
        CALL GF_UNPACK5(CBUF(JPOS + 1), LSEC5, IOF, GFLD%NDPTS, &
             GFLD%IDRTNUM, GFLD%IDRTMPL, GFLD%IDRTLEN, ICND)
        JPOS = JPOS + LSEC5
        CALL G2_GBYTEC(CBUF, GFLD%IBMAP, (JPOS + 5)*8, 1 * 8)  ! GET IBMAP
        IRET = 0
     ELSE      ! PDT DID NOT MATCH
        IPOS = IPOS+INLEN
     ENDIF
  ENDDO
END SUBROUTINE GETGB2S

!> Generate an index record for each field in a GRIB2 message. The index
!> records are written to index buffer pointed to by cbuf. All integers
!> in the index are in big-endian format.
!>
!> This subroutine is called by getg2ir(), which packages the index
!> records into an index file.
!>
!> The index buffer returned contains index records with the
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
!> - byte ii + 1-  jj grid definition section (gds)
!> - byte jj + 1-  kk product definition section (pds)
!> - byte kk + 1-  ll the data representation section (drs)
!> - byte ll + 1-ll + 6 first 6 bytes of the bit map section (bms)
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
!> @author Mark Iredell @date 1995-10-31
subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)
  implicit none
  integer :: lugb, lskip, lgrib
  character(len = 1), pointer, dimension(:) :: cbuf
  integer :: numfld, mlen, iret
  integer (kind = 8) :: lskip8

  interface
     subroutine ix2gb2(lugb, lskip8, idxver, lgrib, cbuf, numfld, mlen, iret)
       integer :: lugb
       integer (kind = 8) :: lskip8
       integer :: idxver, lgrib
       character(len = 1), pointer, dimension(:) :: cbuf
       integer :: numfld, mlen, iret
     end subroutine ix2gb2
  end interface

  lskip8 = lskip
  call ix2gb2(lugb, lskip8, 1, lgrib, cbuf, numfld, mlen, iret)
end SUBROUTINE IXGB2

!> Generate an index record for each field in a GRIB2 message. The index
!> records are written to index buffer pointed to by cbuf. All integers
!> in the index are in big-endian format.
!>
!> This subroutine is called by getg2ir(), which packages the index
!> records into an index file.
!>
!> The index buffer returned contains index records with the
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
!> - byte ii + 1-  jj grid definition section (gds)
!> - byte jj + 1-  kk product definition section (pds)
!> - byte kk + 1-  ll the data representation section (drs)
!> - byte ll + 1-ll + 6 first 6 bytes of the bit map section (bms)
!>
!> @param lugb Unit of the unblocked GRIB file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param lskip8 Number of bytes to skip before GRIB message.
!> @param idxver Index version, 1 for legacy, 2 for GRIB2 files > 2 GB.
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
!> @author Mark Iredell @date 1995-10-31
subroutine ix2gb2(lugb, lskip8, idxver, lgrib, cbuf, numfld, mlen, iret)
  use re_alloc              ! needed for subroutine realloc
  implicit none

  integer :: lugb
  integer (kind = 8) :: lskip8
  integer :: idxver, lgrib
  CHARACTER(LEN = 1), POINTER, DIMENSION(:) :: CBUF
  integer :: numfld, mlen, iret
  integer :: lskip
  
  CHARACTER CVER, CDISC
  CHARACTER(LEN = 4) :: CTEMP
  INTEGER LOCLUS, LOCGDS, LENGDS, LOCBMS
  integer :: indbmp, numsec, next, newsize, mova2i, mbuf, lindex
  integer :: linmax, ixskp
  integer :: mxspd, mxskp, mxsgd, mxsdr, mxsbm, mxlus
  integer :: mxlen, mxds, mxfld, mxbms
  integer :: init, ixlus
  integer :: ixsgd, ibread, ibskip, ilndrs, ilnpds, istat, ixds
  integer :: ixspd, ixfld, ixids, ixlen, ixsbm, ixsdr
  integer :: lbread, lensec, lensec1
  PARAMETER(LINMAX = 5000, INIT = 50000, NEXT = 10000)
  PARAMETER(IXSKP = 4, IXLUS = 8, IXSGD = 12, IXSPD = 16, IXSDR = 20, IXSBM = 24, &
       IXDS = 28, IXLEN = 36, IXFLD = 42, IXIDS = 44)
  PARAMETER(MXSKP = 4, MXLUS = 4, MXSGD = 4, MXSPD = 4, MXSDR = 4, MXSBM = 4, &
       MXDS = 4, MXLEN = 4, MXFLD = 2, MXBMS = 6)
  CHARACTER CBREAD(LINMAX), CINDEX(LINMAX)
  CHARACTER CIDS(LINMAX), CGDS(LINMAX)

  lskip = lskip8
  LOCLUS = 0
  IRET = 0
  MLEN = 0
  NUMFLD = 0
  NULLIFY(CBUF)
  MBUF = INIT
  ALLOCATE(CBUF(MBUF), STAT = ISTAT)    ! ALLOCATE INITIAL SPACE FOR CBUF
  IF (ISTAT .NE. 0) THEN
     IRET = 1
     RETURN
  ENDIF

  ! Read sections 0 and 1 for versin number and discipline.
  IBREAD = MIN(LGRIB, LINMAX)
  CALL BAREAD(LUGB, LSKIP, IBREAD, LBREAD, CBREAD)
  IF(LBREAD .NE. IBREAD) THEN
     IRET = 2
     RETURN
  ENDIF
  IF(CBREAD(8) .NE. CHAR(2)) THEN          !  NOT GRIB EDITION 2
     IRET = 3
     RETURN
  ENDIF
  CVER = CBREAD(8)
  CDISC = CBREAD(7)
  CALL G2_GBYTEC(CBREAD, LENSEC1, 16 * 8, 4 * 8)
  LENSEC1 = MIN(LENSEC1, IBREAD)
  CIDS(1:LENSEC1) = CBREAD(17:16 + LENSEC1)
  IBSKIP = LSKIP + 16 + LENSEC1

  ! Loop through remaining sections creating an index for each field.
  IBREAD = MAX(5, MXBMS)
  DO
     CALL BAREAD(LUGB, IBSKIP, IBREAD, LBREAD, CBREAD)
     CTEMP = CBREAD(1)//CBREAD(2)//CBREAD(3)//CBREAD(4)
     IF (CTEMP .EQ. '7777') RETURN        ! END OF MESSAGE FOUND
     IF(LBREAD .NE. IBREAD) THEN
        IRET = 2
        RETURN
     ENDIF
     CALL G2_GBYTEC(CBREAD, LENSEC, 0 * 8, 4 * 8)
     CALL G2_GBYTEC(CBREAD, NUMSEC, 4 * 8, 1 * 8)

     IF (NUMSEC .EQ. 2) THEN                 ! SAVE LOCAL USE LOCATION
        LOCLUS = IBSKIP-LSKIP
     ELSEIF (NUMSEC .EQ. 3) THEN                 ! SAVE GDS INFO
        LENGDS = LENSEC
        CGDS = CHAR(0)
        CALL BAREAD(LUGB, IBSKIP, LENGDS, LBREAD, CGDS)
        IF (LBREAD .NE. LENGDS) THEN
           IRET = 2
           RETURN
        ENDIF
        LOCGDS = IBSKIP-LSKIP
     ELSEIF (NUMSEC .EQ. 4) THEN                 ! FOUND PDS
        CINDEX = CHAR(0)
        CALL G2_SBYTEC(CINDEX, LSKIP, 8 * IXSKP, 8 * MXSKP)    ! BYTES TO SKIP
        CALL G2_SBYTEC(CINDEX, LOCLUS, 8 * IXLUS, 8 * MXLUS)   ! LOCATION OF LOCAL USE
        CALL G2_SBYTEC(CINDEX, LOCGDS, 8 * IXSGD, 8 * MXSGD)   ! LOCATION OF GDS
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSPD, 8 * MXSPD)  ! LOCATION OF PDS
        CALL G2_SBYTEC(CINDEX, LGRIB, 8 * IXLEN, 8 * MXLEN)    ! LEN OF GRIB2
        CINDEX(41) = CVER
        CINDEX(42) = CDISC
        CALL G2_SBYTEC(CINDEX, NUMFLD + 1, 8 * IXFLD, 8 * MXFLD)   ! FIELD NUM
        CINDEX(IXIDS + 1:IXIDS + LENSEC1) = CIDS(1:LENSEC1)
        LINDEX = IXIDS + LENSEC1
        CINDEX(LINDEX + 1:LINDEX + LENGDS) = CGDS(1:LENGDS)
        LINDEX = LINDEX + LENGDS
        ILNPDS = LENSEC
        CALL BAREAD(LUGB, IBSKIP, ILNPDS, LBREAD, CINDEX(LINDEX + 1))
        IF (LBREAD .NE. ILNPDS) THEN
           IRET = 2
           RETURN
        ENDIF
        LINDEX = LINDEX + ILNPDS
     ELSEIF (NUMSEC .EQ. 5) THEN                 ! FOUND DRS
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSDR, 8 * MXSDR)  ! LOCATION OF DRS
        ILNDRS = LENSEC
        CALL BAREAD(LUGB, IBSKIP, ILNDRS, LBREAD, CINDEX(LINDEX + 1))
        IF (LBREAD .NE. ILNDRS) THEN
           IRET = 2
           RETURN
        ENDIF
        LINDEX = LINDEX + ILNDRS
     ELSEIF (NUMSEC .EQ. 6) THEN                 ! FOUND BMS
        INDBMP = MOVA2I(CBREAD(6))
        IF (INDBMP.LT.254) THEN
           LOCBMS = IBSKIP-LSKIP
           CALL G2_SBYTEC(CINDEX, LOCBMS, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ELSEIF (INDBMP.EQ.254) THEN
           CALL G2_SBYTEC(CINDEX, LOCBMS, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ELSEIF (INDBMP.EQ.255) THEN
           CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXSBM, 8 * MXSBM)  ! LOC. OF BMS
        ENDIF
        CINDEX(LINDEX + 1:LINDEX + MXBMS) = CBREAD(1:MXBMS)
        LINDEX = LINDEX + MXBMS
        CALL G2_SBYTEC(CINDEX, LINDEX, 0, 8 * 4)    ! NUM BYTES IN INDEX RECORD
     ELSEIF (NUMSEC .EQ. 7) THEN                 ! FOUND DATA SECTION
        CALL G2_SBYTEC(CINDEX, IBSKIP-LSKIP, 8 * IXDS, 8 * MXDS)   ! LOC. OF DATA SEC.
        NUMFLD = NUMFLD + 1
        IF ((LINDEX + MLEN) .GT. MBUF) THEN ! ALLOCATE MORE SPACE IF NECESSARY
           NEWSIZE = MAX(MBUF + NEXT, MBUF + LINDEX)
           CALL REALLOC(CBUF, MLEN, NEWSIZE, ISTAT)
           IF (ISTAT .NE. 0) THEN
              NUMFLD = NUMFLD-1
              IRET = 4
              RETURN
           ENDIF
           MBUF = NEWSIZE
        ENDIF
        CBUF(MLEN + 1:MLEN + LINDEX) = CINDEX(1:LINDEX)
        MLEN = MLEN + LINDEX
     ELSE                           ! UNRECOGNIZED SECTION
        IRET = 5
        RETURN
     ENDIF
     IBSKIP = IBSKIP + LENSEC
  ENDDO
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
     subroutine getidx(lugb,lugi,cbuf,nlen,nnum,irgi)
       character(len=1),pointer,dimension(:) :: cbuf
       integer,intent(in) :: lugb,lugi
       integer,intent(out) :: nlen,nnum,irgi
     end subroutine getidx
  end interface

  ! Call getidx with 0 for the first parameter, ensuring that the
  ! internal memory is freed.
  call getidx(0, 0, cindex, nlen, nnum, iret)

end subroutine gf_finalize

