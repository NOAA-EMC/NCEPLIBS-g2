!> @file
!> @brief Find, read or generate a GRIB2 index for
!> the GRIB2 file associated with unit lugb.
!> @author Stephen Gilbert @date 2005-03-15

!> Find, read or generate a GRIB2 index for
!> the GRIB2 file associated with unit lugb. If the index already
!> exists, it is returned, otherwise, the index is (1) read from an
!> existing indexfile associated with unit lugi or (2) generated
!> from the GRIB2 file lugb.
!>
!> Users can force a regeneration of an index: if lugi equals lugb,
!> the index will be regenerated from the data in file lugb. If lugi
!> is less than zero, then the index is re-read from index file
!> abs(lugi).
!>
!> @note The file unit numbers must be in range 0 - 9999.
!>
!> @param[in] lugb integer unit of the GRIB2 data file.
!> File must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] lugi integer unit of the GRIB2 index file.
!> If nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. Set to 0 to get index information from the GRIB2 file.
!> @param[out] cindex character*1 Pointer to a buffer that will get
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
  integer :: irgi, mskp, nmess

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

  !  determine whether index buffer needs to be initialized
  lux = 0
  iret = 0
  if (lugb .le. 0 .or. lugb .gt. 9999) then
     print *, ' '
     print *, ' file unit number out of range'
     print *, ' use unit numbers in range: 0 - 9999 '
     print *, ' '
     iret = 90
     return
  endif
  if (lugi .eq. lugb) then      ! force regeneration of index from grib2 file
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = 0
  endif

  if (lugi .lt. 0) then      ! force re-read of index from indexfile
     ! associated with unit abs(lugi)
     if (associated(idxlist(lugb)%cbuf))  &
          deallocate(idxlist(lugb)%cbuf)
     nullify(idxlist(lugb)%cbuf)
     idxlist(lugb)%nlen = 0
     idxlist(lugb)%nnum = 0
     lux = abs(lugi)
  endif

  !  check if index already exists in memory
  if (associated(idxlist(lugb)%cbuf)) then
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
     print *, ' '
     print *, ' error reading index file '
     print *, ' '
     iret = 96
     return
  endif
end subroutine getidx
