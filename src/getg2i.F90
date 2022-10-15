!> @file
!> @brief This subroutine read a GRIB2 index file and return its
!> content.
!> @author Mark Iredell @date 1995-10-31

!> Read a GRIB2 index file and return its contents.
!>
!> For the format of the index file, see the documentation of
!> ixgb2().
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial.
!> 1996-10-31 | Mark Iredell | Augmented optional definitions to byte 320.
!> 2002-01-03 | Stephen Gilbert | Modified from getgi to work with GRIB2.
!>
!> @param[in] lugi Integer unit of the unblocked GRIB index file. Must
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
