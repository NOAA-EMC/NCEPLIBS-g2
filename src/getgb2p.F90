!> @file
!> @brief This subroutine finds and extracts a GRIB2 message from a
!> file.
!> @author Mark Iredell @date 1994-04-01

!> This subroutine finds and extracts a GRIB2 message from a file.
!>
!> This subroutine reads a grib index file (or optionally the grib
!> file itself) to get the index buffer (i.e. table of contents) for
!> the grib file.  It finds in the index buffer a reference to the
!> grib field requested.
!>
!> The grib field request specifies the number of fields to skip and
!> the unpacked identification section, grid definition template and
!> product defintion section parameters. (A requested parameter of
!> -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested grib field is found, then it is read from the grib
!> file and unpacked. If the grib field is not found, then the return
!> code will be nonzero.
!>
!> Note that derived type @ref grib_mod::gribfield contains pointers
!> to many arrays of data. The memory for these arrays is allocated
!> when the values in the arrays are set, to help minimize problems
!> with array overloading. Because of this users are should free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1994-04-01 | Mark Iredell | Initial
!> 1995-10-31 | Mark Iredell | modularized portions of code into subprograms and allowed for unspecified index file
!> 2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with grib2
!> 2003-12-17 | Stephen Gilbert | modified from getgb2 to return packed grib2 message
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] LUGI integer unit of the unblocked grib index file.  if
!> nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine. (set to 0 to get index buffer from the grib file),
!> @param[in] J integer number of fields to skip (set to 0 to search
!> from beginning)
!> @param[in] JDISC grib2 discipline number of requested field (if =
!> -1, accept any discipline see code table 0.0)
!> - 0 meteorological products
!> - 1 hydrological products
!> - 2 land surface products
!> - 3 space products
!> - 10 oceanographic products
!> @param[in] JIDS integer array of values in the identification
!> section (set to -9999 for wildcard).
!> - JIDS(1) identification of originating centre
!> (see common code table c-1)
!> - JIDS(2) identification of originating sub-centre
!> - JIDS(3) grib master tables version number
!> (see code table 1.0) 0 experimental;1 initial operational version number.
!> - JIDS(4) grib local tables version number (see code table 1.1)
!> 0 local tables not used; 1-254 number of local tables version used.
!> - JIDS(5) significance of reference time (code table 1.2)
!> 0 analysis; 1 start of forecast; 2 verifying time of forecast; 3 observation time
!> - JIDS(6) year (4 digits)
!> - JIDS(7) month
!> - JIDS(8) day
!> - JIDS(9) hour
!> - JIDS(10) minute
!> - JIDS(11) second
!> - JIDS(12) production status of processed data (see code table 1.3)
!> 0 operational products; 1 operational test products;
!> 2 research products; 3 re-analysis products.
!> - JIDS(13) type of processed data (see code table 1.4)
!> 0 analysis products; 1 forecast products; 2 analysis and forecast
!> products; 3 control forecast products; 4 perturbed forecast products;
!> 5 control and perturbed forecast products; 6 processed satellite
!> observations; 7 processed radar observations.
!> @param[in] JPDTN integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] JGDTN integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[in] EXTRACT logical value indicating whether to return a
!> grib2 message with just the requested field, or the entire
!> grib2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] K integer field number unpacked.
!> @param[out] GRIBM returned grib message.
!> @param[out] LENG length of returned grib message in bytes.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!>
!> @note Specify an index file if feasible to increase speed.
!> Do not engage the same logical unit from more than one processor.
!>
!> @author Mark Iredell @date 1994-04-01
subroutine getgb2p(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
     extract, k, gribm, leng, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, lugi, j, jdisc, jpdtn, jgdtn
  integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
  logical, intent(in) :: extract
  integer, intent(out) :: k, iret, leng
  character(len = 1), pointer, dimension(:) :: gribm

  type(gribfield) :: gfld
  integer :: msk1, irgi, irgs, jk, lpos, lux, msk2, mskp, nlen, nmess, nnum

  character(len = 1), pointer, dimension(:) :: cbuf
  parameter(msk1 = 32000, msk2 = 4000)

  save cbuf, nlen, nnum
  data lux/0/

  !  declare interfaces (required for cbuf pointer)
  interface
     subroutine getg2i(lugi, cbuf, nlen, nnum, iret)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugi
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getg2i
     subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum,  &
          nmess, iret)
       character(len = 1), pointer, dimension(:) :: cbuf
       integer, intent(in) :: lugb, msk1, msk2, mnum
       integer, intent(out) :: nlen, nnum, nmess, iret
     end subroutine getg2ir
     subroutine getgb2rp(lugb, cindex, extract, gribm, leng, iret)
       integer, intent(in) :: lugb
       character(len = 1), intent(in) :: cindex(*)
       logical, intent(in) :: extract
       integer, intent(out) :: leng, iret
       character(len = 1), pointer, dimension(:) :: gribm
     end subroutine getgb2rp
  end interface

  !  determine whether index buffer needs to be initialized
  irgi = 0
  if (lugi .gt. 0 .and. lugi .ne. lux) then
     call getg2i(lugi, cbuf, nlen, nnum, irgi)
     lux = lugi
  elseif (lugi .le. 0 .and. lugb .ne. lux) then
     mskp = 0
     call getg2ir(lugb, msk1, msk2, mskp, cbuf, nlen, nnum, nmess, irgi)
     lux = lugb
  endif
  if (irgi .gt. 1) then
     iret = 96
     lux = 0
     return
  endif

  !  search index buffer
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       jk, gfld, lpos, irgs)
  if (irgs .ne. 0) then
     iret = 99
     call gf_free(gfld)
     return
  endif

  !  extract grib message from file
  call getgb2rp(lugb, cbuf(lpos:), extract, gribm, leng, iret)

  k = jk
  call gf_free(gfld)
end subroutine getgb2p
