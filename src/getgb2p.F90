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
!> @param[in] lugb Unit of the unblocked grib data file. The
!> file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] lugi Unit of the unblocked grib index file. If
!> nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> subroutine. Set to 0 to get index buffer from the grib file.
!> @param[in] J Number of fields to skip (set to 0 to search
!> from beginning).
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
!> (if = -1, don't bother matching PDT - accept any)
!> @param[in] jpdt Array of values defining the Product Definition
!> Template 4.n of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template number (if = -1, don't
!> bother matching gdt - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] extract value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. return grib2 message containing only the requested field.
!> - .false. return entire grib2 message containing the requested field.
!> @param[out] k field number unpacked.
!> @param[out] gribm returned grib message.
!> @param[out] leng length of returned grib message in bytes.
!> @param[out] iret integer return code
!> - 0 No error.
!> - 96 Error reading index.
!> - 97 Error reading grib file.
!> - 99 Request not found.
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

  ! Determine whether index buffer needs to be initialized.
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

  ! Search index buffer.
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       jk, gfld, lpos, irgs)
  if (irgs .ne. 0) then
     iret = 99
     call gf_free(gfld)
     return
  endif

  ! Extract grib message from file.
  call getgb2rp(lugb, cbuf(lpos:), extract, gribm, leng, iret)

  k = jk
  call gf_free(gfld)
end subroutine getgb2p
