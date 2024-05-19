!> @file
!> @brief The getgb2 subroutines find and unpack a GRIB2 message in a
!> file.
!> @author Ed Hartnett @date Mar 6, 2024

!> This is a legacy version of getgb2i2(). It finds and unpacks a
!> GRIB2 message in a file, using an version 1 index record which is
!> either found in memory, read from an index file, or generated.
!>
!> Users of this routine must:
!> 1. include a use grib_mod
!> 2. call gf_free() on the @ref grib_mod::gribfield parameter
!> 3. free library memory with gf_finalize()
!>
!> For more details, see getgb2i2().
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] lugi integer unit of the unblocked grib index file.
!> If nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. lugi may be:
!> - > 0 read index from index file lugi, if index doesn"t already exist.
!> - = 0 to get index buffer from the grib file, if index
!> doesn"t already exist.
!> - < 0 force reread of index from index file abs(lugi).
!> - = lugb force regeneration of index from GRIB2 file lugb.
!> @param[in] j integer number of fields to skip (0 to search from
!> beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field:
!> --1 accept any discipline
!> - 0 meteorological products
!> - 1 hydrological products
!> - 2 land surface products
!> - 3 space products
!> - 10 oceanographic products
!> @param[in] JIDS integer array of values in the identification section
!> (=-9999 for wildcard).
!> @param[in] jpdtn integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] jgdtn integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[in] unpack logical value indicating whether to unpack bitmap/data
!> - .TRUE. unpack bitmap and data values
!> - .FALSE. do not unpack bitmap and data values
!> @param[out] k integer field number unpacked
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Mark Iredell, Ed Hartnett @date 1994-04-01
subroutine getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
     unpack, k, gfld, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, lugi, j, jdisc
  integer, dimension(:) :: jids(*)
  integer, intent(in) :: jpdtn
  integer, dimension(:) :: jpdt(*)
  integer, intent(in) :: jgdtn
  integer, dimension(:) :: jgdt(*)
  logical, intent(in) :: unpack
  integer, intent(out) :: k
  type(gribfield), intent(out) :: gfld
  integer, intent(out) ::iret
  integer :: idxver = 1

  call getgb2i2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       unpack, idxver, k, gfld, iret)
end subroutine getgb2

!> Find and unpack a GRIB2 message in a file, using an version 1 or 2
!> index record which is either found in memory, read from an index
!> file, or generated.
!>
!> This subroutine is similar to getgb2(), but allows the caller to
!> specify the index version of the generated index (if one is
!> generated). Use index version 2 for all new code, as it handles all
!> GRIB2 files, including files > 2 GB. Use index version 1 for
!> backward compatibility, but it does not work past the first 2 GB of
!> the file.
!>
!> The GRIB field request specifies the number of fields to skip and
!> the unpacked identification section, grid definition template and
!> product defintion section parameters. (A requested parameter of
!> -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested GRIB field is found, then it is read from the GRIB
!> file and unpacked. Its number is returned along with the associated
!> unpacked parameters. the bitmap (if any); the data values are
!> unpacked only if argument unpack is set to true. If the GRIB
!> field is not found, then the return code will be nonzero.
!>
!> The decoded information for the selected GRIB field is returned in
!> a derived type variable, gfld, which is of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> Derived type @ref grib_mod::gribfield contains pointers to many
!> arrays of data. Users must free this memory by calling gf_free().
!>
!> This subroutine calls getidx2(), which allocates memory and stores
!> the resulting pointers in an array that is a Fortran "save"
!> variable. The result is that the memory will not be freed by the
!> library and cannot be reached by the caller. To free this memory
!> call gf_finalize() after all library operations are complete.
!>
!> @note Specify an index file if feasible to increase speed. Do not
!> engage the same logical unit from more than one processor.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] lugi integer unit of the unblocked grib index file.
!> If nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. lugi may be:
!> - > 0 read index from index file lugi, if index doesn"t already exist.
!> - = 0 to get index buffer from the grib file, if index
!> doesn"t already exist.
!> - < 0 force reread of index from index file abs(lugi).
!> - = lugb force regeneration of index from GRIB2 file lugb.
!> @param[in] j integer number of fields to skip (0 to search from
!> beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field:
!> --1 accept any discipline
!> - 0 meteorological products
!> - 1 hydrological products
!> - 2 land surface products
!> - 3 space products
!> - 10 oceanographic products
!> @param[in] JIDS integer array of values in the identification section
!> (=-9999 for wildcard)
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
!> @param[in] jpdtn integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] jgdtn integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[in] unpack logical value indicating whether to unpack bitmap/data
!> - .TRUE. unpack bitmap and data values
!> - .FALSE. do not unpack bitmap and data values
!> @param[in] idxver Index version of the cindex buffer.
!> @param[out] k integer field number unpacked
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Ed Hartnett, Mark Iredell @date 2024-03-19
subroutine getgb2i2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
     unpack, idxver, k, gfld, iret)
  use g2logging
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, lugi, j, jdisc
  integer, dimension(:) :: jids(*)
  integer, intent(in) :: jpdtn
  integer, dimension(:) :: jpdt(*)
  integer, intent(in) :: jgdtn
  integer, dimension(:) :: jgdt(*)
  logical, intent(in) :: unpack
  integer, intent(in) :: idxver
  integer, intent(out) :: k
  type(gribfield), intent(out) :: gfld
  integer, intent(out) ::iret
  character(len = 1), pointer, dimension(:) :: cbuf
  integer :: nnum, nlen, lpos, jk, irgi, irgs

  ! Declare interfaces (required for cbuf pointer).
  interface
     subroutine getidx2(lugb, lugi, idxver, cindex, nlen, nnum, iret)
       integer, intent(in) :: lugb, lugi, idxver
       character(len = 1), pointer, dimension(:) :: cindex
       integer, intent(out) :: nlen, nnum, iret
     end subroutine getidx2
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
     subroutine getgb2l2(lugb, idxver, cindex, gfld, iret)
       use grib_mod
       integer, intent(in) :: lugb, idxver
       character(len = 1), intent(in) :: cindex(*)
       type(gribfield) :: gfld
       integer, intent(out) :: iret
     end subroutine getgb2l2
     subroutine getgb2r2(lugb, idxver, cindex, gfld, iret)
       use grib_mod
       implicit none
       integer, intent(in) :: lugb, idxver
       character(len=1), intent(in) :: cindex(*)
       type(gribfield) :: gfld
       integer, intent(out) :: iret
     end subroutine getgb2r2
  end interface

#ifdef LOGGING
  write(g2_log_msg, '(a, i2, a, i2, a, i5, a, i5, a, l, a, i1)') 'getgb2i2: lugb ', lugb, ' lugi ', lugi, &
       ' j ', j, ' jdisc ', jdisc, ' unpack ', unpack, ' idxver ', idxver
  call g2_log(1)
#endif
  
  ! Fill cbuf with the index records of this file, by recalling them
  ! from memory, reading them from the index file, or generating them
  ! from the data file.
  irgi = 0
  call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, irgi)
  if (irgi .gt. 1) then
     iret = 96
     return
  endif

  ! Search the index in cbuf for the first message which meets our
  ! search criteria.
  call getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  jk, &
       gfld, lpos, irgs)
  if (irgs .ne. 0) then
     iret = 99
     call gf_free(gfld)
     return
  endif

  ! Read local use section for the selected GRIB2 message, if available.
  call getgb2l2(lugb, idxver, cbuf(lpos), gfld, iret)

  ! Read and unpack grib record for the selected GRIB2 message.
  if (unpack) then
     call getgb2r2(lugb, idxver, cbuf(lpos), gfld, iret)
  endif

  ! Return the number of the unpacked field to the caller.
  k = jk
end subroutine getgb2i2

!> Read and unpack a local use section from a GRIB2 index record
!> (index format 1) and GRIB2 file.
!>
!> This subprogram is intended for private use by getgb2() routine
!> only.
!>
!> Note that derived type gribfield contains pointers to many arrays
!> of data. Users must free this memory with gf_free().
!>
!> This subroutine supports only index format 1, which will not work
!> for files > 2 GB. New code should use getgb2l2().
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> @param[in] cindex index record of the grib field (see ix2gb2() for
!> description of an index record.)
!> @param[out] gfld derived type gribfield @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Stephen Gilbert @date 2002-05-07
subroutine getgb2l(lugb, cindex, gfld, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb
  character(len = 1), intent(in) :: cindex(*)
  type(gribfield) :: gfld
  integer, intent(out) :: iret

  interface
     subroutine getgb2l2(lugb, idxver, cindex, gfld, iret)
       use grib_mod
       integer, intent(in) :: lugb, idxver
       character(len = 1), intent(in) :: cindex(*)
       type(gribfield) :: gfld
       integer, intent(out) :: iret
     end subroutine getgb2l2
  end interface

  call getgb2l2(lugb, 1, cindex, gfld, iret)

end subroutine getgb2l
  
!> Read and unpack a local use section from a GRIB2 index record
!> (index format 1 or 2) and GRIB2 file.
!>
!> This subroutine decodes information for the selected GRIB2 field and
!> returns it in a derived type variable, gfld. gfld is of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> This subprogram is intended for private use by getgb2() routine
!> only.
!>
!> Note that derived type gribfield contains pointers to many arrays
!> of data. Users must free this memory with gf_free().
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> @param[in] idxver Index version of the cindex buffer.
!> @param[in] cindex index record of the grib field (see ix2gb2() for
!> description of an index record.)
!> @param[out] gfld derived type gribfield @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Stephen Gilbert @date 2002-05-07
subroutine getgb2l2(lugb, idxver, cindex, gfld, iret)
  use g2logging
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, idxver
  character(len = 1), intent(in) :: cindex(*)
  type(gribfield) :: gfld
  integer, intent(out) :: iret

  integer :: lskip, skip2
  integer (kind = 8) :: lskip8, iskip8, lread8, ilen8, skip28
  character(len = 1):: csize(4)
  character(len = 1), allocatable :: ctemp(:)
  integer :: ilen, iofst, ierr
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)
  integer :: mypos

  interface
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
     subroutine g2_gbytec81(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
       integer (kind = 8) :: iout(1)
     end subroutine g2_gbytec81
     subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: lencsec2
       integer, intent(out) :: ierr
       character(len = 1), pointer, dimension(:) :: csec2
     end subroutine gf_unpack2
  end interface


#ifdef LOGGING
  write(g2_log_msg, '(a, i2, a, i1)') 'getgb2l2: lugb ', lugb, ' idxver ', idxver
  call g2_log(1)
#endif
  
  ! Get info.
  nullify(gfld%local)
  iret = 0
  mypos = INT4_BITS ! Skip length of index record.

  ! These are all 4-byte ints in index format 1, and 8-byte ints in
  ! index version 2.
  if (idxver .eq. 1) then
     ! Read bytes to skip in file before message.
     call g2_gbytec1(cindex, lskip, mypos, INT4_BITS)
     mypos = mypos + INT4_BITS
     lskip8 = lskip
     ! Read bytes to skip in msg before local use.
     call g2_gbytec1(cindex, skip2, mypos, INT4_BITS)
     skip28 = skip2
  else
     ! Read bytes to skip in file before message.
     call g2_gbytec81(cindex, lskip8, mypos, INT8_BITS)
     mypos = mypos + INT8_BITS
     ! Read bytes to skip in msg before local use.
     call g2_gbytec81(cindex, skip28, mypos, INT8_BITS)
     mypos = mypos + INT8_BITS
  endif

  ! Read and unpack local use section, if present.
  if (skip28 .ne. 0) then
     iskip8 = lskip8 + skip28

     ! Get length of section.
     call bareadl(lugb, iskip8, 4_8, lread8, csize)    
     call g2_gbytec1(csize, ilen, 0, 32)
     allocate(ctemp(ilen))
     ilen8 = ilen

     ! Read in section.
     call bareadl(lugb, iskip8, ilen8, lread8, ctemp)  
     if (ilen8 .ne. lread8) then
        iret = 97
        deallocate(ctemp)
        return
     endif
     iofst = 0
     call gf_unpack2(ctemp, ilen, iofst, gfld%locallen, gfld%local, ierr)
     if (ierr .ne. 0) then
        iret = 98
        deallocate(ctemp)
        return
     endif
     deallocate(ctemp)
  else
     gfld%locallen = 0
  endif
end subroutine getgb2l2

!> Find and extract a GRIB2 message from a file.
!>
!> This subroutine reads a GRIB index file (or optionally the GRIB
!> file itself) to get the index buffer (i.e. table of contents) for
!> the GRIB file. It finds in the index buffer a reference to the
!> GRIB field requested.
!>
!> The GRIB field request specifies the number of fields to skip and
!> the unpacked identification section, grid definition template and
!> product defintion section parameters. (A requested parameter of
!> -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested GRIB field is found, then it is read from the GRIB
!> file and unpacked. If the GRIB field is not found, then the return
!> code will be nonzero.
!>
!> The derived type @ref grib_mod::gribfield contains allocated memory
!> that must be freed by the caller with subroutine gf_free().
!>
!> @note Specifing an index file may increase speed.
!> Do not engage the same logical unit from more than one processor.
!>
!> @param[in] lugb Unit of the unblocked GRIB data file. The
!> file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] lugi Unit of the unblocked GRIB index file. If
!> nonzero, file must have been opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> subroutine. Set to 0 to get index buffer from the GRIB file.
!> @param[in] j Number of fields to skip (set to 0 to search
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
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] extract value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. return GRIB2 message containing only the requested field.
!> - .false. return entire GRIB2 message containing the requested field.
!> @param[out] k field number unpacked.
!> @param[out] gribm returned GRIB message.
!> @param[out] leng length of returned GRIB message in bytes.
!> @param[out] iret integer return code
!> - 0 No error.
!> - 96 Error reading index.
!> - 97 Error reading GRIB file.
!> - 99 Request not found.
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
  integer :: msk1, irgi, irgs, jk, lpos, msk2, mskp, nlen, nmess, nnum

  character(len = 1), pointer, dimension(:) :: cbuf
  parameter(msk1 = 32000, msk2 = 4000)

  ! Declare interfaces (required for cbuf pointer).
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

  ! Initialize the index information in cbuf.
  irgi = 0
  if (lugi .gt. 0) then
     call getg2i(lugi, cbuf, nlen, nnum, irgi)
  elseif (lugi .le. 0) then
     mskp = 0
     call getg2ir(lugb, msk1, msk2, mskp, cbuf, nlen, nnum, nmess, irgi)
  endif
  if (irgi .gt. 1) then
     iret = 96
     return
  endif

  ! Find info from index and fill a grib_mod::gribfield variable.
  call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       jk, gfld, lpos, irgs)
  if (irgs .ne. 0) then
     iret = 99
     call gf_free(gfld)
     return
  endif

  ! Extract grib message from file.
  nullify(gribm)
  call getgb2rp(lugb, cbuf(lpos:), extract, gribm, leng, iret)

  k = jk

  ! Free cbuf memory allocated in getg2i/getg2ir().
  if (associated(cbuf)) deallocate(cbuf)
  
  call gf_free(gfld)
end subroutine getgb2p

! subroutine getgb2p2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
!      extract, k, gribm, leng, iret)
!   use grib_mod
!   implicit none

!   integer, intent(in) :: lugb, lugi, j, jdisc, jpdtn, jgdtn
!   integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
!   logical, intent(in) :: extract
!   integer, intent(out) :: k, iret, leng
!   character(len = 1), pointer, dimension(:) :: gribm

!   type(gribfield) :: gfld
!   integer :: irgi, irgs, jk, lpos, mskp, nlen, nmess, nnum, idxver
!   integer(kind = 8) :: msk1, msk2

!   character(len = 1), pointer, dimension(:) :: cbuf
!   parameter(msk1 = 32000, msk2 = 4000)

!   ! Declare interfaces (required for cbuf pointer).
!   interface
!      subroutine getg2i2(lugi, cbuf, idxver, nlen, nnum, iret)
!        integer, intent(in) :: lugi
!        character(len=1), pointer, dimension(:) :: cbuf
!        integer, intent(out) :: idxver, nlen, nnum, iret
!      end subroutine getg2i2
!      subroutine getg2i2r(lugb, msk1, msk2, mnum, idxver, cbuf, &
!           nlen, nnum, nmess, iret)
!        integer, intent(in) :: lugb
!        integer (kind = 8), intent(in) :: msk1, msk2
!        integer, intent(in) :: mnum, idxver
!        character(len = 1), pointer, dimension(:) :: cbuf
!        integer, intent(out) :: nlen, nnum, nmess, iret
!      end subroutine getg2i2r
!      subroutine getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
!           jgdt, k, gfld, lpos, iret)
!        import gribfield
!        character(len = 1), intent(in) :: cbuf(nlen)
!        integer, intent(in) :: idxver, nlen, nnum, j, jdisc
!        integer, dimension(:) :: jids(*)
!        integer, intent(in) :: jpdtn
!        integer, dimension(:) :: jpdt(*)
!        integer, intent(in) :: jgdtn
!        integer, dimension(:) :: jgdt(*)
!        integer, intent(out) :: k
!        type(gribfield), intent(out) :: gfld
!        integer, intent(out) :: lpos, iret
!      end subroutine getgb2s2
!      subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng, iret)
!        integer, intent(in) :: lugb, idxver
!        character(len = 1), intent(in) :: cindex(*)
!        logical, intent(in) :: extract
!        character(len = 1), pointer, dimension(:) :: gribm
!        integer, intent(out) :: leng, iret
!      end subroutine getgb2rp2
!   end interface

!   ! Initialize the index information in cbuf.
!   irgi = 0
!   if (lugi .gt. 0) then
!      call getg2i2(lugi, cbuf, idxver, nlen, nnum, irgi)
!   elseif (lugi .le. 0) then
!      mskp = 0
!      call getg2i2r(lugb, msk1, msk2, mskp, 2, cbuf, nlen, nnum, nmess, irgi)
!   endif
!   if (irgi .gt. 1) then
!      iret = 96
!      return
!   endif

!   ! Find info from index and fill a grib_mod::gribfield variable.
!   call getgb2s2(cbuf, 2, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
!        jk, gfld, lpos, irgs)

!   if (irgs .ne. 0) then
!      iret = 99
!      call gf_free(gfld)
!      return
!   endif

!   ! Extract grib message from file.
!   nullify(gribm)
!   call getgb2rp2(lugb, 2, cbuf(lpos:), .false., gribm, leng, iret)

!   k = jk

!   ! Free cbuf memory allocated in getg2i/getg2ir().
!   if (associated(cbuf)) deallocate(cbuf)

!   call gf_free(gfld)
! end subroutine getgb2p2

!> Read and unpack sections 6 and 7 from a GRIB2 message using a
!> version 1 index record.
!>
!> This function is maintained for backward compatibility. New code
!> should use getgb2r2(), which can handle both version 1 and version
!> 2 index records.
!>
!> Metadata for the field must already exists in derived type @ref
!> grib_mod::gribfield. Specifically, it requires gfld\%ibmap,
!> gfld\%ngrdpts, gfld\%idrtnum, gfld\%idrtmpl, and gfld\%ndpts.
!>
!> The field data is returned in derived type variable, gfld, of type
!> @ref grib_mod::gribfield. Users of this routine will need to
!> include the line "use grib_mod" in their calling routine.
!>
!> This subprogram is intended for private use by getgb2()
!> routines only.
!>
!> Derived type gribfield contains pointers to many arrays of
!> data. Users must free this memory by calling gf_free().
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] cindex version 1 index record of the field (see
!> subroutine ix2gb2() for description of an index record.)
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret Return code:
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Stephen Gilbert, Ed Hartnett @date 2002-01-11
subroutine getgb2r(lugb, cindex, gfld, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb
  character(len=1), intent(in) :: cindex(*)
  type(gribfield) :: gfld
  integer, intent(out) :: iret

  interface
     subroutine getgb2r2(lugb, idxver, cindex, gfld, iret)
       use grib_mod
       implicit none
       integer, intent(in) :: lugb, idxver
       character(len=1), intent(in) :: cindex(*)
       type(gribfield) :: gfld
       integer, intent(out) :: iret
     end subroutine getgb2r2
  end interface

  call getgb2r2(lugb, 1, cindex, gfld, iret)

end subroutine getgb2r

!> Read and unpack sections 6 and 7 from a GRIB2 message using a
!> version 1 or version 2 index record.
!>
!> Metadata for the field must already exists in derived type @ref
!> grib_mod::gribfield. Specifically, it requires gfld\%ibmap,
!> gfld\%ngrdpts, gfld\%idrtnum, gfld\%idrtmpl, and gfld\%ndpts.
!>
!> The field data is returned in derived type variable, gfld, of type
!> @ref grib_mod::gribfield. Users of this routine will need to
!> include the line "use grib_mod" in their calling routine.
!>
!> This subprogram is intended for private use by getgb2()
!> routines only.
!>
!> Derived type gribfield contains pointers to many arrays of
!> data. Users must free this memory by calling gf_free().
!>
!> @note Do not engage the same logical unit from more than one
!> processor.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] idxver Index version, 1 for legacy, 2 if file may be >
!> 2 GB.
!> @param[in] cindex version 1 or 2 index record of the grib field
!> (see subroutine ixgb2() for description of an index record.)
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret Return code:
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Ed Hartnett, Stephen Gilbert @date Feb 14, 2024
subroutine getgb2r2(lugb, idxver, cindex, gfld, iret)
  use g2logging
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, idxver
  character(len=1), intent(in) :: cindex(*)
  type(gribfield) :: gfld
  integer, intent(out) :: iret

  integer :: lskip, skip6, skip7
  character(len=1):: csize(4)
  character(len=1), allocatable :: ctemp(:)
  real, pointer, dimension(:) :: newfld
  integer :: n, j, iskip, iofst, ilen, ierr, idum
  integer :: inc
  integer (kind = 8) :: lskip8, lread8, ilen8, iskip8
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)

  interface
     subroutine gf_unpack6(cgrib, lcgrib, iofst, ngpts, ibmap, bmap, ierr)
       character(len=1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib, ngpts
       integer, intent(inout) :: iofst
       integer, intent(out) :: ibmap
       integer, intent(out) :: ierr
       logical*1, pointer, dimension(:) :: bmap
     end subroutine gf_unpack6
     subroutine gf_unpack7(cgrib, lcgrib, iofst, igdsnum, igdstmpl, &
          idrsnum, idrstmpl, ndpts, fld, ierr)
       character(len=1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib, ndpts, idrsnum, igdsnum
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: idrstmpl, igdstmpl
       integer, intent(out) :: ierr
       real, pointer, dimension(:) :: fld
     end subroutine gf_unpack7
  end interface

#ifdef LOGGING
  write(g2_log_msg, '(a, i2, a, i1)') 'getgb2r2: lugb ', lugb, ' idxver ', idxver
  call g2_log(1)
#endif
  
  ! Get info.
  nullify(gfld%bmap, gfld%fld)
  iret = 0
  inc = 0
  if (idxver .eq. 1) then
     call g2_gbytec(cindex, lskip, INT4_BITS, INT4_BITS)
     lskip8 = lskip
  else
     inc = 20
     call g2_gbytec8(cindex, lskip8, INT4_BITS, INT8_BITS)
     lskip = int(lskip8, kind(4))
  endif
  call g2_gbytec(cindex, skip6, (24 + inc) * INT1_BITS, INT4_BITS)
  call g2_gbytec(cindex, skip7, (28 + inc) * INT1_BITS, INT4_BITS)

  ! Read and unpack bit_map, if present.
  if (gfld%ibmap .eq. 0 .or. gfld%ibmap .eq. 254) then
     iskip = lskip + skip6
     iskip8 = lskip8 + skip6

     ! get length of section.
     call bareadl(lugb, iskip8, 4_8, lread8, csize)
     call g2_gbytec(csize, ilen, 0, 32)
     allocate(ctemp(ilen))
     ilen8 = ilen
     
     ! read in section.
     call bareadl(lugb, iskip8, ilen8, lread8, ctemp)  
     if (ilen8 .ne. lread8) then
        iret = 97
        deallocate(ctemp)
        return
     endif
     iofst = 0
     call gf_unpack6(ctemp, ilen, iofst, gfld%ngrdpts, idum, gfld%bmap, ierr)
     if (ierr .ne. 0) then
        iret = 98
        deallocate(ctemp)
        return
     endif
     deallocate(ctemp)
  endif

  ! Read and unpack data field.
  iskip = lskip + skip7
  iskip8 = lskip8 + skip7
  
  ! Get length of section.
  call bareadl(lugb, iskip8, 4_8, lread8, csize)    
  call g2_gbytec(csize, ilen, 0, 32)
  if (ilen .lt. 6) ilen = 6
  allocate(ctemp(ilen))
  ilen8 = ilen

  ! Read in section.
  call bareadl(lugb, iskip8, ilen8, lread8, ctemp)  
  if (ilen8 .ne. lread8) then
     iret = 97
     deallocate(ctemp)
     return
  endif
  iofst = 0
  call gf_unpack7(ctemp, ilen, iofst, gfld%igdtnum, gfld%igdtmpl, &
       gfld%idrtnum, gfld%idrtmpl, gfld%ndpts, gfld%fld, ierr)
  if (ierr .ne. 0) then
     iret = 98
     deallocate(ctemp)
     return
  endif
  deallocate(ctemp)

  ! If bitmap is used with this field, expand data field
  ! to grid, if possible.
  if (gfld%ibmap .ne. 255 .AND. associated(gfld%bmap)) then
     allocate(newfld(gfld%ngrdpts))
     n = 1
     do j = 1, gfld%ngrdpts
        if (gfld%bmap(j)) then
           newfld(j) = gfld%fld(n)
           n = n+1
        else
           newfld(j) = 0.0
        endif
     enddo
     deallocate(gfld%fld);
     gfld%fld => newfld;
     gfld%expanded = .true.
  else
     gfld%expanded = .true.
  endif
end subroutine getgb2r2

!> Extract a grib message from a file given the index (index format 1)
!> of the requested field.
!>
!> This subroutine is maintained for backward compatibility. New code
!> should use getgb2rp2().
!>
!> The GRIB message returned can contain only the requested field
!> (extract=.true.), or the complete GRIB message originally
!> containing the desired field can be returned (extract=.false.) even
!> if other fields were included in the GRIB message.
!>
!> If the GRIB field is not found, then the return code will be
!> nonzero.
!>
!> @note For files greater than 2 GB, this subroutine will only work
!> if the message is within the first 2 GB of the file.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] cindex Index record of the grib field (see docunentation of
!> subroutine ix2gb2() for description of an index record.)
!> @param[in] extract Logical value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] gribm Returned grib message.
!> @param[out] leng Length of returned grib message in bytes.
!> @param[out] iret Return code:
!> - 0 No error.
!> - 97 Error reading grib file.
!>
!> @author Stephen Gilbert, Ed Hartnett @date 2003-12-31
subroutine getgb2rp(lugb, cindex, extract, gribm, leng, iret)
  implicit none

  integer, intent(in) :: lugb
  character(len = 1), intent(in) :: cindex(*)
  logical, intent(in) :: extract
  character(len = 1), pointer, dimension(:) :: gribm
  integer, intent(out) :: leng, iret
  integer (kind = 8) :: leng8

  interface
     subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng8, iret)
       integer, intent(in) :: lugb, idxver
       character(len = 1), intent(in) :: cindex(*)
       logical, intent(in) :: extract
       character(len = 1), pointer, dimension(:) :: gribm
       integer (kind = 8), intent(out) :: leng8
       integer, intent(out) :: iret
     end subroutine getgb2rp2
  end interface

  ! Call the legacy version of this function. It will only work with
  ! GRIB messages < 2 GB.
  call getgb2rp2(lugb, 1, cindex, extract, gribm, leng8, iret)
  leng = int(leng8, kind(4))

end subroutine getgb2rp

!> Extract a grib message from a file given the version 1 or 2 index
!> of the requested field.
!>
!> The GRIB message returned can contain only the requested field
!> (extract=.true.), or the complete GRIB message originally
!> containing the desired field can be returned (extract=.false.) even
!> if other fields were included in the GRIB message.
!>
!> If the GRIB field is not found, then the return code will be
!> nonzero.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] idxver Version of index, use 1 for legacy, 2 if files
!> may be > 2 GB.
!> @param[in] cindex Index record of the grib field (see docunentation of
!> subroutine ixgb2() for description of an index record.)
!> @param[in] extract Logical value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] gribm Returned grib message.
!> @param[out] leng8 Length of returned grib message in bytes.
!> @param[out] iret Return code:
!> - 0 No error.
!> - 97 Error reading grib file.
!>
!> @author Edward Hartnett, Stephen Gilbert @date Feb 13, 2024
subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng8, iret)
  use g2logging
  implicit none

  integer, intent(in) :: lugb, idxver
  character(len = 1), intent(in) :: cindex(*)
  logical, intent(in) :: extract
  integer (kind = 8), intent(out) :: leng8
  integer, intent(out) :: iret
  character(len = 1), pointer, dimension(:) :: gribm

  integer, parameter :: zero = 0
  character(len = 1), allocatable, dimension(:) :: csec2, csec6, csec7
  character(len = 4) :: ctemp
  integer :: lencur, len0, ibmap = 0, ipos, iskip
  integer :: len7 = 0, len8 = 0, len3 = 0, len4 = 0, len5 = 0, len6 = 0, len1 = 0, len2 = 0
  integer :: iskp2, iskp6, iskp7
  integer (kind = 8) :: iskp2_8
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)
  integer :: mypos, inc = 0
  integer (kind = 8) :: lread8, iskip8, len2_8, len7_8, len6_8

#ifdef LOGGING
  write(g2_log_msg, '(a, i2, a, i1, a, l)') 'getgb2rp2: lugb ', lugb, ' idxver ', idxver, &
       ' extract ', extract
  call g2_log(1)
#endif
  
  iret = 0

  ! Extract grib message from file.
  mypos = INT4_BITS
  if (extract) then
     len0 = 16
     len8 = 4
     if (idxver .eq. 1) then
        call g2_gbytec(cindex, iskip, mypos, INT4_BITS)    ! bytes to skip in file
        mypos = mypos + INT4_BITS
        iskip8 = iskip
        call g2_gbytec(cindex, iskp2, mypos, INT4_BITS)    ! bytes to skip for section 2
        mypos = mypos + INT4_BITS
        iskp2_8 = iskp2
        mypos = mypos + 32 * INT1_BITS ! skip ahead in the cindex
     else
        inc = 20
        call g2_gbytec8(cindex, iskip8, mypos, INT8_BITS)    ! bytes to skip in file
        mypos = mypos + INT8_BITS
        iskip = int(iskip8, kind(4))
        call g2_gbytec8(cindex, iskp2_8, mypos, INT8_BITS)    ! bytes to skip for section 2
        mypos = mypos + INT8_BITS
        mypos = mypos + 36 * INT1_BITS ! skip ahead in the cindex
     endif
     if (iskp2_8 .gt. 0) then
        call bareadl(lugb, iskip8 + iskp2_8, 4_8, lread8, ctemp)
        call g2_gbytec(ctemp, len2, 0, INT4_BITS)      ! length of section 2
        allocate(csec2(len2))
        len2_8 = len2
        call bareadl(lugb, iskip8 + iskp2_8, len2_8, lread8, csec2)
     else
        len2 = 0
     endif
     call g2_gbytec(cindex, len1, mypos, INT4_BITS)      ! length of section 1
     ipos = 44 + len1
     mypos = mypos + len1 * INT1_BITS ! skip ahead in the cindex
     call g2_gbytec(cindex, len3, mypos, INT4_BITS)      ! length of section 3
     ipos = ipos + len3
     mypos = mypos + len3 * INT1_BITS ! skip ahead in the cindex
     call g2_gbytec(cindex, len4, mypos, INT4_BITS)      ! length of section 4
     ipos = ipos + len4
     mypos = mypos + len4 * INT1_BITS ! skip ahead in the cindex
     call g2_gbytec(cindex, len5, mypos, INT4_BITS)      ! length of section 5
     ipos = ipos + len5
     mypos = mypos + len5 * INT1_BITS ! skip ahead in the cindex
     call g2_gbytec(cindex, len6, mypos, INT4_BITS)      ! length of section 6
     ipos = ipos + 5
     mypos = mypos + len6 * INT1_BITS ! skip ahead in the cindex
     call g2_gbytec(cindex, ibmap, mypos, INT1_BITS)      ! bitmap indicator
     if (ibmap .eq. 254) then
        call g2_gbytec(cindex, iskp6, (24 + inc) * INT1_BITS, INT4_BITS)    ! bytes to skip for section 6
        !call baread(lugb, iskip + iskp6, 4, lread, ctemp)
        call bareadl(lugb, iskip8 + iskp6, 4_8, lread8, ctemp)
        call g2_gbytec(ctemp, len6, 0, INT4_BITS)      ! length of section 6
     endif

     !  read in section 7 from file
     call g2_gbytec(cindex, iskp7, (28 + inc) * INT1_BITS, INT4_BITS)    ! bytes to skip for section 7
     !call baread(lugb, iskip + iskp7, 4, lread, ctemp)
     call bareadl(lugb, iskip8 + iskp7, 4_8, lread8, ctemp)
     call g2_gbytec(ctemp, len7, 0, INT4_BITS)      ! length of section 7
     allocate(csec7(len7))
     !call baread(lugb, iskip + iskp7, len7, lread, csec7)
     len7_8 = len7
     call bareadl(lugb, iskip8 + iskp7, len7_8, lread8, csec7)

     leng8 = len0 + len1 + len2 + len3 + len4 + len5 + len6 + len7 + len8
     if (.not. associated(gribm)) allocate(gribm(leng8))

     ! Create Section 0
     gribm(1) = 'G'
     gribm(2) = 'R'
     gribm(3) = 'I'
     gribm(4) = 'B'
     gribm(5) = char(0)
     gribm(6) = char(0)
     gribm(7) = cindex(42 + inc) ! discipline
     gribm(8) = cindex(41 + inc) ! GRIB version
     call g2_sbytec8(gribm, leng8, 8 * INT1_BITS, INT8_BITS)

     ! Copy Section 1
     gribm(17:16 + len1) = cindex(45 + inc:44 + inc + len1)
     lencur = 16 + len1
     ipos = 44 + inc + len1

     ! Copy Section 2, if necessary
     if (iskp2_8 .gt. 0) then
        gribm(lencur + 1:lencur + len2) = csec2(1:len2)
        lencur = lencur + len2
     endif

     ! Copy Sections 3 through 5
     gribm(lencur + 1:lencur + len3 + len4 + len5) = cindex(ipos + 1:ipos + len3 + len4 + len5)
     lencur = lencur + len3 + len4 + len5
     ipos = ipos + len3 + len4 + len5

     ! Copy Section 6
     if (len6 .eq. 6 .and. ibmap .ne. 254) then
        gribm(lencur + 1:lencur + len6) = cindex(ipos + 1:ipos + len6)
        lencur = lencur + len6
     else
        call g2_gbytec(cindex, iskp6, (24 + inc) * 8, INT4_BITS)    ! bytes to skip for section 6
        call bareadl(lugb, iskip8 + iskp6, 4_8, lread8, ctemp)
        call g2_gbytec(ctemp, len6, 0, INT4_BITS)      ! length of section 6
        allocate(csec6(len6))
        len6_8 = len6
        call bareadl(lugb, iskip8 + iskp6, len6_8, lread8, csec6)
        gribm(lencur + 1:lencur + len6) = csec6(1:len6)
        lencur = lencur + len6
        if (allocated(csec6)) deallocate(csec6)
     endif

     ! Copy Section 7
     gribm(lencur + 1:lencur + len7) = csec7(1:len7)
     lencur = lencur + len7

     ! Section 8
     gribm(lencur + 1) = '7'
     gribm(lencur + 2) = '7'
     gribm(lencur + 3) = '7'
     gribm(lencur + 4) = '7'

     !  clean up
     if (allocated(csec2)) deallocate(csec2)
     if (allocated(csec7)) deallocate(csec7)
  else    ! do not extract field from message :  get entire message
     if (idxver .eq. 1) then
        call g2_gbytec(cindex, iskip, mypos, INT4_BITS)    ! bytes to skip in file
        mypos = mypos + INT4_BITS
        mypos = mypos + 6 * INT4_BITS
        iskip8 = iskip
     else
        call g2_gbytec8(cindex, iskip8, mypos, INT8_BITS)    ! bytes to skip in file
        mypos = mypos + INT8_BITS
        mypos = mypos + 2 * INT8_BITS + 4 * INT4_BITS
     endif

     call g2_gbytec8(cindex, leng8, mypos, INT8_BITS)      ! length of grib message
#ifdef LOGGING
     write(g2_log_msg, *) ' iskip8 ', iskip8, ' mypos/8 ', mypos/8, &
          ' leng8 ', leng8
     call g2_log(2)
#endif

     if (.not. associated(gribm)) allocate(gribm(leng8))
     call bareadl(lugb, iskip8, leng8, lread8, gribm)
     if (leng8 .ne. lread8) then
        deallocate(gribm)
        nullify(gribm)
        iret = 97
        return
     endif
  endif
end subroutine getgb2rp2
