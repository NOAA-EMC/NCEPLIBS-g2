!> @file
!> @brief Find and unpack a GRIB2 message in a file.
!> @author Mark Iredell @date 1994-04-01

!> Find and unpack a GRIB2 message in a file. It reads a GRIB index
!> file (or optionally the GRIB file itself) to get the index buffer
!> (i.e. table of contents) for the GRIB file.
!>
!> Find in the index buffer a reference to the GRIB field requested.
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
!> This subroutine calls getidx(), which allocates memory and stores
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
!> @param[out] k integer field number unpacked
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!> - other gf_getfld grib2 unpacker return code
!>
!> @author Mark Iredell @date 1994-04-01
subroutine getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
     unpack, k, gfld, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, lugi, j, jdisc, jpdtn, jgdtn
  integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
  logical, intent(in) :: unpack
  integer, intent(out) :: k, iret
  type(gribfield), intent(out) :: gfld
  character(len = 1), pointer, dimension(:) :: cbuf
  integer :: nnum, nlen, lpos, jk, irgi, irgs, idxver

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

  ! Determine whether index buffer needs to be initialized.
  irgi = 0
  idxver = 2
  call getidx2(lugb, lugi, idxver, cbuf, nlen, nnum, irgi)
  if (irgi .gt. 1) then
     iret = 96
     return
  endif

  ! search index buffer.
  call getgb2s2(cbuf, idxver, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  jk, &
       gfld, lpos, irgs)
  if (irgs .ne. 0) then
     iret = 99
     call gf_free(gfld)
     return
  endif

  ! Read local use section, if available.
  call getgb2l2(lugb, idxver, cbuf(lpos), gfld, iret)

  ! Read and unpack grib record.
  if (unpack) then
     call getgb2r2(lugb, idxver, cbuf(lpos), gfld, iret)
  endif
  k = jk
end subroutine getgb2

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
  use grib_mod
  implicit none

  integer, intent(in) :: lugb, idxver
  character(len = 1), intent(in) :: cindex(*)
  type(gribfield) :: gfld
  integer, intent(out) :: iret

  integer :: lskip, skip2
  integer (kind = 8) :: lskip8, iskip8, lread8, ilen8
  character(len = 1):: csize(4)
  character(len = 1), allocatable :: ctemp(:)
  integer :: ilen, iofst, ierr
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)
  integer :: mypos

  interface
     subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: lencsec2
       integer, intent(out) :: ierr
       character(len = 1), pointer, dimension(:) :: csec2
     end subroutine gf_unpack2
  end interface

  ! Get info.
  nullify(gfld%local)
  iret = 0
  mypos = INT4_BITS
  if (idxver .eq. 1) then
     call g2_gbytec(cindex, lskip, mypos, INT4_BITS)
     mypos = mypos + INT4_BITS
     lskip8 = lskip
  else
     call g2_gbytec8(cindex, lskip8, mypos, INT8_BITS)
     mypos = mypos + INT8_BITS
  endif
  call g2_gbytec(cindex, skip2, mypos, INT4_BITS)

  ! Read and unpack local use section, if present.
  if (skip2 .ne. 0) then
     iskip8 = lskip8 + skip2

     ! Get length of section.
     call bareadl(lugb, iskip8, 4_8, lread8, csize)    
     call g2_gbytec(csize, ilen, 0, 32)
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
