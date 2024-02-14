!> @file
!> @brief Read and unpack sections 6 and 7 from a GRIB2 message using
!> an index record.
!> @author Stephen Gilbert @date 2002-01-11

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
  integer :: n, lread, j, iskip, iofst, ilen, ierr, idum
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

  ! Get info.
  nullify(gfld%bmap, gfld%fld)
  iret = 0
  inc = 0
  if (idxver .eq. 1) then
     call g2_gbytec(cindex, lskip, INT4_BITS, INT4_BITS)
     iskip8 = iskip
  else
     inc = 4
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
     call baread(lugb, iskip, 4, lread, csize)
     call g2_gbytec(csize, ilen, 0, 32)
     allocate(ctemp(ilen))

     ! read in section.
     call baread(lugb, iskip, ilen, lread, ctemp)  
     if (ilen .ne. lread) then
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
  call baread(lugb, iskip, 4, lread, csize)    
  call g2_gbytec(csize, ilen, 0, 32)
  if (ilen .lt. 6) ilen = 6
  allocate(ctemp(ilen))

  ! Read in section.
  call baread(lugb, iskip, ilen, lread, ctemp)  
  if (ilen .ne. lread) then
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
