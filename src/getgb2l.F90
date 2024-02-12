!> @file
!> @brief Read and unpack a local use section from a GRIB2 index
!> record.
!> @author Stephen Gilbert @date 2002-05-07

!> Read and unpack a local use section from a GRIB2 index record.
!>
!> This subroutine decodes information for the selected grib field and
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
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> @param[in] CINDEX index record of the grib field (see ix2gb2() for
!> description of an index record.)
!> @param[out] GFLD derived type gribfield @ref grib_mod::gribfield.
!> @param[out] IRET integer return code
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

  integer :: lskip, skip2
  character(len = 1):: csize(4)
  character(len = 1), allocatable :: ctemp(:)
  integer :: ilen, iofst, iskip, lread, ierr

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
  call g2_gbytec(cindex, lskip, 4 * 8, 4 * 8)
  call g2_gbytec(cindex, skip2, 8 * 8, 4 * 8)

  ! Read and unpack local use section, if present.
  if (skip2 .ne. 0) then
     iskip = lskip + skip2

     ! Get length of section.
     call baread(lugb, iskip, 4, lread, csize)    
     call g2_gbytec(csize, ilen, 0, 32)
     allocate(ctemp(ilen))

     ! Read in section.
     call baread(lugb, iskip, ilen, lread, ctemp)  
     if (ilen .ne. lread) then
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
end subroutine getgb2l
