!> @file
!> @brief This subroutine reads and unpacks a local use section from a
!> GRIB2 message.
!> @author Stephen Gilbert @date 2002-05-07

!> This subroutine reads and unpacks a local use section from a GRIB2
!> message.
!>
!> This subroutine decodes information for the selected grib field and
!> returns it in a derived type variable, gfld. gfld is of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> This subprogram is intended for private use by getgb2 routines
!> only.
!>
!> Note that derived type gribfield contains pointers to many arrays
!> of data. The memory for these arrays is allocated when the values
!> in the arrays are set, to help minimize problems with array
!> overloading. Because of this users should free this memory, when it
!> is no longer needed, by an explicit call to subroutine gf_free().
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> @param[in] CINDEX index record of the grib field (see docblock of
!> subroutine ixgb2 for description of an index record.)
!> @param[out] GFLD derived type gribfield @ref grib_mod::gribfield.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @note Do not engage the same logical unit from more than one
!> processor.
!>
!> @author Stephen Gilbert @date 2002-05-07
subroutine getgb2l(lugb, cindex, gfld, iret)

    use grib_mod
    implicit none

    integer, intent(in) :: lugb
    character(len = 1), intent(in) :: cindex(*)
    integer, intent(out) :: iret
    type(gribfield) :: gfld

    integer :: lskip, skip2
    character(len = 1):: csize(4)
    character(len = 1), allocatable :: ctemp(:)

    !implicit none additions
    integer :: iskip, lread, ilen, iofst, ierr

    interface
        subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
            character(len=1), intent(in) :: cgrib(lcgrib)
            integer, intent(in) :: lcgrib
            integer, intent(inout) :: iofst
            integer, intent(out) :: lencsec2
            integer, intent(out) :: ierr
            character(len = 1), pointer, dimension(:) :: csec2
        end subroutine gf_unpack2
    end interface

    !  get info
    nullify(gfld%local)
    iret = 0
    call g2_gbytec(cindex, lskip, 4 * 8, 4 * 8)
    call g2_gbytec(cindex, skip2, 8 * 8, 4 * 8)


    !  read and unpack local use section, if present
    if ( skip2 .ne. 0 ) then
        iskip = lskip + skip2
        call baread(lugb, iskip, 4, lread, csize)    ! get length of section
        call g2_gbytec(csize, ilen, 0, 32)
        allocate(ctemp(ilen))
        call baread(lugb, iskip, ilen, lread, ctemp)  ! read in section
        if (ilen .ne. lread) then
            iret = 97
            deallocate(ctemp)
            return
        endif
        iofst = 0
        call gf_unpack2(ctemp, ilen, iofst, gfld%locallen, &
            gfld%local, ierr)
        if (ierr .ne. 0) then
            iret = 98
            deallocate(ctemp)
            return
        endif
        deallocate(ctemp)
    else
        gfld%locallen = 0
    endif

    return
end subroutine getgb2l
