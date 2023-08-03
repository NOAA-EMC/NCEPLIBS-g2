!> @file
!> @brief Unpack Section 6 ([Bit-Map Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect6.shtml))
!> of a GRIB2 message.
!> @author Stephen Gilbert @date 2000-05-26

!> Unpack Section 6 ([Bit-Map Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect6.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of Section 6.
!> @param[in] ngpts Number of grid points specified in the bit-map.
!> @param[out] ibmap Bitmap indicator (see Code Table 6.0)
!> - 0 bitmap applies and is included in Section 6.
!> - 1-253 Predefined bitmap applies.
!> - 254 Previously defined bitmap applies to this field.
!> - 255 Bit map does not apply to this product.
!> @param[out] bmap Logical*1 array containing decoded bitmap. (if ibmap=0)
!> The dimension of this array can be obtained in advance from maxvals(7),
!> which is returned from subroutine gribinfo().
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 4 Unrecognized pre-defined bit-map.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack6(cgrib, lcgrib, iofst, ngpts, ibmap, bmap, ierr)
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ngpts
  integer, intent(inout) :: iofst
  integer, intent(out) :: ibmap
  integer, intent(out) :: ierr

  logical*1, pointer, dimension(:) :: bmap
  integer :: intbmap(ngpts)
  integer :: istat, j

  ierr = 0
  nullify(bmap)

  iofst = iofst + 32    ! Skip Length of Section.
  iofst = iofst + 8     ! Skip section number.

  call g2_gbytec(cgrib, ibmap, iofst, 8)    ! Get bit-map indicator
  iofst = iofst + 8

  if (ibmap .eq. 0) then               ! Unpack bitmap
     istat = 0
     if (ngpts .gt. 0) allocate(bmap(ngpts), stat = istat)
     if (istat .ne. 0) then
        ierr = 6
        nullify(bmap)
        return
     endif
     call g2_gbytesc(cgrib, intbmap, iofst, 1, 0, ngpts)
     iofst = iofst + ngpts
     do j = 1, ngpts
        bmap(j) = .true.
        if (intbmap(j) .eq. 0) bmap(j) = .false.
     enddo
  endif

end subroutine gf_unpack6

