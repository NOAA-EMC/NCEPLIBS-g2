!> @file
!> @brief This subroutine unpacks Section 2 ([Local Use Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect2.shtml))
!> of a GRIB2 message.
!> @author Stephen Gilbert @date 2002-04-09

!> This subroutine unpacks Section 2 ([Local Use Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect2.shtml))
!> of a GRIB2 message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end (returned) of
!> Section 2.
!> @param[out] lencsec2 Length (in octets) of Local Use data.
!> @param[out] csec2 Pointer to a character*1 array containing local use data.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 2 Array passed is not section 2.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2002-04-09
subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  integer, intent(out) :: lencsec2
  integer, intent(out) :: ierr
  character(len = 1), pointer, dimension(:) :: csec2

  ierr = 0
  lencsec2 = 0
  nullify(csec2)

  call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
  iofst = iofst + 32
  lencsec2 = lensec - 5
  call g2_gbytec(cgrib, isecnum, iofst, 8) ! Get Section Number
  iofst = iofst + 8
  ipos = (iofst / 8) + 1

  if (isecnum .ne. 2) then
     ierr = 6
     print *, 'gf_unpack2: Not Section 2 data. '
     return
  endif

  allocate(csec2(lencsec2), stat = istat)
  if (istat .ne. 0) then
     ierr = 6
     nullify(csec2)
     return
  endif

  csec2(1:lencsec2) = cgrib(ipos:ipos + lencsec2 - 1)
  iofst = iofst + (lencsec2 * 8)

  return
end subroutine gf_unpack2
