!> @file
!> @brief This subroutine packs up a data field.
!> @author Stephen Gilbert @date 2000-06-21
!>

!> This subroutine unpacks a data field that was packed using a simple
!> packing algorithm as defined in the GRIB2 documention, using info
!> from the GRIB2 Data Representation Template 5.0.
!>    
!> @param[in] cpack The packed data field (character*1 array).
!> @param[in] len length of packed field cpack.
!> @param[in] idrstmpl Contains the array of values for Data
!> Representation Template 5.0.
!> @param[in] ndpts The number of data values to unpack.
!> @param[out] fld Contains the unpacked data values.
!>
!> @author Stephen Gilbert @date 2000-06-21
subroutine simunpack(cpack, len, idrstmpl, ndpts, fld)
  implicit none
  
  character(len=1), intent(in) :: cpack(len)
  integer, intent(in) :: ndpts, len
  integer, intent(in) :: idrstmpl(*)
  real, intent(out) :: fld(ndpts)

  integer :: ifld(ndpts)
  integer(4) :: ieee
  real :: ref, bscale, dscale
  integer :: itype, j, nbits

  ieee = idrstmpl(1)
  call rdieee(ieee, ref, 1)
  bscale = 2.0**real(idrstmpl(2))
  dscale = 10.0**real(-idrstmpl(3))
  nbits = idrstmpl(4)
  itype = idrstmpl(5)

  ! If nbits equals 0,  we have a constant field where the reference value
  ! is the data value at each gridpoint.
  if (nbits .ne. 0) then
     call g2_gbytesc(cpack, ifld, 0, nbits, 0, ndpts)
     do j=1, ndpts
        fld(j) = ((real(ifld(j)) * bscale) + ref) * dscale
     enddo
  else
     !print *, 'unpack ref ', ref
     !print *, 'unpack ndpts ', ndpts
     do j=1, ndpts
        fld(j) = ref
     enddo
  endif
end subroutine simunpack
