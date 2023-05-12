!> @file
!> @brief Unpack a data field packed into a PNG image format.
!> @author Stephen Gilbert @date 2000-06-21

!> Unpack a data field packed into a PNG image format.
!>
!> This subroutine unpacks a data field that was packed into a
!> PNG image format using info from the GRIB2 Data Representation
!> Template 5.40 or 5.40000.
!>
!> @param[in] cpack The packed data field (character*1 array).
!> @param[in] len length of packed field cpack().
!> @param[in] idrstmpl Contains the array of values for Data
!> Representation Template 5.40 or 5.40000.
!> @param[in] ndpts The number of data values to unpack.
!> @param[out] fld Contains the unpacked data values.
!>
!> @author Stephen Gilbert @date 2000-06-21
subroutine pngunpack(cpack, len, idrstmpl, ndpts, fld)
  implicit none
  
  character(len = 1), intent(in) :: cpack(len)
  integer, intent(in) :: ndpts, len
  integer, intent(in) :: idrstmpl(*)
  real, intent(out) :: fld(ndpts)

  integer :: ifld(ndpts)
  character(len = 1), allocatable :: ctemp(:)
  integer(4) :: ieee
  real :: ref, bscale, dscale
  integer :: width, height
  integer :: iret, itype, j, nbits

  interface
     function dec_png(pngbuf, width, height, cout) bind(c, name="dec_png")
       use iso_c_binding
       character(kind = c_char), intent(in) :: pngbuf(*)
       integer(c_int), intent(in) :: width, height
       character(kind = c_char), intent(out) :: cout(*)
       integer(c_int) :: dec_png
     end function dec_png
  end interface

  ieee = idrstmpl(1)
  call rdieee(ieee, ref, 1)
  bscale = 2.0**real(idrstmpl(2))
  dscale = 10.0**real(-idrstmpl(3))
  nbits = idrstmpl(4)
  itype = idrstmpl(5)

  ! If nbits equals 0, we have a constant field where the reference value
  ! is the data value at each gridpoint.
  if (nbits .ne. 0) then
     allocate(ctemp(ndpts * 4))
     iret = dec_png(cpack, width, height, ctemp)
     call g2_gbytesc(ctemp, ifld, 0, nbits, 0, ndpts)
     deallocate(ctemp)
     do j = 1, ndpts
        fld(j) = ((real(ifld(j)) * bscale) + ref) * dscale
     enddo
  else
     do j = 1, ndpts
        fld(j) = ref
     enddo
  endif
end subroutine pngunpack
