!> @file
!> @brief This subroutine unpacks a data field packed into a
!> PNG image format.
!> @author Stephen Gilbert @date 2000-06-21

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
  integer(kind = 8) :: ndpts8, len8
  integer(kind = 8) :: idrstmpl8(7)
  integer :: i

  interface
#if KIND == 4
     subroutine pngunpack_c(cpack, len, idrstmpl, ndpts, fld) bind(c, name="pngunpack")
#else
     subroutine pngunpack_c(cpack, len, idrstmpl, ndpts, fld) bind(c, name="pngunpackd")
#endif
       use iso_c_binding
       character(kind = c_char), intent(in) :: cpack(*)
       integer(c_size_t), intent(in) :: len       
       integer(kind = c_size_t), intent(in) :: idrstmpl(*)              
       integer(c_size_t), intent(in) :: ndpts       
#if KIND == 4
       real(c_float), intent(out) :: fld(*)
#else       
       real(c_double), intent(out) :: fld(*)
#endif
     end subroutine pngunpack_c
  end interface

  ! We need these parameters as 8-byte ints for the C function.
  ndpts8 = ndpts
  len8 = len

  ! Need to copy idrstmpl array to 8-byte int array for the C
  ! function.
  do i = 1, 7
     idrstmpl8(i) = idrstmpl(i)
  end do

  ! Call the C function.
  call pngunpack_c(cpack, len8, idrstmpl8, ndpts8, fld)
  
end subroutine pngunpack
