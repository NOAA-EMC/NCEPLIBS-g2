!> @file
!> @brief Unpack a data field from a JPEG2000 code stream.
!> @author Stephen Gilbert @date 2002-12-17

!> Unpack a data field from a JPEG2000 code stream.
!>
!> This subroutine unpacks a data field that was packed into a
!> JPEG2000 code stream using info from the GRIB2 Data Representation
!> Template 5.40 or 5.40000.
!>
!> @param[in] cpack The packed data field (character*1 array).
!> @param[in] len length of packed field cpack().
!> @param[in] idrstmpl Array of values for Data Representation
!> Template 5.40 or 5.40000.
!> @param[in] ndpts The number of data values to unpack.
!> @param[out] fld Contains the unpacked data values.
!>
!> @author Stephen Gilbert @date 2002-12-17
subroutine jpcunpack(cpack, len, idrstmpl, ndpts, fld)
  use iso_c_binding
  implicit none
  
  integer, intent(in) :: len
  character(len = 1), intent(in) :: cpack(len)
  integer, intent(in) :: ndpts
  integer, intent(in) :: idrstmpl(*)
  real, intent(out) :: fld(ndpts)
  
  integer(kind = 8) :: ndpts8, len8
  integer(kind = 8) :: idrstmpl8(7)
  integer(kind = 8) :: my_ierr
  integer :: i
  
  interface
#if KIND == 4
     function jpcunpack_c(cpack, len, idrstmpl, ndpts, fld) bind(c, name="g2c_jpcunpackf")
#else
     function jpcunpack_c(cpack, len, idrstmpl, ndpts, fld) bind(c, name="g2c_jpcunpackd")
#endif
       use iso_c_binding
       integer(c_size_t), value, intent(in) :: len       
       character(kind = c_char), intent(in) :: cpack(*)
       integer(kind = c_size_t), intent(in) :: idrstmpl(*)              
       integer(c_size_t), value, intent(in) :: ndpts       
#if KIND == 4
       real(c_float), intent(out) :: fld(*)
#else       
       real(c_double), intent(out) :: fld(*)
#endif
       integer(c_int) :: jpcunpack_c
     end function jpcunpack_c
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
  my_ierr = jpcunpack_c(cpack, len8, idrstmpl8, ndpts8, fld)

end subroutine jpcunpack
