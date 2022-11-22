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
  integer :: ierr

  interface
#if KIND == 4
     function pngunpack_c(cpack, len8, idrstmpl, ndpts, fld) &
          bind(c, name="g2c_pngunpackf")
#else
     function pngunpack_c(cpack, len8, idrstmpl, ndpts, fld) &
          bind(c, name="g2c_pngunpackd")
#endif
       use iso_c_binding
       integer(c_size_t), value :: len8       
       character(kind = c_char), intent(in) :: cpack(len8)
       integer(kind = c_int), intent(in) :: idrstmpl(*)              
       integer(c_size_t), value :: ndpts       
#if KIND == 4
       real(c_float), intent(out) :: fld(*)
#else       
       real(c_double), intent(out) :: fld(*)
#endif
       integer(c_int) :: pngunpack_c
     end function pngunpack_c
  end interface

  ! We need these parameters as 8-byte ints for the C function.
  ndpts8 = ndpts
  len8 = len

  ! Call the C function.
  ierr = pngunpack_c(cpack, len8, idrstmpl, ndpts8, fld)
  
end subroutine pngunpack
