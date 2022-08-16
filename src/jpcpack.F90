!> @file
!> @brief Pack up a data field into a JPEG2000 code stream.
!> @author Stephen Gilbert @date 2002-12-17

!> Pack up a data field into a JPEG2000 code stream.
!>
!> After the data field is scaled, and the reference value is
!> subtracted out, it is treated as a grayscale image and passed to a
!> JPEG2000 encoder. It also fills in [GRIB2 - TABLE 5.40 TYPE OF
!> COMPRESSION](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-40.shtml).
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2002-12-17 | Stephen Gilbert | Initial Development.
!> 2002-12-29 | Stephen Gilbert | check jpeg2000 encoding for success. If not, try again.
!> 2022-08-10 | Hartnett | Adding interfaces for C functions.
!>
!> @param[in] fld The data values to pack.
!> @param[in] width number of points in the x direction
!> @param[in] height number of points in the y direction
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3
!> - idrstmpl(1) Reference value - ignored on input.
!> - idrstmpl(2) Binary Scale Factor.
!> - idrstmpl(3) Decimal Scale Factor.
!> - idrstmpl(4) Number of bits containing each grayscale pixel value
!> - idrstmpl(5) Original field type, currently set = 0 on output
!> Data values assumed to be reals.
!> - idrstmpl(6) = 0 use lossless compression; = 1 use lossy compression.
!> - idrstmpl(7) Desired compression ratio, if idrstmpl(6)=1.
!> @param[out] cpack The packed data field (character*1 array).
!> @param[out] lcpack length of packed field cpack.
!>
!> @author Stephen Gilbert @date 2002-12-17
subroutine jpcpack(fld, width, height, idrstmpl, cpack, lcpack)
  implicit none

  integer, intent(in) :: width, height
  real, intent(in) :: fld(width * height)
  character(len = 1), intent(out) :: cpack(*)
  integer, intent(inout) :: idrstmpl(*)
  integer, intent(inout) :: lcpack
  
  integer(kind = 8) :: width8, height8, lcpack8
  integer(kind = 8) :: idrstmpl8(7)
  integer :: i
  
  interface
#if KIND == 4
     subroutine jpcpack_c(fld, my_width, height, idrstmpl, cpack, lcpack) bind(c, name="jpcpack")
#else
     subroutine jpcpack_c(fld, my_width, height, idrstmpl, cpack, lcpack) bind(c, name="jpcpackd")
#endif
       use iso_c_binding
       integer(c_size_t), intent(in) :: my_width, height
#if KIND == 4
       real(c_float), intent(in) :: fld(my_width * height)
#else       
       real(c_double), intent(in) :: fld(my_width * height)
#endif
       integer(kind = c_size_t), intent(in) :: idrstmpl(*)              
       character(kind = c_char), intent(in) :: cpack(*)              
       integer(c_size_t), value :: lcpack
     end subroutine jpcpack_c
  end interface

  ! We need these parameters as 8-byte ints for the C function.
  width8 = width
  height8 = height
  lcpack8 = lcpack
  
  ! Need to copy idrstmpl array to 8-byte int array for the C
  ! function.
  do i = 1, 7
     idrstmpl8(i) = idrstmpl(i)
  end do

  ! Call the C function.
  call jpcpack_c(fld, width8, height8, idrstmpl8, cpack, lcpack8)

  ! Need to copy idrstmpl array from 8-byte back to 4-byte.
  do i = 1, 7
     idrstmpl(i) = int(idrstmpl8(i))
  end do
  lcpack = int(lcpack8)
  
end subroutine jpcpack
