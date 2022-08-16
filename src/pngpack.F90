!> @file
!> @brief This subroutine packs up a data field into PNG image format.
!> @author Stephen Gilbert @date 2002-12-21

!> This subroutine packs up a data field into PNG image format. After
!> the data field is scaled, and the reference value is subtracted out,
!> it is treated as a grayscale image and passed to a PNG encoder. It
!> also fills in GRIB2 Data Representation Template 5.41 or 5.40010
!> with the appropriate values.
!>
!> @param[in] fld Contains the data values to pack.
!> @param[in] width number of points in the x direction.
!> @param[in] height number of points in the y direction.
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3.
!> - idrstmpl(1) Reference value - ignored on input.
!> - idrstmpl(2) Binary Scale Factor.
!> - idrstmpl(3) Decimal Scale Factor.
!> - idrstmpl(4) Number of bits containing each grayscale pixel value.
!> - idrstmpl(5) Original field type, currently set = 0 on output
!> Data values assumed to be reals.
!> - idrstmpl(6) = 0 use lossless compression; = 1 use lossy compression.
!> - idrstmpl(7) Desired compression ratio, if idrstmpl(6)=1.
!> @param[out] cpack The packed data field (character*1 array)
!> @param[out] lcpack length of packed field cpack.
!>
!> @author Stephen Gilbert @date 2002-12-21
subroutine pngpack(fld, width, height, idrstmpl, cpack, lcpack)
  implicit none

  integer, intent(in) :: width, height
  real, intent(in) :: fld(width * height)
  character(len = 1), intent(out) :: cpack(*)
  integer, intent(inout) :: idrstmpl(*)
  integer, intent(out) :: lcpack

  integer(kind = 8) :: width8, height8, lcpack8
  integer(kind = 8) :: idrstmpl8(7)
  integer :: i
  
  interface
#if KIND == 4
     subroutine pngpack_c(fld, width, height, idrstmpl, cpack, lcpack) bind(c, name="pngpack")
#else
     subroutine pngpack_c(fld, width, height, idrstmpl, cpack, lcpack) bind(c, name="pngpackd")
#endif
       use iso_c_binding
       integer(c_size_t), intent(in) :: width, height
#if KIND == 4
       real(c_float), intent(in) :: fld(width * height)
#else       
       real(c_double), intent(in) :: fld(width * height)
#endif
       integer(kind = c_size_t), intent(in) :: idrstmpl(*)              
       character(kind = c_char), intent(in) :: cpack(*)              
       integer(c_size_t), intent(out) :: lcpack
     end subroutine pngpack_c
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
  call pngpack_c(fld, width8, height8, idrstmpl8, cpack, lcpack8)

  ! Need to copy idrstmpl array from 8-byte back to 4-byte.
  do i = 1, 7
     idrstmpl(i) = int(idrstmpl8(i))
  end do
  lcpack = lcpack8
end subroutine pngpack
