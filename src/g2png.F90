!> @file
!> @brief Pack/unpack a data field into PNG image format, defined in
!> [Data Representation Template
!> 5.40](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-40.shtml).
!> @author Stephen Gilbert @date 2002-12-21

!> Pack a data field into PNG image format, defined in [Data
!> Representation Template
!> 5.41](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-41.shtml).
!>
!> After the data field is scaled, and the reference value is
!> subtracted out, it is treated as a grayscale image and passed to a
!> PNG encoder. It also fills in GRIB2 Data Representation Template
!> 5.41 or 5.40010 with the appropriate values.
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
!> @param[out] lcpack The length of packed field cpack.
!>
!> @author Stephen Gilbert @date 2002-12-21
subroutine pngpack(fld, width, height, idrstmpl, cpack, lcpack)

  use, intrinsic :: iso_c_binding, only: c_size_t
  implicit none
   
  integer, intent(in) :: width, height
  real, intent(in) :: fld(width * height)
  character(len = 1), intent(out) :: cpack(*)
  integer, intent(inout) :: idrstmpl(*)
  integer, intent(out) :: lcpack
  integer(c_size_t) :: width_c, height_c
  integer :: ret

  interface
     function g2c_pngpackd(fld, width, height, idrstmpl, cpack, lcpack) bind(c)
      use, intrinsic :: iso_c_binding
      real(kind=c_double), intent(in):: fld(*)
      integer(c_size_t), value, intent(in) :: width, height
      integer(c_int), intent(inout) :: idrstmpl(*)
      character(kind=c_char), intent(out) :: cpack(*)
      integer(c_int), intent(out) :: lcpack
      integer(c_int) :: g2c_pngpackd
     end function g2c_pngpackd
     function g2c_pngpackf(fld, width, height, idrstmpl, cpack, lcpack) bind(c)
      use, intrinsic :: iso_c_binding
      real(kind=c_float), intent(in):: fld(*)
      integer(c_size_t), value, intent(in) :: width, height
      integer(c_int), intent(inout) :: idrstmpl(*)
      character(kind=c_char), intent(out) :: cpack(*)
      integer(c_int), intent(out) :: lcpack
      integer(c_int) :: g2c_pngpackf
     end function g2c_pngpackf
  end interface

  width_c = width
  height_c = height

#if KIND==4
  ret = g2c_pngpackf(fld, width_c, height_c, idrstmpl, cpack, lcpack)
#else
  ret = g2c_pngpackd(fld, width_c, height_c, idrstmpl, cpack, lcpack)
#endif

end subroutine pngpack

!> Unpack a data field with PNG, defined in [Data Representation
!> Template
!> 5.40](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-40.shtml).
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

  use, intrinsic :: iso_c_binding, only: c_size_t
  implicit none
  
  character(len = 1), intent(in) :: cpack(len)
  integer, intent(in) :: ndpts, len
  integer, intent(in) :: idrstmpl(*)
  real, intent(out) :: fld(ndpts)
  integer(c_size_t) :: ndpts_c, len_c
  integer :: ret

  interface
   function g2c_pngunpackd(cpack, len, idrstmpl, ndpts, fld) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none 
    integer(c_size_t), value, intent(in) :: len
    integer(c_size_t), value, intent(in) :: ndpts 
    character(kind=c_char), intent(in) :: cpack(*)
    integer(c_int), intent(in) :: idrstmpl(*)
    real(kind=c_double), intent(out) :: fld(*)
    integer(c_int) :: g2c_pngunpackd
   end function g2c_pngunpackd
   function g2c_pngunpackf(cpack, len, idrstmpl, ndpts, fld) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none 
      integer(c_size_t), value, intent(in) :: len
      integer(c_size_t), value, intent(in) :: ndpts 
      character(kind=c_char), intent(in) :: cpack(*)
      integer(c_int), intent(in) :: idrstmpl(*)
      real(kind=c_float), intent(out) :: fld(*)
      integer(c_int) :: g2c_pngunpackf
   end function g2c_pngunpackf
  end interface

  len_c = len
  ndpts_c = ndpts
#if KIND==4
  ret = g2c_pngunpackf(cpack, len_c, idrstmpl, ndpts_c, fld)
#else
  ret = g2c_pngunpackd(cpack, len_c, idrstmpl, ndpts_c, fld)
#endif

end subroutine pngunpack
