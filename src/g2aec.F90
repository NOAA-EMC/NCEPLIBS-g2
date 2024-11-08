!> @file
!> @brief Pack/unpack a data field that was packed with AEC compression.
!> @author Eric Engle @date 2023-10-16

!> Pack a data field into a AEC code stream as defined in
!> [Data Representation Template
!> 5.42](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-42.shtml).
!>
!> After the data are scaled, and the reference value is subtracted
!> out, the data are passed to the AEC encoder.
!> 
!> This function also fills in GRIB2 Data Representation Template 5.42
!> with the appropriate values.
!>
!> @param[in] fld The data values to pack.
!> @param[in] width number of points in the x direction
!> @param[in] height number of points in the y direction
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template [Table
!> 5.42](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-42.shtml).
!> - idrstmpl(1) Reference value - ignored on input.
!> - idrstmpl(2) Binary Scale Factor.
!> - idrstmpl(3) Decimal Scale Factor.
!> - idrstmpl(4) Number of bits containing each grayscale pixel value
!> - idrstmpl(5) Original field type, currently set = 0 on output
!> Data values assumed to be reals.
!> - idrstmpl(6) CCSDS compression options mask.
!> - idrstmpl(7) Block size.
!> - idrstmpl(8) Reference sample interval.
!> @param[out] cpack The packed data field (character*1 array).
!> @param[inout] lcpack When function is called, contains the length
!> of buffer cpack. After functions returns, contains the length of
!> the packed data in bytes.
!>
!> @author Eric Engle @date 2023-10-16
subroutine aecpack(fld,width,height,idrstmpl,cpack,lcpack)

  use, intrinsic :: iso_c_binding, only: c_size_t
  implicit none
  
  integer,intent(in) :: width,height
  real,intent(in) :: fld(width*height)
  character(len=1),intent(out) :: cpack(*)
  integer,intent(inout) :: idrstmpl(*)
  integer,intent(inout) :: lcpack
  integer(c_size_t) :: width_c, height_c
  integer :: ret

  interface
   function g2c_aecpackd(fld, width, height, idrstmpl, cpack, lcpack) bind(c)
    use, intrinsic :: iso_c_binding
    real(kind=c_double), intent(in):: fld(*)
    integer(c_size_t), value, intent(in) :: width, height
    integer(c_int), intent(inout) :: idrstmpl(*)
    character(kind=c_char), intent(out) :: cpack(*)
    integer(c_int), intent(out) :: lcpack
    integer(c_int) :: g2c_aecpackd
   end function g2c_aecpackd
   function g2c_aecpackf(fld, width, height, idrstmpl, cpack, lcpack) bind(c)
    use, intrinsic :: iso_c_binding
    real(kind=c_float), intent(in):: fld(*)
    integer(c_size_t), value, intent(in) :: width, height
    integer(c_int), intent(inout) :: idrstmpl(*)
    character(kind=c_char), intent(out) :: cpack(*)
    integer(c_int), intent(out) :: lcpack
    integer(c_int) :: g2c_aecpackf
   end function g2c_aecpackf
  end interface

  width_c = width
  height_c = height
  
#if KIND==4
  ret = g2c_aecpackf(fld, width_c, height_c, idrstmpl, cpack, lcpack)
#else
  ret = g2c_aecpackd(fld, width_c, height_c, idrstmpl, cpack, lcpack)
#endif

end subroutine
  
!> Unpack a data field from a AEC code stream as defined in
!> [Data Representation Template
!> 5.42](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-42.shtml).
!>
!> @param[in] cpack The packed data field (character*1 array).
!> @param[in] len length of packed field cpack().
!> @param[in] idrstmpl Array of values for Data Representation
!> [Template
!> 5.42](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-42.shtml).
!> @param[in] ndpts The number of data values to unpack.
!> @param[out] fld Contains the unpacked data values.
!>
!> @author Eric Engle @date 2023-10-16
subroutine aecunpack(cpack,len,idrstmpl,ndpts,fld)
  
  use, intrinsic :: iso_c_binding, only: c_size_t
  implicit none
  
  character(len=1),intent(in) :: cpack(len)
  integer,intent(in) :: ndpts,len
  integer,intent(in) :: idrstmpl(*)
  real,intent(out) :: fld(ndpts)
  integer(c_size_t) :: ndpts_c, len_c
  integer :: ret

  interface
   function g2c_aecunpackd(cpack, len, idrstmpl, ndpts, fld) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none 
    integer(c_size_t), value, intent(in) :: len
    integer(c_size_t), value, intent(in) :: ndpts 
    character(kind=c_char), intent(in) :: cpack(*)
    integer(c_int), intent(in) :: idrstmpl(*)
    real(kind=c_double), intent(out) :: fld(*)
    integer(c_int) :: g2c_aecunpackd
   end function g2c_aecunpackd
   function g2c_aecunpackf(cpack, len, idrstmpl, ndpts, fld) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none 
    integer(c_size_t), value, intent(in) :: len
    integer(c_size_t), value, intent(in) :: ndpts 
    character(kind=c_char), intent(in) :: cpack(*)
    integer(c_int), intent(in) :: idrstmpl(*)
    real(kind=c_float), intent(out) :: fld(*)
    integer(c_int) :: g2c_aecunpackf
   end function g2c_aecunpackf
  end interface

  len_c = len
  ndpts_c = ndpts
#if KIND==4
  ret = g2c_aecunpackf(cpack, len_c, idrstmpl, ndpts_c, fld)
#else
  ret = g2c_aecunpackd(cpack, len_c, idrstmpl, ndpts_c, fld)
#endif
  
end subroutine aecunpack