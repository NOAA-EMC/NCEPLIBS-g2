!> @file
!> @brief Pack up a data field using a complex packing algorithm.
!> @author Stephen Gilbert @date 2004-08-27

!> Pack up a data field using a complex packing algorithm.
!>
!> This subroutine supports GRIB2 complex packing templates with or
!> without spatial differences, Data Representation Templates (DRT)
!> [GRIB2 - DATA REPRESENTATION TEMPLATE 5.2 - Grid point data -
!> complex
!> packing](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-2.shtml)
!> and [GRIB2 - DATA REPRESENTATION TEMPLATE 5.3 - Grid point data -
!> complex packing and spatial
!> differencing](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-3.shtml).
!>
!> It also fills in GRIB2 Data Representation Template 5.2 or 5.3 with
!> the appropriate values.
!>
!> @param[in] fld The data values to pack.
!> @param[in] ndpts The number of data values in array fld.
!> @param[in] idrsnum Data Representation Template number. Must equal
!> 2 or 3.
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3
!> - 1 Reference value - ignored on input
!> - 2 Binary Scale Factor
!> - 3 Decimal Scale Factor
!> - 7 Missing value management
!> - 8 Primary missing value
!> - 9 Secondary missing value
!> - 17 Order of Spatial Differencing  (1 or 2)
!> @param[out] cpack The packed data field (character*1 array).
!> @param[out] lcpack Length of packed field cpack. -1 is returned if
!> idrstmpl(7) is not set correctly.
!>
!> @author Stephen Gilbert @date 2004-08-27
subroutine cmplxpack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)

  integer,intent(in) :: ndpts,idrsnum
  real,intent(in) :: fld(ndpts)
  character(len=1),intent(out) :: cpack(*)
  integer,intent(inout) :: idrstmpl(*)
  integer,intent(out) :: lcpack

  if ( idrstmpl(7) .eq. 0 ) then       ! No internal missing values
     call compack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
  elseif ( idrstmpl(7).eq.1 .OR. idrstmpl(7).eq.2) then
     call misspack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
  else
     print *,'cmplxpack: Do not recognize Missing value option.'
     lcpack=-1
  endif

  return
end subroutine cmplxpack
