!>    @file
!>    @brief This subroutine packs up a data field using a complex
!>    packing algorithm as defined in the GRIB2 documention.
!>    @author Gilbert ORG: W/NP11 @date 2004-08-27
!>     
!>    This subroutine supports GRIB2 complex packing templates with or without
!>    spatial differences (i.e. DRTs 5.2 and 5.3).
!>    It also fills in GRIB2 Data Representation Template 5.2 or 5.3 
!>    with the appropriate values.
!>    
!>    @param[in] fld() Contains the data values to pack
!>    @param[in] ndpts The number of data values in array fld()
!>    @param[in] idrsnum Data Representation Template number 5.N Must equal 2 or 3.
!>    @param[inout] idrstmpl Contains the array of values for Data Representation Template 5.2 or 5.3
!>    - (1) = Reference value - ignored on input set by compack routine.
!>    - (2) = Binary Scale Factor
!>    - (3) = Decimal Scale Factor
!>    - (7) = Missing value management
!>    - (8) = Primary missing value
!>    - (9) = Secondary missing value
!>    - (17) = Order of Spatial Differencing  ( 1 or 2 )
!>    @param[out] pack The packed data field (character*1 array)
!>    @param[out] lcpack length of packed field cpack().
!>
!>    @author Gilbert ORG: W/NP11 @date 2004-08-27
!>

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
      end
