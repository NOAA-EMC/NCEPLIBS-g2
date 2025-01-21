!> @file
!> @brief Information on all GRIB2 Product Definition Templates used
!> in [Section 4 - the Product Definition Section
!> (PDS)](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect4.shtml)
!> @author Stephen Gilbert @date 2000-05-11

!> @brief Information on all GRIB2 Product Definition Templates used
!> in [Section 4 - the Product Definition Section
!> (PDS)](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect4.shtml)
!>
!> Each Template has three parts:
!> 1. The number of entries in the template (mapppdslen);
!> 2. A map of the template (mappds), which contains the number of
!> octets in which to pack each of the template values;
!> 3.  a logical value (needext) that indicates whether the Template
!> needs to be extended. In some cases the number of entries in a
!> template can vary depending upon values specified in the static
!> part of the template. (Template 4.3 as an example).
!>
!> This module also contains two subroutines.
!> - getpdstemplate() returns the octet map for a specified
!> Template number.
!> - extpdstemplate() will calculate the extended octet map of an
!> appropriate template given values for the static part of the
!> template.
!>
!> @note Array mapgrid contains the number of octets in which the
!> corresponding template values will be stored. A negative value in
!> mapgrid is used to indicate that the corresponding template entry
!> can contain negative values. This information is used later when
!> packing (or unpacking) the template data values. Negative data
!> values in GRIB are stored with the left most bit set to one, and
!> a negative number of octets value in mapgrid indicates that this
!> possibility should be considered. The number of octets used to
!> store the data value in this case would be the absolute value of
!> the negative value in mapgrid.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-05-11 | Gilbert | Initial
!> 2025-01-21 | Stahl | Replaced subroutines w/ call to g2c, removed template array
!>
!> @author Stephen Gilbert @date 2000-05-11
module pdstemplates

contains

  !> This subroutine returns PDS template information for a specified
  !> Product Definition Template. The number of entries in the
  !> template is returned along with a map of the number of octets
  !> occupied by each entry. Also,  a flag is returned to indicate
  !> whether the template would need to be extended.
  !>
  !> @param[in] number the Product Definition Template number that is
  !> being requested.
  !> @param[out] nummap Number of entries in the Template.
  !> @param[out] map An array containing the number of octets that each
  !> template entry occupies when packed up into the PDS.
  !> @param[out] needext Logical variable indicating whether the
  !> Product Defintion Template has to be extended.
  !> @param[out] iret Error return code.
  !> - 0 no error.
  !> - 1 Undefine Product Template number.
  !>
  !> @author  Stephen Gilbert  @date 2000-05-11
  subroutine getpdstemplate(number, nummap, map, needext, iret)

    use, intrinsic :: iso_c_binding
    implicit none

    integer, intent(in) :: number
    integer, intent(out) :: nummap, map(*), iret
    logical, intent(out) :: needext
    integer :: needext_int

    interface
       function g2c_get_pds_template(number, nummap, map, needext) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), value, intent(in) :: number
        integer(c_int), intent(out) :: nummap
        integer(c_int), intent(out) :: map(*)
        integer(c_int), intent(out) :: needext
        integer(c_int) :: g2c_get_pds_template
       end function g2c_get_pds_template
    end interface

    iret = g2c_get_pds_template(number, nummap, map, needext_int)

    needext = needext_int
  end subroutine getpdstemplate

  !> This subroutine generates the remaining octet map for a given
  !> Product Definition Template, if required. Some Templates can
  !> vary depending on data values given in an earlier part of the
  !> Template, and it is necessary to know some of the earlier entry
  !> values to generate the full octet map of the Template.
  !>
  !> @param[in] number the Product Definition Template number.
  !> @param[in] list An array containing the number of octets that match
  !> the Product Definition Template.
  !> @param[out] nummap Number of entries in the Template.
  !> @param[out] map An array containing the number of octets that each
  !> template entry occupies when packed up into the PDS.
  !>
  !> @author Stephen Gilbert @date 2000-05-11
  subroutine extpdstemplate(number, list, nummap, map)

    use, intrinsic :: iso_c_binding
    implicit none

    integer, intent(in) :: number, list(*)
    integer, intent(out) :: nummap, map(*)
    integer :: iret, i, extlen, ext(*), temp_map(*)
    logical :: needext

    interface
       function g2c_get_pds_template_extension(number, list, extlen, ext) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), value, intent(in) :: number
        integer(c_int), intent(in) :: list(*)
        integer(c_int), intent(out) :: extlen
        integer(c_int), intent(out) :: ext(*)
        integer(c_int) :: g2c_get_pds_template_extension
       end function g2c_get_pds_template_extension
    end interface

    call getpdstemplate(number, nummap, temp_map, needext, iret)

    if (iret .gt. 0) return
    
    if (.not. needext) return

    iret = g2c_get_pds_template_extension(number, list, extlen, ext)
    map(1:nummap) = temp_map(1:nummap)
    nummap = nummap + extlen

    do i=1,extlen
      map(nummap+i) = ext(i)
    enddo

  end subroutine extpdstemplate
end module pdstemplates
