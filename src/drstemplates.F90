!> @file
!> @brief Handles Data Representation Templates used in Section 5.
!> @author Stephen Gilbert @date 2001-04-03

!> Handles Data Representation Templates used in Section 5.  (See
!> https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect5.shtml.)
!>
!> Each Template has three parts:
!> 1. The number of entries in the template (mapdrslen);
!> 2. A map of the template (mapdrs), which contains the number of
!> octets in which to pack each of the template values;
!> 3. A logical value (needext) that indicates whether the Template
!> needs to be extended.
!>
!> This module also contains two subroutines. Subroutine
!> getdrstemplate() returns the octet map for a specified Template
!> number, and subroutine extdrstemplate() will calculate the extended
!> octet map of an appropriate template given values for the "static"
!> part of the template.
!>
!> @note Array mapdrs contains the number of octets in which the
!> corresponding template values will be stored. A negative value in
!> mapdrs is used to indicate that the corresponding template entry
!> can contain negative values. This information is used later when
!> packing (or unpacking) the template data values. Negative data
!> values in GRIB are stored with the left most bit set to one, and a
!> negative number of octets value in mapdrs indicates that this
!> possibility should be considered. The number of octets used to
!> store the data value in this case would be the absolute value of
!> the negative value in mapdrs.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-04-03 | Gilbert | Initial
!> 2025-01-21 | Stahl | Replaced subroutines w/ call to g2c, removed template array
!>
!> @author Stephen Gilbert @date 2001-04-03
module drstemplates
  implicit none

  interface
     function g2c_get_drs_template(number, nummap, map, needext) bind(c)
      use, intrinsic :: iso_c_binding
      integer(c_int), value, intent(in) :: number
      integer(c_int), intent(out) :: nummap
      integer(c_int), intent(out) :: map(*)
      integer(c_int), intent(out) :: needext
      integer(c_int) :: g2c_get_drs_template
     end function g2c_get_drs_template
  end interface

contains

  !> Return DRS template information for a specified Data
  !> Representation Template.
  !>
  !> The number of entries in the template is returned along with a map
  !> of the number of octets occupied by each entry. Also, a flag is
  !> returned to indicate whether the template would need to be extended.
  !>
  !> @param[in] number NN, indicating the number of the Data Representation
  !> Template 5.NN that is being requested.
  !> @param[out] nummap Number of entries in the Template
  !> @param[out] map An array containing the number of octets that each
  !> template entry occupies when packed up into the DRS.
  !> @param[out] needext Logical variable indicating whether the Data Representation
  !> Template has to be extended.
  !> @param[out] iret Error return code.
  !> - 0 = no error
  !> - 1 = Undefined Data Representation Template number.
  !>
  !> @author Stephen Gilbert @date 2000-05-11
  subroutine getdrstemplate(number, nummap, map, needext, iret)

    use, intrinsic :: iso_c_binding
    implicit none

    integer, intent(in) :: number
    integer, intent(out) :: nummap, map(*), iret
    logical, intent(out) :: needext
    integer :: needext_int

    iret = g2c_get_drs_template(number, nummap, map, needext_int)

    needext = needext_int > 0

  end subroutine getdrstemplate

  !> Generate the remaining octet map for a given Data
  !> Representation Template, if required.
  !>
  !> Some Templates can vary depending on data values given in an
  !> earlier part of the Template, and it is necessary to know some of
  !> the earlier entry values to generate the full octet map of the
  !> Template.
  !>
  !> Currently no templates have been implemented which require an
  !> extension.
  !>
  !> @param[in] number NN, indicating the number of the Data
  !> Representation Template 5.NN that is being requested.
  !> @param[in] list The list of values for each entry in the Data
  !> Representation Template 5.NN.
  !> @param[out] nummap Number of entries in the Template
  !> @param[out] map An array containing the number of octets that each
  !> template entry occupies when packed up into the GDS.
  !>
  !> @author  Stephen Gilbert @date 2000-05-11
  subroutine extdrstemplate(number, list, nummap, map)
    implicit none

    integer, intent(in) :: number, list(*)
    integer, intent(out) :: nummap, map(*)
    integer :: iret
    logical :: needext

    call getdrstemplate(number, nummap, map, needext, iret)

    if (iret .gt. 0) return

    ! No implemented DRS templates need extensions.
    if (.not. needext) return

  end subroutine extdrstemplate
end module drstemplates
