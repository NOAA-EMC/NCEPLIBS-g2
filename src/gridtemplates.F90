!> @file
!> @brief This Fortran module contains info on all the available
!> GRIB2 Grid Definition Templates used in Section 3 - the Grid
!> Definition Section (GDS).
!> @author Stephen Gilbert @date 2000-05-09

!> This Fortran module contains info on all the available GRIB2 Grid
!> Definition Templates used in [Section 3 - the Grid Definition
!> Section
!> (GDS)](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml).
!>
!> Each Template has three parts:
!> 1. The number of entries in the template (mapgridlen);
!> 2. A map of the template (mapgrid), which contains the number of
!> octets in which to pack each of the template values;
!> 3. A logical value (needext) that indicates whether the Template
!> needs to be extended. In some cases the number of entries in a
!> template can vary depending upon values specified in the "static"
!> part of the template. (See Template 3.120 as an example).
!>
!> This module also contains two subroutines:
!> * getgridtemplate() returns the octet map for a specified Template
!> number
!> * extgridtemplate() calculates the extended octet map of a template
!> that needs extension.
!>
!> Array mapgrid contains the number of bytes in which the
!> corresponding template values will be stored. A negative value in
!> mapgrid is used to indicate that the corresponding template entry
!> can contain negative values. This information is used later when
!> packing/unpacking the template data values.
!>
!> Negative data values in GRIB are stored with the left most bit set to
!> one, and a negative number of bytes value in mapgrid indicates that
!> this possibility should be considered. The number of bytes used to
!> store the data value in this case would be the absolute value of the
!> negative value in mapgrid.
!>
!> @author Stephen Gilbert @date 2000-05-09
module gridtemplates
  implicit none
  
  integer, parameter :: MAXLEN = 200 !< maximum number of octets in mapgrid
  integer, parameter :: MAXTEMP = 31 !< maximum number of entries in the template

  interface
     function g2c_get_grid_template(number, nummap, map, needext) bind(c)
      use, intrinsic :: iso_c_binding
      integer(c_int), value, intent(in) :: number
      integer(c_int), intent(out) :: nummap
      integer(c_int), intent(out) :: map(*)
      integer(c_int), intent(out) :: needext
      integer(c_int) :: g2c_get_grid_template
     end function g2c_get_grid_template
     function g2c_get_grid_template_extension(number, list, extlen, ext) bind(c)
      use, intrinsic :: iso_c_binding
      integer(c_int), value, intent(in) :: number
      integer(c_int), intent(in) :: list(*)
      integer(c_int), intent(out) :: extlen
      integer(c_int), intent(out) :: ext(*)
      integer(c_int) :: g2c_get_grid_template_extension
     end function g2c_get_grid_template_extension
     function g2c_get_gdt_len(number, nummap) bind(c)
      use, intrinsic :: iso_c_binding
      integer(c_int), value, intent(in) :: number
      integer(c_int), intent(out) :: nummap
      integer(c_int) :: g2c_get_gdt_len
     end function g2c_get_gdt_len
  end interface

contains

  !> Get the grid template information for a specified Grid Definition
  !> Template.
  !>
  !> The number of entries in the template is returned along with a
  !> map of the number of octets occupied by each entry. Also, a flag
  !> is returned to indicate whether the template would need to be
  !> extended.
  !>
  !> @param[in] number NN, indicating the number of the Grid
  !> Definition Template that is being requested.
  !> @param[out] nummap Number of entries in the Template.
  !> @param[out] map An array containing the number of octets that
  !> each template entry occupies when packed up into the GDS.
  !> @param[out] needext Logical variable indicating whether the Grid
  !> Defintion Template has to be extended.
  !> @param[out] iret Error return code.
  !> - 0 no error.
  !> - 1 Undefine Grid Template number.
  !>
  !> @author Stephen Gilbert @date 2000-05-09
  subroutine getgridtemplate(number, nummap, map, needext, iret)
    implicit none

    integer, intent(in) :: number
    integer, intent(out) :: nummap, map(*), iret
    logical, intent(out) :: needext
    integer :: needext_int

    iret = g2c_get_grid_template(number, nummap, map, needext_int)

    if (iret .ne. 0) then
      nummap = 0
      needext = .false.
    else if (needext_int .eq. 1) then
      needext = .true.
    else
      needext = .false.
    endif
  end subroutine getgridtemplate

  !> Generate the remaining octet map for a given Grid Definition
  !> Template, if required.
  !>
  !> Some Templates can vary depending on data values given in an
  !> earlier part of the Template, and it is necessary to know some of
  !> the earlier entry values to generate the full octet map of the
  !> Template.
  !>
  !> @param[in] number NN, indicating the number of the Grid
  !> Definition Template 3.NN that is being requested.
  !> @param[in] list The list of values for each entry in the Grid
  !> Definition Template.
  !> @param[out] nummap Number of entries in the Template.
  !> @param[out] map An array containing the number of octets that
  !> each template entry occupies when packed up into the GDS.
  !>
  !>    @author Stephen Gilbert @date 2000-05-09
  subroutine extgridtemplate(number, list, nummap, map)
    implicit none

    integer, intent(in) :: number, list(*)
    integer, intent(out) :: nummap, map(*)
    integer :: iret, i, extlen, ext(MAXLEN)
    logical :: needext

    iret = getgdtlen(number)

    if (iret .ne. 0) return

    call getgridtemplate(number, nummap, map(1:nummap), needext, iret)

    if (iret .ne. 0) return
    if (.not. needext) return

    iret = g2c_get_grid_template_extension(number, list, extlen, ext)
    do i=1,extlen
      map(nummap+i) = ext(i)
    enddo
    nummap = nummap + extlen
  end subroutine extgridtemplate

  !> This function returns the initial length (number of entries) in
  !> the static part of specified Grid Definition Template.
  !>
  !> @param[in] number NN,  indicating the number of the Grid
  !> Definition Template that is being requested.
  !>
  !> @return Number of entries in the static part of the grid
  !> definition template, or 0, if requested template is not found.
  !>
  !> @author Stephen Gilbert @date 2004-05-11
  integer function getgdtlen(number)
    implicit none

    integer, intent(in) :: number
    integer :: iret, nummap

    getgdtlen = 0
    iret = g2c_get_gdt_len(number, nummap)
    if (iret .ne. 0) return

    getgdtlen = nummap
  end function getgdtlen
end module gridtemplates