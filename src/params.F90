!> @file
!> @brief Info on all the available GRIB Parameters.
!> @author Stephen Gilbert @date 2001-06-05

!> This Fortran Module contains info on all the available GRIB
!> Parameters, and their GRIB1 and GRIB2 codes. If both of the GRIB1
!> codes are 255, that means the parameters is "UNKNOWN" in GRIB1 and
!> cannot be converted to GRIB1.
!>
!> @author Stephen Gilbert @date 2001-06-05
!> @author Brent Gordon, Boi Vuong
module params

  implicit none
  integer, parameter :: MAXPARAM = 2000 !< maximum number of GRIB parameters.

  interface
   function g2c_param_g1tog2(g1num, g1ver, g2disc, g2cat, g2num) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int), value, intent(in) :: g1num
    integer(c_int), value, intent(in) :: g1ver
    integer(c_int), intent(out) :: g2disc
    integer(c_int), intent(out) :: g2cat
    integer(c_int), intent(out) :: g2num
    integer(c_int) :: g2c_param_g1tog2
   end function g2c_param_g1tog2
   function g2c_param_abbrev(g2disc, g2cat, g2num, abbrev) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int), value, intent(in) :: g2disc
    integer(c_int), value, intent(in) :: g2cat
    integer(c_int), value, intent(in) :: g2num
    character(kind=c_char), intent(out) :: abbrev(*)
    integer(c_int) :: g2c_param_abbrev
   end function g2c_param_abbrev
   function g2c_param_g2tog1(g2disc, g2cat, g2num, g1num, g1ver) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int), value, intent(in) :: g2disc
    integer(c_int), value, intent(in) :: g2cat
    integer(c_int), value, intent(in) :: g2num
    integer(c_int), intent(out) :: g1num
    integer(c_int), intent(out) :: g1ver
    integer(c_int) :: g2c_param_g2tog1
   end function g2c_param_g2tog1
  end interface

contains

  !> This subroutine returns the corresponding GRIB2 Discipline
  !> Category and Number for a given GRIB1 parameter value and table
  !> version.
  !>
  !> @param[in] g1val GRIB1 parameter number for which discipline is
  !> requested.
  !> @param[in] g1ver GRIB1 parameter table version number.
  !> @param[out] g2disc corresponding GRIB2 Discipline number.
  !> @param[out] g2cat corresponding GRIB2 Category number.
  !> @param[out] g2num corresponding GRIB2 Parameter number within
  !> Category g2cat.
  !>
  !> @author Stephen Gilbert @date 2001-06-05
  subroutine param_g1_to_g2(g1val, g1ver, g2disc, g2cat, g2num)
    implicit none

    integer, intent(in) :: g1val, g1ver
    integer, intent(out) :: g2disc, g2cat, g2num
    integer :: iret

    g2disc = 255
    g2cat = 255
    g2num = 255

    iret = g2c_param_g1tog2(g1val, g1ver, g2disc, g2cat, g2num)

    if (iret .ne. 0) then
      print *, 'param_g1_to_g2:GRIB1 param ', g1val, ' not found.', &
          ' for table version ', g1ver
    end if

  end subroutine param_g1_to_g2

  !> This function returns the parameter abbreviation for
  !> a given GRIB2 Discipline, Category and Parameter number.
  !>
  !> @param[in] g2disc GRIB2 discipline number (See Code Table 0.0).
  !> @param[in] g2cat corresponding GRIB2 Category number.
  !> @param[in] g2num corresponding GRIB2 Parameter number within
  !> Category g2cat.
  !> @return parameter abbreviation for GRIB2 info.
  !>
  !> @author Stephen Gilbert @date 2002-01-04
  character(len = 8) function param_get_abbrev(g2disc, g2cat, g2num)
    use, intrinsic :: iso_c_binding, only : c_char, c_null_char
    implicit none

    integer, intent(in) :: g2disc, g2cat, g2num
    integer :: iret, i
    character(c_char) :: abbrev(8)

    iret = g2c_param_abbrev(g2disc, g2cat, g2num, abbrev)
    param_get_abbrev = ""
    do i=1,8
      if (abbrev(i) == C_NULL_CHAR) exit
      param_get_abbrev(i:i) = abbrev(i)
    end do
  end function param_get_abbrev

  !> This subroutine returns the GRIB 1 parameter number for
  !> a given GRIB2 Discipline, Category and Parameter number.
  !>
  !> @param[in] g2disc GRIB2 Discipline number (See Code Table 0.0).
  !> @param[in] g2cat corresponding GRIB2 Category number.
  !> @param[in] g2num corresponding GRIB2 Parameter number within
  !> Category g2cat.
  !> @param[out] g1val GRIB1 parameter number for which discipline is
  !> requested.
  !> @param[out] g1ver GRIB1 parameter table version number.
  !>
  !> @author Stephen Gilbert @date 2002-01-04
  subroutine param_g2_to_g1(g2disc, g2cat, g2num, g1val, g1ver)
    implicit none

    integer, intent(in) :: g2disc, g2cat, g2num
    integer, intent(out) :: g1val, g1ver
    integer :: iret

    g1val = 255
    g1ver = 255

    iret = g2c_param_g2tog1(g2disc, g2cat, g2num, g1val, g1ver)

    if (iret .ne. 0) then
      print *, 'param_g2_to_g1:GRIB2 param ', g2disc, g2cat, &
          g2num, ' not found.'
    end if

  end subroutine param_g2_to_g1

end module params
