!> @file
!> @brief Set the log level for internal library debugging.
!> @author Ed Hartnett @date 2022-08-16

!> Set the log level for internal library debugging.
!>

!> Turn on internal logging for the NCEPLIBS-g2c library. This is only
!> useful to programmers working on the GRIB libraries.
!>
!> If the NCEPLIBS-g2c library was not built with LOGGING=ON, then
!> setting the log level will have no effect.
!>
!> @param[in] log_level Set to -1 to show nothing, 0 for errors only,
!> and 1 or higher for progressively more verbose logging levels.
!> @return 0 for success.
!>
!> @author Ed Hartnett @date 2022-08-16
function g2_set_log_level(log_level)
  implicit none

  integer, intent(in) :: log_level
  integer g2_set_log_level

  interface
     function g2c_set_log_level(log_level) bind(c, name="g2_set_log_level")
       use iso_c_binding
       integer(c_int), value :: log_level
       integer(c_int) :: g2c_set_log_level
     end function g2c_set_log_level
  end interface

  g2_set_log_level = g2c_set_log_level(log_level)
  
end function g2_set_log_level
