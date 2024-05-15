!> @file
!> @brief Logging for the g2 library.
!> @author Edward Hartnett @date 5/15/24

!> @brief Logging for the g2 library.
!>
!> Set g2_log_level to turn on logging.
!>
!> @author Edward Hartnett @date 5/15/24
module g2logging
  integer g2_log_level !< 0 for no logging.
  character* 120 g2_log_msg !< Fill this with the logging message.

contains
  !> Print a debug message for the g2 library.
  !>
  !> @param[in] level If this is lower or equal to the current setting
  !> of g2_log_level, print this log message.
  !>
  !> @author Edward Hartnett @date 5/15/24
  subroutine g2_log(level)
    integer, intent(in) :: level

    if (level .le. g2_log_level) then
       print *, g2_log_msg
    endif
  end subroutine g2_log
end module g2logging
