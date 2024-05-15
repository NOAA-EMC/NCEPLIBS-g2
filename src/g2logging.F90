!> @file
!> @brief Logging for the g2 library.
!> @author Edward Hartnett @date 5/15/24

!> @brief Logging for the g2 library.
!>
!> @author Edward Hartnett @date 5/15/24
module g2logging
  integer g2_log_level !< 0 for no logging.
  character* 120 g2_log_msg !< For messages.

contains
  subroutine g2_log(level)
    integer, intent(in) :: level

    if (level .le. g2_log_level) then
       print *, g2_log_msg
    endif
  end subroutine g2_log
end module g2logging
