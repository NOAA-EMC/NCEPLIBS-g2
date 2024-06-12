! Contains a module with interfaces to the C functions in the g2c
! library.
!
! Note: this file is excluded from the doxygen build (see Doxyfile.in)
! because these functions are already documented in the g2c library.
!
! Edward Hartnett, 6/12/2024
module g2c_interface
  interface
     function g2c_open(path, mode, g2idp) bind(c)
       use iso_c_binding, only: c_char, c_int
       character(kind = c_char), intent(in)  :: path(*)
       integer(c_int), value       :: mode
       integer(c_int), intent(out) :: g2idp
       integer(c_int) :: g2c_open
     end function g2c_open
  end interface
end module g2c_interface
