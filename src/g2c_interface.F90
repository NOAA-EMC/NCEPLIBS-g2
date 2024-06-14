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
       use iso_c_binding
       character(kind = c_char), intent(in)  :: path(*)
       integer(c_int), value :: mode
       integer(c_int), intent(out) :: g2idp
       integer(c_int) :: g2c_open
     end function g2c_open

     function g2c_open_index(data_file, index_file, mode, g2cid) bind(c)
       use iso_c_binding
       character(kind=c_char), intent(in)  :: data_file(*), index_file(*)
       integer(c_int), value :: mode
       integer(c_int), intent(out) :: g2cid
       integer(c_int) :: g2c_open_index
     end function g2c_open_index

     function g2c_close(g2id) result(status)
       use iso_c_binding
       integer(c_int), intent(in) :: g2id
       integer(c_int) :: status
     end function g2c_close
  end interface
end module g2c_interface
