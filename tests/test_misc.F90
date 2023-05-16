! This is a test program for NCEPLIBS-g2.
!
! This program tests misc stuff.
!
! Ed Hartnett 5/16/23
program test_misc
  implicit none
  integer :: i

  print *, 'Testing misc stuff...'

  ! This test demonstrates that the mova2i_ C function is not needed
  ! any more. See https://github.com/NOAA-EMC/NCEPLIBS-g2/issues/396.
  do i = 0, 255
     if (i .ne. ichar(char(i))) stop 10
  end do
  
  print *, 'SUCCESS!'
end program test_misc
