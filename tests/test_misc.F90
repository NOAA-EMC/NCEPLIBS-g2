! This is a test program for NCEPLIBS-g2.
!
! This program tests misc stuff.
!
! Ed Hartnett 5/16/23
program test_misc
  implicit none
  
  integer :: i

  print *, 'Testing misc stuff...'

  do i = 0, 255
     if (i .ne. ichar(char(i))) stop 10
  end do
  
  print *, 'SUCCESS!'
end program test_misc
