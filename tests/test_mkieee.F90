! Test the mkieee and rdieee functions in NCEPLIBS-g2
!
! Brian Curtis 2022/02/01
! Ed Hartnett

program test_mkieee
  implicit none

#ifdef KIND_4
  integer, parameter :: num = 9
  real :: rieee(num)
  real :: a(num) = (/ real :: 4.3, 5.6, 1.67, 2.33, -4.3, 0.33, 0.0, -400.0, 101029284739847594.0/)
  real :: b(num)
  integer :: i

  print *, 'Testing mkieee and rdieee ...'

  call mkieee(a, rieee, num)

  do i = 1, num
     if (abs(a(i) - rieee(i)) .gt. .00001) stop 10
  end do

  call rdieee(rieee, b, num)

  do i = 1, num
     print *, i, a(i), b(i)
     if (abs(a(i) - b(i)) .gt. .00001) stop 20
  end do

#endif
  
  print *, '... Success!'

end program test_mkieee
