! Test the mkieee and rdieee functions in NCEPLIBS-g2
!
! Brian Curtis 2022/02/01
! Ed Hartnett

program test_mkieee
  implicit none

#ifdef KIND_4
  integer, parameter :: num = 9
  real :: rieee(num)
  real :: a(num) = (/ 4.3000000, 5.6000000, 1.6700000, 2.3300000, -4.3000000, 0.3300000, 0.0000000, -400.000000, 10102928473984759485903839387494930/)
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
