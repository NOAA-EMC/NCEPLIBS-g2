! Test the mkieee and rdieee functions in NCEPLIBS-g2

! Brian Curtis 2022/02/01

program test_mkieee
    implicit none
    
    integer, parameter :: num = 4
    real(4) :: rieee(num)
    real(4) :: a(num) = (/ 4.3000000, 5.6000000, 1.6700000, 2.3300000 /)
    real(8) :: b(num)
    integer :: i

    print *, 'Testing mkieee and rdieee ...'

    call mkieee(a, rieee, num)

    do i = 1, num
        if (abs(a(i) - rieee(i)) .gt. .01) stop 10
    end do

    call rdieee(rieee, b, num)

    do i = 1, num
        if (abs(a(i) - b(i)) .gt. .01) stop 20
    end do

    print *, '... Success!'

end program test_mkieee