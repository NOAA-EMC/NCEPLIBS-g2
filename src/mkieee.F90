!> @file
!> @brief Copy array of real to array of IEEE 32-bit floating point.
!> @author Stephen Gilbert @date 2000-05-09

!> This subroutine copies an array of real to an array of 32-bit IEEE
!> floating points.
!>
!> @param[in] a Input array of floating point values.
!> @param[out] rieee Output array of floating point values in 32-bit
!> IEEE format.
!> @param[in] num Number of floating point values to convert.
!>
!> @author Stephen Gilbert @date 2000-05-09
subroutine mkieee(a, rieee, num)
  implicit none

  real(4), intent(in) :: a(num)
  real(4), intent(out) :: rieee(num)
  integer, intent(in) :: num
  real :: alog2, atemp
  integer :: iexp, imant, j, n
  integer(4) :: ieee
  real, parameter :: two23 = scale(1.0,23)
  real, parameter :: two126 = scale(1.0,126)

  alog2 = alog(2.0)

  do j = 1, num
     ieee = 0

     if (a(j) .eq. 0.) then
        ieee = 0
        rieee(j) = transfer(ieee, rieee(j))
        !       write(6,fmt = '(f20.10,5x,b32)') a,a
        !       write(6,fmt = '(f20.10,5x,b32)') rieee,rieee
        cycle
     endif

     !  Set Sign bit (bit 31 - leftmost bit)
     if (a(j) .lt. 0.0) then
        ieee = ibset(ieee, 31)
        atemp = abs(a(j))
     else
        ieee = ibclr(ieee, 31)
        atemp = a(j)
     endif

     !  Determine exponent n with base 2
     if (atemp .ge. 1.0) then
        n = 0
        do while (2.0**(n + 1) .le. atemp)
           n = n + 1
        enddo
     else
        n = -1
        do while (2.0**n .gt. atemp)
           n = n - 1
        enddo
     endif
     !        n=floor(alog(atemp)/alog2)
     !write(6,*) ' logstuff ',alog(atemp)/alog2
     !write(6,*) ' logstuffn ',n
     iexp = n+127
     if (n .gt. 127) iexp = 255     ! overflow
     if (n .lt. -127) iexp = 0
     !      set exponent bits (bits 30-23)
     call mvbits(iexp, 0, 8, ieee, 23)

     !  Determine Mantissa
     if (iexp .ne. 255) then
        if (iexp .ne. 0) then
           atemp = (atemp / (2.0**n)) - 1.0
        else
           atemp = atemp * two126
        endif
        imant = nint(atemp * two23)
     else
        imant = 0
     endif
     !      set mantissa bits (bits 22-0 )
     call mvbits(imant, 0, 23, ieee, 0)

     !  Transfer IEEE bit string to real variable
     rieee(j) = transfer(ieee, rieee(j))
     !       write(6,fmt = '(f20.10,5x,b32)') a,a
     !       write(6,fmt = '(f20.10,5x,b32)') rieee,rieee

  enddo
end subroutine mkieee
