!> @file
!> @brief This subroutine reads 32-bit IEEE floating point values.
!> @author Stephen Gilbert @date 2000-05-09

!> This subroutine reads a list of real values in 32-bit IEEE
!> floating point format.
!>
!> @param[in] rieee Input array of floating point values in 32-bit IEEE format.
!> @param[in] num Number of floating point values to convert.
!> @param[out] a Output array of real values.
!>
!> @author Stephen Gilbert @date 2000-05-09
subroutine rdieee(rieee,a,num)

  real(4),intent(in) :: rieee(num)
  real,intent(out) :: a(num)
  integer,intent(in) :: num

  integer(4) :: ieee

  real,parameter :: two23=scale(1.0,-23)
  real,parameter :: two126=scale(1.0,-126)

  do j=1,num
     !
     !  Transfer IEEE bit string to integer variable
     !
     ieee=transfer(rieee(j),ieee)
     !
     !  Extract sign bit, exponent, and mantissa
     !
     isign=ibits(ieee,31,1)
     iexp=ibits(ieee,23,8)
     imant=ibits(ieee,0,23)
     sign=1.0
     if (isign.eq.1) sign=-1.0

     if ( (iexp.gt.0).and.(iexp.lt.255) ) then
        temp=2.0**(iexp-127)
        a(j)=sign*temp*(1.0+(two23*real(imant)))

     elseif ( iexp.eq.0 ) then
        if ( imant.ne.0 ) then
           a(j)=sign*two126*two23*real(imant)
        else
           a(j)=sign*0.0
        endif

     elseif ( iexp.eq.255 ) then
        a(j)=sign*huge(a(j))

     endif

  enddo

  return
end subroutine rdieee
