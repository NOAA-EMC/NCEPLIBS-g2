!>    @file
!>    @brief Define math functions used by other code.
!>    @author Stephen Gilbert @date 2000-06-21
!>

!>    This module defines integer math functions used by other programs.
!>    It include following functions:
!>    - ilog2 Calculate log(x)/log(2).
!>     - ilog2_8 for 8 bit integer numbers.
!>     - ilog2_4 for 4 bit integer numbers.
!>     - ilog2_2 for 2 bit integer numbers.
!>     - ilog2_1 for 1 bit integer numbers.
!>    - i1log2 Calculate log(x+1)/log(2) unless x=maxint, in which case log(x)/log(2).
!>     - i1log2_8 for 8 bit integer numbers.
!>     - i1log2_4 for 4 bit integer numbers.
!>     - i1log2_2 for 2 bit integer numbers.
!>     - i1log2_1 for 1 bit integer numbers.
!>
!>    @author Stephen Gilbert @date 2000-06-21
module intmath
  implicit none

  interface ilog2
     ! log(x)/log(2)
     module procedure ilog2_8
     module procedure ilog2_4
     module procedure ilog2_2
     module procedure ilog2_1
  end interface ilog2

  interface i1log2
     ! log(x+1)/log(2) unless x=maxint, in which case log(x)/log(2)
     module procedure i1log2_8
     module procedure i1log2_4
     module procedure i1log2_2
     module procedure i1log2_1
  end interface i1log2

contains

  !>    This function returns log(x+1)/log(2) unless x=maxint, in
  !>    which case log(x)/log(2) for 8 bit integer numbers.
  !>    @param[in] ival 8 bit integer numbers.
  !>    @return value for log(x+1)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function i1log2_8(ival)
    implicit none
    integer(kind=8), value :: ival
    integer(kind=8)::i1log2_8
    integer(kind=8), parameter :: one=1
    if(ival+one<ival) then
       i1log2_8=ilog2_8(ival)
    else
       i1log2_8=ilog2_8(ival+one)
    endif
  end function i1log2_8

  !>    This function returns log(x+1)/log(2) unless x=maxint, in
  !>    which case log(x)/log(2) for 4 bit integer numbers.
  !>    @param[in] ival 4 bit integer numbers.
  !>    @return value for log(x+1)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function i1log2_4(ival)
    implicit none
    integer(kind=4), value :: ival
    integer(kind=4)::i1log2_4
    integer(kind=4), parameter :: one=1
    if(ival+one<ival) then
       i1log2_4=ilog2_4(ival)
    else
       i1log2_4=ilog2_4(ival+one)
    endif
  end function i1log2_4

  !>    This function returns log(x+1)/log(2) unless x=maxint, in
  !>    which case log(x)/log(2) for 2 bit integer numbers.
  !>    @param[in] ival 2 bit integer numbers.
  !>    @return value for log(x+1)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function i1log2_2(ival)
    implicit none
    integer(kind=2), value :: ival
    integer(kind=2)::i1log2_2
    integer(kind=2), parameter :: one=1
    if(ival+one<ival) then
       i1log2_2=ilog2_2(ival)
    else
       i1log2_2=ilog2_2(ival+one)
    endif
  end function i1log2_2

  !>    This function returns log(x+1)/log(2) unless x=maxint, in
  !>    which case log(x)/log(2) for 1 bit integer numbers.
  !>    @param[in] ival 1 bit integer numbers.
  !>    @return value for log(x+1)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function i1log2_1(ival)
    implicit none
    integer(kind=1), value :: ival
    integer(kind=1)::i1log2_1
    integer(kind=1), parameter :: one=1
    if(ival+one<ival) then
       i1log2_1=ilog2_1(ival)
    else
       i1log2_1=ilog2_1(ival+one)
    endif
  end function i1log2_1

  !>    This function returns log(x)/log(2) for 8 bit integer numbers.
  !>    @param[in] i_in 8 bit integer numbers.
  !>    @return value for log(x)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function ilog2_8(i_in)
    implicit none
    integer(kind=8), value :: i_in
    integer(kind=8)::ilog2_8,i
    ilog2_8=0
    i=i_in
    if(i<=0) return
    if(iand(i,i-1)/=0) then
       !write(0,*) 'iand i-1'
       ilog2_8=1
    endif
    if(iand(i,Z'FFFFFFFF00000000')/=0) then
       ilog2_8=ilog2_8+32
       i=ishft(i,-32)
       !write(0,*) 'iand ffffffff',i,ilog2_8
    endif
    if(iand(i,Z'00000000FFFF0000')/=0) then
       ilog2_8=ilog2_8+16
       i=ishft(i,-16)
       !write(0,*) 'iand ffff' ,i,ilog2_8
    endif
    if(iand(i,Z'000000000000FF00')/=0) then
       ilog2_8=ilog2_8+8
       i=ishft(i,-8)
       !write(0,*) 'iand ff',i,ilog2_8
    endif
    if(iand(i,Z'00000000000000F0')/=0) then
       ilog2_8=ilog2_8+4
       i=ishft(i,-4)
       !write(0,*) 'iand f',i,ilog2_8
    endif
    if(iand(i,Z'000000000000000C')/=0) then
       ilog2_8=ilog2_8+2
       i=ishft(i,-2)
       !write(0,*) 'iand c',i,ilog2_8
    endif
    if(iand(i,Z'0000000000000002')/=0) then
       ilog2_8=ilog2_8+1
       i=ishft(i,-1)
       !write(0,*) 'iand 2',i,ilog2_8
    endif
  end function ilog2_8

  !>    This function returns log(x)/log(2) for 4 bit integer numbers.
  !>    @param[in] i_in 4 bit integer numbers.
  !>    @return value for log(x)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function ilog2_4(i_in)
    implicit none
    integer(kind=4), value :: i_in
    integer(kind=4)::ilog2_4,i
    ilog2_4=0
    i=i_in
    if(i<=0) return
    if(iand(i,i-1)/=0) then
       !write(0,*) 'iand i-1'
       ilog2_4=1
    endif
    if(iand(i,Z'FFFF0000')/=0) then
       ilog2_4=ilog2_4+16
       i=ishft(i,-16)
       !write(0,*) 'iand ffff' ,i,ilog2_4
    endif
    if(iand(i,Z'0000FF00')/=0) then
       ilog2_4=ilog2_4+8
       i=ishft(i,-8)
       !write(0,*) 'iand ff',i,ilog2_4
    endif
    if(iand(i,Z'000000F0')/=0) then
       ilog2_4=ilog2_4+4
       i=ishft(i,-4)
       !write(0,*) 'iand f',i,ilog2_4
    endif
    if(iand(i,Z'0000000C')/=0) then
       ilog2_4=ilog2_4+2
       i=ishft(i,-2)
       !write(0,*) 'iand c',i,ilog2_4
    endif
    if(iand(i,Z'00000002')/=0) then
       ilog2_4=ilog2_4+1
       i=ishft(i,-1)
       !write(0,*) 'iand 2',i,ilog2_4
    endif
  end function ilog2_4

  !>    This function returns log(x)/log(2) for 2 bit integer numbers.
  !>    @param[in] i_in 2 bit integer numbers.
  !>    @return value for log(x)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function ilog2_2(i_in)
    implicit none
    integer(kind=2), value :: i_in
    integer(kind=2)::ilog2_2,i
    ilog2_2=0
    i=i_in
    if(i<=0) return
    if(iand(i,int(i-1,kind=2))/=0) then
       !write(0,*) 'iand i-1'
       ilog2_2=1
    endif
    if(iand(i,Z'FF00')/=0) then
       ilog2_2=ilog2_2+8
       i=ishft(i,-8)
       !write(0,*) 'iand ff',i,ilog2_2
    endif
    if(iand(i,Z'00F0')/=0) then
       ilog2_2=ilog2_2+4
       i=ishft(i,-4)
       !write(0,*) 'iand f',i,ilog2_2
    endif
    if(iand(i,Z'000C')/=0) then
       ilog2_2=ilog2_2+2
       i=ishft(i,-2)
       !write(0,*) 'iand c',i,ilog2_2
    endif
    if(iand(i,Z'0002')/=0) then
       ilog2_2=ilog2_2+1
       i=ishft(i,-1)
       !write(0,*) 'iand 2',i,ilog2_2
    endif
  end function ilog2_2

  !>    This function returns log(x)/log(2) for 1 bit integer numbers.
  !>    @param[in] i_in 1 bit integer numbers.
  !>    @return value for log(x)/log(2)
  !>    @author Stephen Gilbert @date 2000-06-21
  function ilog2_1(i_in)
    implicit none
    integer(kind=1), value :: i_in
    integer(kind=1)::ilog2_1,i
    ilog2_1=0
    i=i_in
    if(i<=0) return
    if(iand(i,int(i-1,kind=1))/=0) then
       !write(0,*) 'iand i-1'
       ilog2_1=1
    endif
    if(iand(i,Z'F0')/=0) then
       ilog2_1=ilog2_1+4
       i=ishft(i,-4)
       !write(0,*) 'iand f',i,ilog2_1
    endif
    if(iand(i,Z'0C')/=0) then
       ilog2_1=ilog2_1+2
       i=ishft(i,-2)
       !write(0,*) 'iand c',i,ilog2_1
    endif
    if(iand(i,Z'02')/=0) then
       ilog2_1=ilog2_1+1
       i=ishft(i,-1)
       !write(0,*) 'iand 2',i,ilog2_1
    endif
  end function ilog2_1
end module intmath
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$ TEST PROGRAM FOR THIS MODULE
!$$$      program test_intmath
!$$$      use intmath
!$$$      implicit none
!$$$      real(kind=16) :: temp
!$$$      real(kind=16), parameter :: alog2=log(2.0_16)
!$$$      integer(kind=8), parameter  :: &
!$$$     &     one=1,big=Z'7FFFFFFFFFFFFFFF',small=-2000000_8, &
!$$$     &     check=Z'1FFFFFFF'
!$$$      integer(kind=8) :: ival, iret
!$$$      !$OMP PARALLEL DO PRIVATE(ival,temp,iret)
!$$$      do ival=small,big
!$$$ 10      format(Z16,' -- MISMATCH: ',I0,'=>',I0,' (',I0,' = ',F0.10,')')
!$$$ 20      format(Z16,' -- OKAY:     ',I0,'=>',I0,' (',I0,' = ',F0.10,')')
!$$$         if(ival+one<ival) then
!$$$            temp=log(real(max(ival,one),kind=16))/alog2
!$$$         else
!$$$            temp=log(real(max(ival+one,one),kind=16))/alog2
!$$$         endif
!$$$         iret=i1log2(ival)
!$$$         if(iret/=ceiling(temp) .or. ival==0 .or. ival==check) then
!$$$            !$OMP CRITICAL
!$$$            if(iret/=ceiling(temp)) then
!$$$               print 10, ival, ival, iret,ceiling(temp),temp
!$$$            else
!$$$               print 20, ival, ival, iret,ceiling(temp),temp
!$$$            endif
!$$$            !$OMP END CRITICAL
!$$$         endif
!$$$      enddo
!$$$      !$OMP END PARALLEL DO
!$$$      end program test_intmath
