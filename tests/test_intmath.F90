! This is a test program for NCEPLIBS-g2. It tests the intmath module.
!
! Ed Hartnett, 12/22/21
program test_intmath
  use intmath
  implicit none
  real(kind = 16), parameter :: alog2 = log(2.0_16)
  integer(kind = 8) :: ival8 = 10, iret8
  integer(kind = 4) :: ival4 = 10, iret4
  integer(kind = 2) :: ival2 = 10_2, iret2
  integer(kind = 1) :: ival1 = 10_1, iret1
  real(kind=16) :: temp
  integer(kind=8) :: big, check
  integer(kind=8), parameter  :: one = 1
  data big /Z'7FFFFFFFFFFFFFFF'/
  data check /Z'1FFFFFFF'/
  integer(kind=8), parameter  :: small = -2000000_8
  integer(kind=8) :: ival, iret

  print *, 'Testing intmath...'
  
  print *, 'Testing i1log2()...'
  iret8 = i1log2(ival8)
  if (iret8 .ne. 4) stop 2
  iret4 = i1log2(ival4)
  if (iret4 .ne. 4) stop 2
  iret2 = i1log2(ival2)
  if (iret2 .ne. 4) stop 2
  iret1 = i1log2(ival1)
  if (iret1 .ne. 4) stop 2
  print *, 'ok'

  print *, 'Testing ilog2()...'
  iret8 = ilog2(ival8)
  if (iret8 .ne. 4) stop 2
  iret4 = ilog2(ival4)
  if (iret4 .ne. 4) stop 2
  iret2 = ilog2(ival2)
  if (iret2 .ne. 4) stop 2
  iret1 = ilog2(ival1)
  if (iret1 .ne. 4) stop 2
  print *, 'ok'

  ! Here is some legacy test code that could be recovered...
  print *, 'running slow legacy tests...'
  do ival = big - 50000, big - 1
10   format(Z16,' -- MISMATCH: ',I0,'=>',I0,' (',I0,' = ',F0.10,')')
20   format(Z16,' -- OKAY:     ',I0,'=>',I0,' (',I0,' = ',F0.10,')')
     if (ival + one < ival) then
        temp = log(real(max(ival, one), kind=16)) / alog2
     else
        temp = log(real(max(ival + one, one), kind = 16)) / alog2
     endif
     iret = i1log2(ival)
     if (iret /= ceiling(temp) .or. ival == 0 .or. ival == check) then
        if (iret /= ceiling(temp)) then
           print 10, ival, ival, iret, ceiling(temp),temp
           stop 10
        else
           print 20, ival, ival, iret, ceiling(temp),temp
        endif
     endif
  enddo
  print *, 'ok'
  
  print *, 'SUCCESS!'

end program test_intmath

