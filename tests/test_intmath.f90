! This is a test program for NCEPLIBS-g2. It tests the intmath module.
!
! Ed Hartnett, 12/22/21
program test_intmath
  use intmath
  implicit none
  real(kind = 16), parameter :: alog2 = log(2.0_16)
  integer(kind = 8) :: ival8 = 10, iret8
  integer(kind = 4) :: ival4 = 10, iret4
  integer(kind = 2) :: ival2 = 10, iret2
  integer(kind = 1) :: ival1 = 10, iret1

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

  print *, 'SUCCESS!'
end program test_intmath

