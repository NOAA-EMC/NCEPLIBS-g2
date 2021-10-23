program test_gbytec
  implicit none

  character*1 :: out(1)
  integer, parameter :: n = 1
  integer :: in(n)
  integer :: iskip = 0
  integer :: nbyte = 8
  integer :: nskip = 0

  print *, 'Testing gbytec.'

  print *, 'Testing sbytec()...'
  in(1) = 3
  call g2_sbytec(out, in, iskip, nbyte)
  print *, ichar(out(1))
  if (ichar(out(1)) .ne. in(1)) stop 10

  print *, 'Testing sbytesc()...'
  in(1) = 3
  call g2_sbytesc(out, in, iskip, nbyte, nskip, n)
  print *, ichar(out(1))
  if (ichar(out(1)) .ne. in(1)) stop 10

  print *, 'SUCCESS!'
end program test_gbytec
