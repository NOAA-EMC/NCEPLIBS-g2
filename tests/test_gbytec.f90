program test_gbytec
  implicit none

  character*1 :: out(1)
  character*1 :: out5(20)
  character*1 :: out2(8)
  integer, parameter :: n = 1
  integer :: in(n)
  integer, parameter :: n2 = 2
  integer :: in2(n2)
  integer, parameter :: n5 = 5
  integer :: in5(n5)
  integer :: iskip = 0
  integer :: nbyte = 8
  integer :: nskip = 0
  integer :: i

  print *, 'Testing gbytec.'

  print *, 'Testing sbytec()...'
  in(1) = 3
  call g2_sbytec(out, in, iskip, nbyte)
  if (ichar(out(1)) .ne. in(1)) stop 10

  print *, 'Testing sbytesc()...'
  in(1) = 3
  call g2_sbytesc(out, in, iskip, nbyte, nskip, n)
  if (ichar(out(1)) .ne. in(1)) stop 10

  print *, 'Testing sbytec() with more bytes...'
  
  in2(1) = 1
  in2(2) = 2
  nbyte = 16
  nskip = 0
  call g2_sbytec(out2, in2, iskip, nbyte)
  do i = 1, 8
     print *, ichar(out2(i))
  end do

  ! in5(1) = 1
  ! in5(2) = 2
  ! in5(3) = 3
  ! in5(4) = 4
  ! in5(5) = 5
  ! nbyte = 40
  ! nskip = 0
  ! call g2_sbytec(out5, in5, iskip, nbyte)
  ! do i = 1, 20
  !    print *, ichar(out5(i))
  ! end do
  !if (ichar(out(1)) .ne. in(1)) stop 10

  

  print *, 'SUCCESS!'
end program test_gbytec
