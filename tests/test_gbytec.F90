! This is a test program for the NCEPLIBS-g2 project.
!
! This program tests the g2_gbytesc.F90 code.
!
! Ed Hartnett, Nov. 12, 2021
program test_gbytec
  implicit none

  character*1 :: out(1)
  character*1 :: out5(5)
  character*1 :: out2(8)
  character*1 :: out10(10)
  integer, parameter :: n = 1
  integer :: in(n)
  integer, parameter :: n2 = 2
  integer :: in2(n2)
  integer, parameter :: n5 = 5
  integer :: in5(n5)
  integer :: iskip = 0
  integer :: nbits = 8
  integer :: nskip = 0
  integer :: i

  print *, 'Testing g2_gbytesc.f subroutines.'

  print *, 'Testing g2_sbytec()...'
  in(1) = 3
  out(1) = char(0)
  call g2_sbytec(out, in, iskip, nbits)
  if (ichar(out(1)) .ne. in(1)) stop 10

  print *, 'Testing g2_sbytesc()...'
  in(1) = 3
  out(1) = char(0)
  call g2_sbytesc(out, in, iskip, nbits, nskip, n)
  if (ichar(out(1)) .ne. in(1)) stop 20

  ! This will pack the numbers 1 and 2 into the first two chars of the
  ! buffer. The rest of the output buffer will remain zeros.
  print *, 'Testing g2_sbytesc() packing 2 values...'
  in2(1) = 1
  in2(2) = 2
  do i = 1, 8
     out2(i) = char(0)
  end do
  nbits = 8
  call g2_sbytesc(out2, in2, iskip, nbits, nskip, n2)
  do i = 1, 8
     if (i .le. 2) then
        if (ichar(out2(i)) .ne. in2(i)) stop 30;
     else
        if (ichar(out2(i)) .ne. 0) stop 31;
     endif
  end do

  ! Now pack 5 values into the 5 character array out5.
  print *, 'Testing g2_sbytesc() packing 5 values...'
  in5(1) = 1
  in5(2) = 2
  in5(3) = 3
  in5(4) = 4
  in5(5) = 5
  nbits = 8
  nskip = 0
  do i = 1, 5
     out5(i) = char(0)
  end do
  call g2_sbytesc(out5, in5, iskip, nbits, nskip, n5)
  do i = 1, 5
     if (ichar(out5(i)) .ne. in5(i)) stop 40;     
  end do

  ! Now pack 5 values into the 10 character array out10. Skip every
  ! other byte in the output.
  print *, 'Testing g2_sbytesc() packing 5 values, skipping every other byte...'
  nbits = 8
  nskip = 0
  do i = 1, 10
     out10(i) = char(0)
  end do
  call g2_sbytesc(out10, in5, iskip, nbits, 8, 5)
  do i = 1, 10
     ! print '(z2.2)', out10(i)
     if (mod(i, 2) .gt. 0) then
        if (ichar(out10(i)) .ne. in5(int(i/2) + 1)) stop 51;
     else
        if (ichar(out10(i)) .ne. 0) stop 50;
     endif
  end do

  print *, 'Testing g2_sbytec() with iskip of 1...'
  in(1) = 1
  out(1) = char(0)
  call g2_sbytec(out, in, 1, 6)
  print '(z2.2)', out(1)  
  if (ichar(out(1)) .ne. 2) stop 20

  print *, 'SUCCESS!'

end program test_gbytec
