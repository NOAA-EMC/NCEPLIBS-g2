! This is a test program for the NCEPLIBS-g2 project.
!
! This program tests the g2_gbytesc.F90 code.
!
! Ed Hartnett, Nov. 12, 2021
program test_gbytec
  implicit none

  character*1 :: out(1)
  character*1 :: out4(4)
  character*1 :: out5(5)
  character*1 :: out8(8)
  character*1 :: out10(10)
  character*1 :: out16(16)
  character*1 :: out32(32)
  integer :: in4(4), in1(1)
  integer, parameter :: n = 1
  integer :: in(n)
  real :: r_in(n)
  integer, parameter :: n2 = 2
  integer :: in2(n2)
  real :: r_in2(n2)
  integer, parameter :: n5 = 5
  integer :: in5(n5)
  integer :: iskip = 0
  integer :: nbits = 8
  integer :: nskip = 0
  integer :: i
  integer :: num
  integer(kind = 4) :: in44(4), in44_1(4)
  integer(kind = 8) :: in8(1), in8_1(1), in84(4), in84_1(4), in8_scalar
  integer, external :: g2_mova2i
  
  print *, 'Testing g2_gbytesc.F90 subroutines.'

  print *, '   testing g2_sbytec(), packing one 32-bit int into 8 bits of a char array in big-endian...'
  in(1) = 3
  out(1) = char(0)
  call g2_sbytec(out, in, iskip, nbits)
  if (g2_mova2i(out(1)) .ne. in(1)) stop 10
  !print '(z2.2)', out(1)
  
  print *, '       packing one 32-bit int into 32 bits of a char array in big-endian...'
  in(1) = 3
  do i = 1, 4
     out4(i) = char(0)
  end do
  call g2_sbytec(out4, in, 0, 32)
  ! do i = 1, 4
  !    print '(z2.2)', out4(i)
  ! end do
  
  print *, '   testing g2_gbytec()...'
  call g2_gbytec(out, in, iskip, nbits)
  if (g2_mova2i(out(1)) .ne. in(1)) stop 11

  print *, '   testing g2_sbytesc()...'
  in(1) = 3
  out(1) = char(0)
  call g2_sbytesc(out, in, iskip, nbits, nskip, n)
  if (g2_mova2i(out(1)) .ne. in(1)) stop 20

  ! This will pack the numbers 1 and 2 into the first two chars of the
  ! buffer. The rest of the output buffer will remain zeros.
  print *, '   testing g2_sbytesc() packing 2 values...'
  in2(1) = 1
  in2(2) = 2
  do i = 1, 8
     out8(i) = char(0)
  end do
  nbits = 8
  call g2_sbytesc(out8, in2, iskip, nbits, nskip, n2)
  do i = 1, 8
     if (i .le. 2) then
        if (g2_mova2i(out8(i)) .ne. in2(i)) stop 30;
     else
        if (g2_mova2i(out8(i)) .ne. 0) stop 31;
     endif
  end do

  ! Now pack 5 values into the 5 character array out5.
  print *, '   testing g2_sbytesc() packing 5 values...'
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
     if (g2_mova2i(out5(i)) .ne. in5(i)) stop 40;     
  end do

  ! Now pack 5 values into the 10 character array out10. Skip every
  ! other byte in the output.
  print *, '   testing g2_sbytesc() packing 5 values, skipping every other byte...'
  nbits = 8
  nskip = 0
  do i = 1, 10
     out10(i) = char(0)
  end do
  call g2_sbytesc(out10, in5, iskip, nbits, 8, 5)
  do i = 1, 10
     ! print '(z2.2)', out10(i)
     if (mod(i, 2) .gt. 0) then
        if (g2_mova2i(out10(i)) .ne. in5(int(i/2) + 1)) stop 51;
     else
        if (g2_mova2i(out10(i)) .ne. 0) stop 52;
     endif
  end do

  print *, '   testing g2_sbytec() with iskip of 1...'
  in(1) = 1
  out(1) = char(0)
  call g2_sbytec(out, in, 1, 6)
  if (g2_mova2i(out(1)) .ne. 2) stop 53

  print *, '   testing g2_sbytesc() with a size 4 output array...'
  iskip = 0
  nbits = 32
  nskip = 0
  num = 1
  in(1) = 1
  call g2_sbytesc(out4, in, iskip, nbits, nskip, num)
  if (g2_mova2i(out4(1)) .ne. 0 .and. g2_mova2i(out4(2)) .ne. 0 .and. &
       g2_mova2i(out4(3)) .ne. 0 .and. g2_mova2i(out4(4)) .ne. 1) stop 60

  print *, '   now unpack into 4 ints with g2_gbytesc()...'
  call g2_gbytesc(out4, in4, iskip, 8, 0, 4)
  do i = 1, 4
     if (i < 4) then
        if (in4(i) .ne. 0) stop 61
     else
        if (in4(i) .ne. 1) stop 62
     endif
  end do

  print *, '   now unpack into 1 int with g2_gbytesc()...'
  call g2_gbytesc(out4, in1, iskip, 32, 0, 1)
  if (in1(1) .ne. 1) stop 70
  
  ! For this test to pass the -fallow-argument-mismatch flag must be
  ! used, because I am passing in a real array instead of an int array
  ! for the in parameter. This is how g2_sbytesc() is called in
  ! addfield.F90.
  ! print *, '   testing g2_sbytesc() with a real array (size 1) instead of an int array...'
  ! iskip = 0
  ! nbits = 32
  ! nskip = 0
  ! num = 1
  ! r_in(1) = 1 
  ! call g2_sbytesc(out4, r_in, iskip, nbits, nskip, num)
  ! ! Note that the 32-bit IEEE representation of 1.0 is 3f800000. The
  ! ! decimal for 3f is 63, the decimal for 80 is 128.
  ! if (g2_mova2i(out4(1)) .ne. 63 .and. g2_mova2i(out4(2)) .ne. 128 .and. &
  !      g2_mova2i(out4(3)) .ne. 0 .and. g2_mova2i(out4(4)) .ne. 0) stop 80
  ! print '(z2.2)', out4(1)  

  ! This test is the same as above, but does not require the
  ! -fallow-argument-mismatch flag.
  print *, '   testing g2_sbytesc() with a size 1 real array instead of an int array, using transfer()...'
  iskip = 0
  nbits = 32
  nskip = 0
  num = 1
  r_in(1) = 1
  in = transfer(r_in, in)
  call g2_sbytesc(out4, in, iskip, nbits, nskip, num)
  ! Note that the 32-bit IEEE representation of 1.0 is 3f800000. The
  ! decimal for 3f is 63, the decimal for 80 is 128.
  if (g2_mova2i(out4(1)) .ne. 63 .and. g2_mova2i(out4(2)) .ne. 128 .and. &
       g2_mova2i(out4(3)) .ne. 0 .and. g2_mova2i(out4(4)) .ne. 0) stop 90

  ! For this test to pass the -fallow-argument-mismatch flag must be
  ! used, because I am passing in a real array instead of an int array
  ! for the in parameter. This is how g2_sbytesc() is called in
  ! addfield.F90.
  ! print *, '   testing g2_sbytesc() with a real array instead of an int array...'
  ! iskip = 0
  ! nbits = 32
  ! nskip = 0
  ! num = 2
  ! r_in2(1) = 1 
  ! r_in2(2) = 1 
  ! call g2_sbytesc(out8, r_in2, iskip, nbits, nskip, num)
  ! ! Note that the 32-bit IEEE representation of 1.0 is 3f800000. The
  ! ! decimal for 3f is 63, the decimal for 80 is 128.
  ! if (g2_mova2i(out8(1)) .ne. 63 .and. g2_mova2i(out8(2)) .ne. 128 .and. &
  !      g2_mova2i(out8(3)) .ne. 0 .and. g2_mova2i(out8(4)) .ne. 0) stop 100
  ! if (g2_mova2i(out8(5)) .ne. 63 .and. g2_mova2i(out8(6)) .ne. 128 .and. &
  !      g2_mova2i(out8(7)) .ne. 0 .and. g2_mova2i(out8(8)) .ne. 0) stop 110
  ! print '(z2.2)', out8(1)  

  ! This test is the same as above, but does not require the -fallow-argument-mismatch flag.
  print *, '   testing g2_sbytesc() with a real array instead of an int array, using transfer() intrinsic...'
  iskip = 0
  nbits = 32
  nskip = 0
  num = 2
  r_in2(1) = 1 
  r_in2(2) = 1 
  in = transfer(r_in2, in2)
  call g2_sbytesc(out8, in2, iskip, nbits, nskip, num)
  ! Note that the 32-bit IEEE representation of 1.0 is 3f800000. The
  ! decimal for 3f is 63, the decimal for 80 is 128.
  if (g2_mova2i(out4(1)) .ne. 63 .and. g2_mova2i(out4(2)) .ne. 128 .and. &
       g2_mova2i(out4(3)) .ne. 0 .and. g2_mova2i(out4(4)) .ne. 0) stop 120
  if (g2_mova2i(out8(5)) .ne. 63 .and. g2_mova2i(out8(6)) .ne. 128 .and. &
       g2_mova2i(out8(7)) .ne. 0 .and. g2_mova2i(out8(8)) .ne. 0) stop 130
  !print '(z2.2)', out4(1)  

  print *, '   testing g2_sbytec8() with 64-bit int...'
  in8(1) = 1
  do i = 1, 8
     out8(i) = char(0)
  end do
  call g2_sbytec8(out8, in8, iskip, 64)
  do i = 1, 8
     !print '(z2.2)', out8(i)
     if (i .lt. 8) then
        if (g2_mova2i(out8(i)) .ne. 0) stop 140
     else
        if (g2_mova2i(out8(i)) .ne. 1) stop 140
     endif
  end do

  print *, '   now unpack into 1 64-bit int with g2_gbytesc8()...'
  in8_1(1) = 0
  call g2_gbytesc8(out8, in8_1, iskip, 64, 0, 1)
  if (in8_1(1) .ne. in8(1)) stop 150

  print *, '   testing g2_sbytec() with 32-bit int array of size 4...'
  do i = 1, 4
     in44(i) = 1
  end do
  do i = 1, 16
     out16(i) = char(0)
  end do
  call g2_sbytesc(out16, in44, iskip, 32, 0, 4)
  ! do i = 1, 16
  !    print '(i3, x, z2.2)', i, out16(i)
  ! end do
  
  print *, '   now unpack into 4 32-bit ints with g2_gbytesc()...'
  do i = 1, 4
     in44_1(i) = 0
  end do
  call g2_gbytesc(out16, in44_1, iskip, 32, 0, 4)
  do i = 1, 4
     if (in44_1(i) .ne. in44(i)) stop 160
  end do

  print *, '   testing g2_sbytec8() with 64-bit int array of size 4...'
  do i = 1, 4
     in84(i) = 1
  end do
  do i = 1, 32
     out32(i) = char(0)
  end do
  !print *, in84
  call g2_sbytesc8(out32, in84, iskip, 64, 0, 4)
  ! do i = 1, 32
  !    print '(i3, x, z2.2)', i, out32(i)
  ! end do

  print *, '   now unpack into 4 64-bit ints with g2_gbytesc8()...'
  do i = 1, 4
     in84_1(i) = 0
  end do
  call g2_gbytesc8(out32, in84_1, iskip, 64, 0, 4)
  !print *, in84_1
  do i = 1, 4
     if (in84_1(i) .ne. in84(i)) stop 200
  end do

  print *, '   testing g2_gbytec81() reading first value of 64-bit int array of size 4...'  
  call g2_gbytec81(out32, in8_scalar, iskip, 64)
  if (in8_scalar .ne. 1) stop 201

  print *, '   testing g2_sbytec8() with very large 64-bit int...'
  ! largest 4 byte signed int, plus 1.
  in8(1) = 2147483647_8 + 1_8
  print '(z16.16)', in8(1)
  do i = 1, 8
     out8(i) = char(0)
  end do
  call g2_sbytec8(out8, in8, iskip, 64)
  do i = 1, 8
     print '(z2.2)', out8(i)
     if (i .eq. 5) then
        if (g2_mova2i(out8(i)) .ne. 128) stop 210
     else
        if (g2_mova2i(out8(i)) .ne. 0) stop 210
     endif
  end do

  print *, '   now unpack into 1 64-bit int with g2_gbytesc8()...'
  in8_1(1) = 0
  call g2_gbytesc8(out8, in8_1, iskip, 64, 0, 1)
  !print *, in8_1
  if (in8_1(1) .ne. in8(1)) stop 150

  print *, 'SUCCESS!'

end program test_gbytec
