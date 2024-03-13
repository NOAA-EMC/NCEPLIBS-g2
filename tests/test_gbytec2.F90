! This is a test program for the NCEPLIBS-g2 project.
!
! This program tests the g2_gbytesc.F90 code, even more.
!
! Ed Hartnett, Mar 5, 2024
program test_gbytec2
  implicit none

  character (len = 1) :: c1(1) = 'a'
  character (len = 1) :: c4(4), c4_2(4)
  character (len = 1) :: c8(8)
  integer (kind = 4) :: i1
  integer :: i
  real (kind = 4) :: r1(1), r2(2)

  ! Initialize some test data.
  do i = 1, 4
     c4(i) = 'a'
  end do
  
  print *, 'Testing g2_gbytesc.F90 subroutines some more.'

  print *, 'testing g2_gbytec1() with a single 8-bit value...'
  call g2_gbytec1(c1, i1, 0, 8)
  ! a in ascii is 97 in decimal, 61 in hex.
  if (i1 .ne. 97) stop 10
  print *, 'OK!'

  print *, 'testing g2_gbytec1() with two 16-bit values...'
  do i = 1, 2
     call g2_gbytec1(c4, i1, (i - 1) * 16, 16)
     if (i1 .ne. 24929) stop 10
  end do
  print *, 'OK!'

  print *, 'testing g2_gbytec1() with four 8-bit values...'
  do i = 0, 3
     call g2_gbytec1(c4, i1, i * 8, 8)
     if (i1 .ne. 97) stop 10
  end do
  print *, 'OK!'

  print *, 'testing g2_gbytec1() with a single 32-bit value...'
  call g2_gbytec1(c4, i1, 0, 32)
  ! This is the decimal for 0x61616161.
  if (i1 .ne. 1633771873) stop 10
  print *, 'OK!'
  
  print *, 'testing g2_sbytec1() with a single 8-bit value...'
  call g2_sbytec1(c4_2, i1, 0, 8)
  ! print '(z2.2)', c4_2(i)
  ! print '(z8.8)', i1
  ! print *, c4_2
  ! a in ascii is 97 in decimal, 61 in hex.
  if (c4_2(1) .ne. 'a') stop 10
  print *, 'OK!'

  ! Reset array.
  do i = 1, 4
     c4_2(i) = '.'
  end do
  
  print *, 'testing g2_sbytec1() with two 16-bit values...'
  do i = 1, 2
     call g2_sbytec1(c4_2, i1, (i - 1) * 16, 16)
  end do
  !print *, c4_2
  if (any(c4 .ne. c4_2)) stop 20
  print *, 'OK!'

  ! Reset array.
  do i = 1, 4
     c4_2(i) = '.'
  end do
  
  print *, 'testing g2_sbytec1() with a single 32-bit value...'
  call g2_sbytec1(c4_2, i1, 0, 32)
  if (any(c4 .ne. c4_2)) stop 25

  print *, 'OK!'
  
  print *, 'testing g2_gbytescr() with a single float...'
  ! Reset array to IEEE float value 1.0.
  c4(1) = char(63)
  c4(2) = char(128)
  c4(3) = char(0)
  c4(4) = char(0)
  call g2_gbytescr(c4, r1, 0, 32, 0, 1)
  if (r1(1) .ne. 1.0) stop 100
  print *, 'OK!'
  
  print *, 'testing g2_gbytescr() with a two floats...'
  ! Reset array to IEEE float value 1.0, twice.
  do i = 0, 1
     c8(1 + i * 4) = char(63)
     c8(2 + i * 4) = char(128)
     c8(3 + i * 4) = char(0)
     c8(4 + i * 4) = char(0)
  end do
  call g2_gbytescr(c8, r2, 0, 32, 0, 2)
  if (r2(1) .ne. 1.0 .or. r2(2) .ne. 1.0) stop 110
  print *, 'OK!'
  
  print *, 'testing g2_sbytescr() with a single float...'
  ! Reset array.
  do i = 1, 4
     c4(i) = '.'
  end do
  r1(1) = 1.0
  call g2_sbytescr(c4, r1, 0, 32, 0, 1)
  if (ichar(c4(1)) .ne. 63 .or. ichar(c4(2)) .ne. 128 .or. ichar(c4(3)) .ne. 0 .or. &
       ichar(c4(4)) .ne. 0) stop 120
  print *, 'OK!'

  print *, 'testing g2_sbytescr() with a two floats...'
  ! Reset array.
  do i = 1, 8
     c8(i) = '.'
  end do
  r2(1) = 1.0
  r2(2) = 1.0
  call g2_sbytescr(c8, r2, 0, 32, 0, 2)
  ! do i = 1, 8
  !    print '(z2.2)', c8(i)
  ! end do
  do i = 0, 1
     if (ichar(c8(1 + i * 4)) .ne. 63 .or. ichar(c8(2 + i * 4)) .ne. 128 .or. &
          ichar(c8(3 + i * 4)) .ne. 0 .or. ichar(c8(4 + i * 4)) .ne. 0) stop 130
  end do
  print *, 'OK!'

  print *, 'testing rdieeec() with a float...'
  ! Reset array to IEEE float value 1.0.
  c4(4) = char(63)
  c4(3) = char(128)
  c4(2) = char(0)
  c4(1) = char(0)
  r1(1) = 0
  call rdieee(c4, r1, 1)
  if (r1(1) .ne. 1.0) stop 300
  print *, 'OK!'

  print *, 'testing rdieeec() with a two floats...'
  ! Reset array to IEEE float value 1.0, twice.
  do i = 0, 1
     c8(4 + i * 4) = char(63)
     c8(3 + i * 4) = char(128)
     c8(2 + i * 4) = char(0)
     c8(1 + i * 4) = char(0)
  end do
  r2(1) = 0
  r2(2) = 0
  call rdieeec(c8, r2, 2)
  if (r2(1) .ne. 1.0 .or. r2(2) .ne. 1.0) stop 310
  print *, 'OK!'

  print *, 'SUCCESS!'

end program test_gbytec2
