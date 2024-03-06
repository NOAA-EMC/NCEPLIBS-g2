! This is a test program for the NCEPLIBS-g2 project.
!
! This program tests the g2_gbytesc.F90 code, even more.
!
! Ed Hartnett, Mar 5, 2024
program test_gbytec2
  implicit none

  character (len = 1) :: c1(1) = 'a'
  character (len = 1) :: c4(4), c4_2(4)
  integer (kind = 4) :: i1
  integer :: i

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
  print *, 'SUCCESS!'

end program test_gbytec2
