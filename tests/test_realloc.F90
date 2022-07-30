! This is a test program for NCEPLIBS-g2.
!
! This program tests the code in realloc.f90.
!
! Ed Hartnett 7/21/22
program test_realloc
  use re_alloc
  implicit none

  character(len=1), pointer, dimension(:) :: c
  integer, pointer, dimension(:) :: i
  integer n, m, istat

  print *, 'Testing realloc...'

  ! Initialize parameters.
  n = 1
  m = 2
  nullify(c)
  nullify(i)

  ! These will fail for character data.
  call realloc(c, -1, m, istat)
  if (istat .ne. 10) stop 3
  call realloc(c, n, 0, istat)
  if (istat .ne. 10) stop 3

  ! This will succeed in allocating new memory for character data.
  call realloc(c, n, m, istat)
  if (istat .ne. 0) stop 4
  c(1) = 'a'
  c(2) = 'b'

  ! This will succeed in reallocating memory for character data.
  call realloc(c, 2, 4, istat)
  if (istat .ne. 0) stop 4
  if (c(1) .ne. 'a' .or. c(2) .ne. 'b') stop 5
  c(3) = 'c'
  c(4) = 'd'
  deallocate(c)

  ! These will fail for integer data.
  call realloc(i, -1, m, istat)
  if (istat .ne. 10) stop 3
  call realloc(i, n, 0, istat)
  if (istat .ne. 10) stop 3

  ! This will succeed in allocating new memory for integer data.
  call realloc(i, n, m, istat)
  if (istat .ne. 0) stop 40
  i(1) = 42
  i(2) = 43

  ! This will succeed in re-allocating memory for integer data.
  call realloc(i, 2, 4, istat)
  if (istat .ne. 0) stop 40
  if (i(1) .ne. 42 .or. i(2) .ne. 43) stop 41
  i(3) = 44
  i(4) = 45
  deallocate(i)

  print *, 'SUCCESS!'
end program test_realloc
