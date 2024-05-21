! This program tests the gb_info() subroutine of the NCEPLIBS-g2
! project. 
!
! Ed Hartnett 7/17/23
program test_gb_info
  use g2logging
  implicit none

  ! Length of our message.
  integer, parameter :: lcgrib = 191
  integer, parameter :: lcgrib2 = 195
  integer :: numfields, maxlocal, numlocal, maxvals(7)
  
  ! Section 0 and 1.
  integer, parameter :: sec1_len = 13
  integer :: listsec0(3), listsec1(sec1_len)
  integer :: expected_listsec1(sec1_len) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0/)

  ! This is a GRIB2 message.
  character :: cgrib(lcgrib2) = (/  char( 71), char( 82),&
       & char( 73), char( 66), char(  0), char(  0), char(  0),&
       & char(  2), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(191), char(  0),&
       & char(  0), char(  0), char( 21), char(  1), char(  0),&
       & char(  7), char(  0), char(  4), char(  2), char( 24),&
       & char(  0), char(  7), char(229), char( 11), char( 13),&
       & char( 15), char( 59), char( 59), char(  1), char(  0),&
       & char(  0), char(  0), char(  0), char(  8), char(  2),&
       & char(  1), char(  2), char(  3), char(  0), char(  0),&
       & char(  0), char( 72), char(  3), char(  0), char(  0),&
       & char(  0), char(  0), char(  4), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  0), char(  0),&
       & char(  0), char(  2), char(  0), char(  0), char(  0),&
       & char(  2), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char( 45), char(  0), char(  0),&
       & char(  0), char( 91), char(  0), char(  0), char(  0),&
       & char(  0), char( 55), char(  0), char(  0), char(  0),&
       & char(101), char(  0), char(  0), char(  0), char(  5),&
       & char(  0), char(  0), char(  0), char(  5), char(  0),&
       & char(  0), char(  0), char(  0), char( 34), char(  4),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  0), char(  0), char(  0),&
       & char( 12), char( 59), char(  0), char(  0), char(  0),&
       & char(  0), char(  0), char(  1), char(  1), char(  0),&
       & char(  0), char(  0), char(  1), char(  2), char(  1),&
       & char(  0), char(  0), char(  0), char(  1), char(  0),&
       & char(  0), char(  0), char( 21), char(  5), char(  0),&
       & char(  0), char(  0), char(  4), char(  0), char(  0),&
       & char( 65), char( 48), char(  0), char(  0), char(  0),&
       & char(  1), char(  0), char(  1), char(  8), char(  0),&
       & char(  0), char(  0), char(  0), char(  6), char(  6),&
       & char(255), char(  0), char(  0), char(  0), char(  9),&
       & char(  7), char(  0), char(  1), char(  1), char(  2),&
       & char( 55), char( 55), char( 55), char( 55), &
       & char(  0), char(  0), char(  0), char(  0) /)
 
  character :: old_val, ov2, ov3, ov4
  integer :: ierr, i

  print *, 'Testing gb_info(), explect and ignore error messages.'

  ! Change the first byte of the message, then try gb_info() - will
  ! not work.
  old_val = cgrib(1)
  cgrib(1) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 1) stop 1
  cgrib(1) = old_val  

  ! Change the GRIB version number, then try gb_info() - will
  ! not work.
  old_val = cgrib(8)
  cgrib(8) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 2) stop 2
  cgrib(8) = old_val  

  ! Change number of section 1, then try gb_info() - will
  ! not work.
  old_val = cgrib(21)
  cgrib(21) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 3) stop 3
  cgrib(21) = old_val  

  ! End message too soon, then try gb_info() - will
  ! not work.
  old_val = cgrib(38)
  ov2 = cgrib(39)
  ov3 = cgrib(40)
  ov4 = cgrib(41)
  cgrib(38) = char(55)
  cgrib(39) = char(55)
  cgrib(40) = char(55)
  cgrib(41) = char(55)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 4) stop 4
  cgrib(38) = old_val  
  cgrib(39) = ov2
  cgrib(40) = ov3
  cgrib(41) = ov4

  ! Change end of message, then try gb_info() - will
  ! not work.
  old_val = cgrib(190)
  cgrib(190) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 5) stop 5
  cgrib(190) = old_val  

  ! Change a section number, then try gb_info() - will
  ! not work.
  old_val = cgrib(42)
  cgrib(42) = char(0)
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 6) stop 6
  cgrib(42) = old_val  

  ! This will work.
  g2_log_level = 3
  call gb_info(cgrib, lcgrib, listsec0, listsec1, numfields, numlocal, maxlocal, ierr)
  if (ierr .ne. 0) stop 10
  if (numfields .ne. 1 .or. numlocal .ne. 1 .or. maxlocal .ne. 3) stop 11
  if (listsec0(1) .ne. 0 .or. listsec0(2) .ne. 2 .or. listsec0(3) .ne. 191) stop 12
  do i = 1, sec1_len
     if (listsec1(i) .ne. expected_listsec1(i)) stop 13
  end do
  g2_log_level = 0

  ! Test gribinfo() as well. This won't work because we change number of section 1.
  old_val = cgrib(21)
  cgrib(21) = char(0)
  call gribinfo(cgrib, lcgrib, listsec0, listsec1, numlocal, numfields, maxvals, ierr)
  if (ierr .ne. 3) stop 25
  cgrib(21) = old_val  

  ! Test gribinfo() as well.
  call gribinfo(cgrib, lcgrib, listsec0, listsec1, numlocal, numfields, maxvals, ierr)
  if (ierr .ne. 0) stop 20
  if (numfields .ne. 1 .or. numlocal .ne. 1) stop 21
  if (maxvals(1) .ne. 3 .or. maxvals(2) .ne. 58 .or. maxvals(3) .ne. 1 .or. maxvals(4) .ne. 25 &
       .or. maxvals(5) .ne. 1 .or. maxvals(6) .ne. 10 .or. maxvals(7) .ne. 4) stop 22
  if (listsec0(1) .ne. 0 .or. listsec0(2) .ne. 2 .or. listsec0(3) .ne. 191) stop 12
  do i = 1, sec1_len
     if (listsec1(i) .ne. expected_listsec1(i)) stop 13
  end do

  print *, 'OK!'
  print *, 'SUCCESS!'

end program test_gb_info
