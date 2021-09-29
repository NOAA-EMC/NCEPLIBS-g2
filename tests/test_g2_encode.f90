! This is a test for the NCEPLIBS-g2 library.
!
! In this test we try out the encode() subroutine.
!
! Ed Hartnett 9/29/21
program test_g2_encode
  implicit none

  integer, parameter :: MAX_MSG_LEN = 256
  character (len = MAX_MSG_LEN) :: msg
  integer :: listsec0(2)
  integer :: listsec1(13)
  integer :: ierr

  print *, 'Testing g2 library encoding.'

!  listsec0(1) Discipline-GRIB Master Table Number (Code Table 0.0)
!  listsec0(2) GRIB Edition Number (currently 2)
  listsec0(1) = 0
  listsec0(2) = 2

! listsec1(1) Id of orginating centre (Common Code Table C-1)
! listsec1(2) Id of orginating sub-centre (local table)
! listsec1(3) GRIB Master Tables Version Number (Code Table 1.0)
! listsec1(4) GRIB Local Tables Version Number (Code Table 1.1)
! listsec1(5) Significance of Reference Time (Code Table 1.2)
! listsec1(6) Reference Time - Year (4 digits)
! listsec1(7) Reference Time - Month
! listsec1(8) Reference Time - Day
! listsec1(9) Reference Time - Hour
! listsec1(10) Reference Time - Minute
! listsec1(11) Reference Time - Second
! listsec1(12) Production status of data (Code Table 1.3)
! listsec1(13) Type of processed data (Code Table 1.4)
  listsec1(1) = 0
  listsec1(2) = 0
  listsec1(3) = 0
  listsec1(4) = 0
  listsec1(5) = 0
  listsec1(6) = 2021
  listsec1(7) = 1
  listsec1(8) = 31
  listsec1(9) = 12
  listsec1(10) = 59
  listsec1(11) = 59
  listsec1(12) = 0
  listsec1(13) = 0
  
  call gribcreate(msg, MAX_MSG_LEN, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 2
  
  print *, 'SUCESSS!'
end program test_g2_encode
