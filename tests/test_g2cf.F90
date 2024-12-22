! This is a test program for NCEPLIBS-g2.
!
! This program tests the g2cf functions.
!
! Ed Hartnett 11/21/22
program test_g2cf
  use g2cf
  implicit none
  character (len = *), parameter :: fileName = "data/gdaswave.t00z.wcoast.0p16.f000.grib2"
  integer :: g2id, num_msg
  integer(kind = 1) :: discipline
  integer(kind = 4) :: num_fields, num_local
  integer(kind = 2) :: center, subcenter
  integer(kind = 1) :: master_version, local_version
  integer(kind = 1) :: sig_ref_time
  integer(kind = 2) :: year
  integer(kind = 1) :: month, day, hour, minute, second  
  integer :: ierr

  print *, 'Testing g2cf API...'
  !ierr = g2cf_set_log_level(1)

  ! Open the test file.
  ierr = g2cf_open(fileName, 0, g2id)
  if (ierr .ne. 0) stop 2

  ! Check number of messages.
  ierr = g2cf_inq(g2id, num_msg)
  if (ierr .ne. 0) stop 10
  if (num_msg .ne. 19) stop 11

  ! Check the last message.
  ierr = g2cf_inq_msg(g2id, 19, discipline, num_fields, num_local, center, subcenter, &
       master_version, local_version)
  if (ierr .ne. 0) stop 100
  !print *, discipline, num_fields, num_local, center, subcenter, master_version, local_version
  if (discipline .ne. 10 .or. num_fields .ne. 1 .or. num_local .ne. 0 .or. center .ne. 7 .or. &
       subcenter .ne. 0 .or. master_version .ne. 2 .or. local_version .ne. 1) stop 12

  ! Check the time of the last message.
  ierr = g2cf_inq_msg_time(g2id, 19, sig_ref_time, year, &
       month, day, hour, minute, second)
  !print *, sig_ref_time, year, month, day, hour, minute, second
  if (sig_ref_time .ne. 1 .or. year .ne. 2021 .or. month .ne. 11 .or. day .ne. 30 .or. &
       hour .ne. 0 .or. minute .ne. 0 .or. second .ne. 0) stop 101

  ! Close the file.
  ierr = g2cf_close(g2id)
  if (ierr .ne. 0) stop 20

  print *, 'SUCCESS!'
end program test_g2cf
