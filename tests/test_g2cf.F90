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
  integer ::  pds_template_len, gds_template_len, drs_template_len
  integer(kind = 8) ::  pds_template(G2_MAX_PDS_TEMPLATE_LEN), gds_template(G2_MAX_GDS_TEMPLATE_LEN), &
       drs_template(G2_MAX_DRS_TEMPLATE_LEN)
  integer(kind = 8) :: expected_pds_template(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, &
       0, 1, 0, 1, 255, 0, 0 /)
  integer(kind = 8) :: expected_gds_template(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, &
       151, 0, 0, 50000000, 210000000, 48, 25000000, 250000000, 166667, 166667, 0  /)
  integer(kind = 8) :: expected_drs_template(7) = (/ 1092616192, 0, 2, 11, 0, 0, 255 /)
  integer :: i
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
  if (ierr .ne. 0) stop 200
  !print *, sig_ref_time, year, month, day, hour, minute, second
  if (sig_ref_time .ne. 1 .or. year .ne. 2021 .or. month .ne. 11 .or. day .ne. 30 .or. &
       hour .ne. 0 .or. minute .ne. 0 .or. second .ne. 0) stop 101

  ! Learn about the first product in the last message.
  ierr = g2cf_inq_prod(g2id, 1, 1, pds_template_len, pds_template, gds_template_len, &
       gds_template, drs_template_len, drs_template)
  if (ierr .ne. 0) stop 300
  !print *, pds_template_len, gds_template_len, drs_template_len
  if (pds_template_len .ne. 15 .or. gds_template_len .ne. 19 .or. drs_template_len .ne.7) stop 301
  !print *, pds_template
  do i = 1, pds_template_len
     if (pds_template(i) .ne. expected_pds_template(i)) stop 302
  end do
  !print *, gds_template
  do i = 1, gds_template_len
     if (gds_template(i) .ne. expected_gds_template(i)) stop 302
  end do
  !print *, drs_template
  do i = 1, drs_template_len
     if (drs_template(i) .ne. expected_drs_template(i)) stop 302
  end do

  ! Close the file.
  ierr = g2cf_close(g2id)
  if (ierr .ne. 0) stop 20

  print *, 'SUCCESS!'
end program test_g2cf
