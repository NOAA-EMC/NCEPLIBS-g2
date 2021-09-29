program test_g2
  use grib_mod
  implicit none

  character(len=5) :: version_in

  print *, 'Testing version string...'
  print *, G2_VERSION

  ! Open the VERSION file and get the value.
  open(20, file = 'VERSION', status = 'old')
  read(20,'(A5)') version_in
  close(20)
  print *, G2_VERSION, 'g2lib-' // version_in

  ! Check the value of G2_VERSION.
  if (G2_VERSION .ne. 'g2lib-' // version_in) stop 2
  
  print *, 'SUCCESS!'
end program test_g2
