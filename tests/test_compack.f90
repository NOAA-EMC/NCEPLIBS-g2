! This program tests the complex packing and unpacking subroutines of
! the NCEPLIBS-g2 project. Link this to the _4 build of the library.
!
! Brian Curtis 11-12-2021
program test_compack
  implicit none

  integer, parameter :: ndpts = 4
  real(8) :: fld_orig(ndpts), fld(ndpts), fld_in(ndpts)
  integer :: idrstmpl(6), idrsnum
  character*1, dimension(10) :: cpack
  character*1, dimension(4) :: expected_cpack
  integer :: lcpack, lensec
  integer :: i, ierr

  print *, 'Testing compack.'
  expected_cpack = (/ char(0), char(93), char(108), char(64) /)
  lensec = 5

  print *, 'Testing simple call to compack...'
  fld_orig = (/42.3, 43.2, 44.1, 45.4/)
  fld = fld_orig
  idrstmpl = (/42, 1, 1, 0, 0, 0/)
  idrsnum = 2
  call compack(fld, ndpts, idrsnum, idrstmpl, cpack, lcpack)
  print *, 'lcpack: ', lcpack

  if (lcpack .ne. ndpts) stop 2

  print *, 'Testing simple call to comunpack...'
  call comunpack(cpack, lcpack, lensec, idrsnum, idrstmpl, ndpts, &
                 fld_in ,ierr)
  if (ierr .ne. 0) then
      print *, 'ierr: ', ierr
      stop 22
  endif
  do i = 1, ndpts
      print *, 'fld, fldin:', fld_orig(i), fld_in(i)
    if (abs(fld_orig(i) - fld_in(i)) .gt. .1) stop 10
  end do

  print *, 'SUCCESS!'

end program test_compack
