! This program tests the complex packing and unpacking subroutines of
! the NCEPLIBS-g2 project. Link this to the _4 build of the library.
!
! Brian Curtis 11-12-2021
program test_compack
  implicit none

  integer, parameter :: ndpts = 4
  real(8) :: fld_orig(ndpts), fld(ndpts), fld_in(ndpts)
  integer :: idrstmpl(16)
  integer :: idrsnum
  character*1, dimension(50) :: cpack
  integer :: lcpack, lensec
  integer :: i, ierr

  print *, 'Testing compack.'
  lensec = ndpts

  print *, 'Testing simple call to compack...'
  fld_orig = (/42.3, 43.2, 44.1, 45.0/)
  fld = fld_orig
  ! idrstmpl = (/42, 1, 1, 0, 0, 0/)
  idrstmpl(1) = 42
  idrstmpl(2) = 1
  idrstmpl(3) = 1
  idrstmpl(7) = 0
  idrstmpl(8) = 0
  idrstmpl(9) = 0
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
      print *, 'fld_orig, fldin:', fld_orig(i), fld_in(i)
    if (abs(fld_orig(i) - fld_in(i)) .gt. .1) stop 10
  end do

  print *, 'SUCCESS!'

end program test_compack
