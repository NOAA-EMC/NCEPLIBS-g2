!>    This test is a test for the NCEPLIBS-g2 library.
!>    It tests g2grids.f90 call all two subrountines and indirectly call
!>    the function. A datafile for test "testdata_g2grids" is added.
!>
!>    Hang Lei 2021-12-27
!>
program tst_g2grids
  use grib_mod
  use g2grids
  use params
  use gridtemplates
  implicit none

  integer :: num_grid
  integer :: id, id2
  integer :: igdt, ierr, i
  integer, dimension(19) :: igdtmp, vdata
  character(len=8) :: gridname

  ! Initialize the variables for tests.
  num_grid = 0
  vdata(:) = (/ 0, 0, 0, 0, 0, 0, 0, 360, 181, 0, 0, 90000000, 0, &
       & 48, -90000000, 359000000, 1000000, 1000000, 0 /)
  id = 15
  id2 = 17
  gridname="gbl_1deg"

  ! Open data files for tests.

  open(id, file = 'testdata_g2grids', status = 'old', action = 'read')

  if (id .ne. 15) stop 101

  ! Test subroutines getgridbynum and getgridbyname.
  call getgridbynum(id, 3, igdt, igdtmp, ierr)
  if (ierr .ne. 0) stop 102
  if (igdt .ne. 0) stop 103

  do i =1,19
     if (igdtmp(i) .ne. vdata(i)) stop 104
  end do

  call getgridbyname(id, gridname, igdt, igdtmp, ierr)
  if (ierr .ne. 0) stop 105
  if (igdt .ne. 0) stop 106

  do i =1, 19
     if (igdtmp(i) .ne. vdata(i)) stop 107
  end do

  close(id)

  ! Free the memory allocated to store grid info.
  call freegridlist()

  print *,"SUCCESS!\n"

end program tst_g2grids
