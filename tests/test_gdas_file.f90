! This program tests the NCEPLIBS-g2 library with a GRIB2 file from
! GDAS.
!
! Ed Hartnett 10/1/21
program test_gdas_file
  use grib_mod
  implicit none

  character(36) :: filename = 'gdaswave.t00z.arctic.9km.f000.grib2'
  integer :: jids(13)
  integer :: k
  type(gribfield) :: gfld
  integer :: ierr
  
  print *, 'Testing reading a GDAS sample file ', filename

  ! Open the file.
  print *, 'Opening file...'
  open(12, file = filename, form = 'unformatted', status = 'old')

  print *, 'Testing getgb2()...'
  call getgb2(12, 0, 0, -1, jids, -1, -9999, -1, -9999, .true., k, gfld, ierr)
  if (ierr .ne. 0) stop 20

  print *, 'Closing file...'
  close(12)
  
  print *, 'SUCCESS!'
end program test_gdas_file
