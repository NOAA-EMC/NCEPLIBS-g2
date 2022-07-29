! This is a test program for NCEPLIBS-g2.
!
! This program tests the gdt2gds.F90 file.
!
! Ed Hartnett 7/28/22
program test_gdt2gds
  implicit none
  
  integer :: MAXKGDS, MAXIGDS
  parameter (MAXKGDS = 22)
  parameter (MAXIGDS = 22)
  integer :: idefnum = 0
  integer :: igds(5), igdstmpl(MAXIGDS), ideflist(1)
  integer :: kgds(200), igrid, iret
  integer :: i
  integer :: latlon_kgds(MAXKGDS) = (/ 0, 1000, 1000, 1, 1, 136, 1, 1, 1, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: latloni_kgds(MAXKGDS) = (/ 0, 65535, 1, 1, 1, 136, 1, 1, 65535, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0 /)
  integer :: mercator_kgds(MAXKGDS) = (/ 1, 1000, 1000, 1, 1, 136, 1, 1, 1, 0, 1000, 1, 1, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: lambert_kgds(MAXKGDS) = (/ 3, 1000, 1000, 1, 1, 136, 1, 1, 1, 1000, 1000, 1, 1, 1, 1, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: gaussian_kgds(MAXKGDS) = (/ 4, 1000, 1000, 1, 1, 136, 1, 1, 1, 1000, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: polar_kgds(MAXKGDS) = (/ 5, 1000, 1000, 1, 1, 136, 1, 1, 1, 1000, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: curvilinear_kgds(MAXKGDS) = (/ 204, 1000, 1000, 0, 0, 136, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: curvilineari_kgds(MAXKGDS) = (/ 204, 65535, 1, 0, 0, 136, 0, 0, 65535, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0 /)
  integer :: rotate_kgds(MAXKGDS) = (/ 203, 1000, 1000, 1, 1, 136, 1, 1, 1, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
  integer :: rotatei_kgds(MAXKGDS) = (/ 203, 65535, 1, 1, 1, 136, 1, 1, 65535, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0 /)
  integer :: rotate2_kgds(MAXKGDS) = (/ 205, 1000, 1000, 1, 1, 136, 1, 1, 1, 1, 1000, 1, 1, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)

  print *, 'Testing gdt2gds()...'

  print *, 'testing with incorrect grid number...'
  igds(5) = 999
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 1) stop 4
  do i = 1, MAXKGDS
     !print *, i, kgds(i), rotate2_kgds(i)
     if (kgds(i) .ne. 0) stop 10
  end do

  print *, 'testing with Lat/lon grid...'
  igds(5) = 0
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 41
  do i = 1, MAXIGDS
     !print *, i, kgds(i), latlon_kgds(i)
     if (kgds(i) .ne. latlon_kgds(i)) stop 101
  end do
  
  print *, 'testing with Lat/lon grid with irregular grid stuff...'
  igds(5) = 0
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  igdstmpl(8) = -1
  igdstmpl(9) = 1
  idefnum = 1
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 41
  do i = 1, MAXIGDS
     !print *, i, kgds(i), latloni_kgds(i)
     if (kgds(i) .ne. latloni_kgds(i)) stop 101
  end do
  idefnum = 0
  
  print *, 'testing with Mercator grid...'
  igds(5) = 10
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 42
  do i = 1, MAXKGDS
     !print *, i, kgds(i), mercator_kgds(i)
     if (kgds(i) .ne. mercator_kgds(i)) stop 102
  end do

  print *, 'testing with Lambert conformal grid...'
  igds(5) = 30
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 43
  do i = 1, MAXKGDS
     !print *, i, kgds(i), lambert_kgds(i)
     if (kgds(i) .ne. lambert_kgds(i)) stop 103
  end do

  print *, 'testing with Gaussian grid...'
  igds(5) = 40
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 44
  do i = 1, MAXKGDS
     !print *, i, kgds(i), gaussian_kgds(i)
     if (kgds(i) .ne. gaussian_kgds(i)) stop 104
  end do

  print *, 'testing with polar stereographic grid...'
  igds(5) = 20
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 45
  do i = 1, MAXKGDS
     !print *, i, kgds(i), polar_kgds(i)
     if (kgds(i) .ne. polar_kgds(i)) stop 105
  end do

  print *, 'testing with curvilinear orthogonal grid...'
  igds(5) = 204
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 46
  do i = 1, MAXKGDS
     !print *, i, kgds(i), curvilinear_kgds(i)
     if (kgds(i) .ne. curvilinear_kgds(i)) stop 106
  end do

  print *, 'testing with curvilinear orthogonal grid with irregular grid stuff...'
  igds(5) = 204
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  igdstmpl(8) = -1
  igdstmpl(9) = 1
  idefnum = 1
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 46
  do i = 1, MAXKGDS
     !print *, i, kgds(i), curvilineari_kgds(i)
     if (kgds(i) .ne. curvilineari_kgds(i)) stop 106
  end do
  idefnum = 0
  
  print *, 'testing with rotate lat/lon grid...'
  igds(5) = 32768
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 47
  do i = 1, MAXKGDS
     !print *, i, kgds(i), rotate_kgds(i)
     if (kgds(i) .ne. rotate_kgds(i)) stop 107
  end do

  print *, 'testing with rotate lat/lon grid with irregular grid stuff...'
  igds(5) = 32768
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  igdstmpl(8) = -1
  igdstmpl(9) = 1
  idefnum = 1
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 47
  do i = 1, MAXKGDS
     !print *, i, kgds(i), rotatei_kgds(i)
     if (kgds(i) .ne. rotatei_kgds(i)) stop 107
  end do

  print *, 'testing with second rotate lat/lon grid...'
  igds(5) = 32769
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 48
  do i = 1, MAXKGDS
     !print *, i, kgds(i), rotate2_kgds(i)
     if (kgds(i) .ne. rotate2_kgds(i)) stop 108
  end do

  print *, 'SUCCESS!'
end program test_gdt2gds
