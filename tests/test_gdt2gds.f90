! This is a test program for NCEPLIBS-g2.
!
! This program tests the gdt2gds.F90 file.
!
! Ed Hartnett 7/28/22
program test_gdt2gds
  implicit none
  
  integer :: MAXKGDS, MAXIGDS
  parameter (MAXKGDS = 22)
  parameter (MAXIGDS = 19)
  integer :: idefnum = 0
  integer :: igds(5), igdstmpl(MAXIGDS), ideflist(1)
  integer :: kgds(200), igrid, iret
  integer :: i
  integer :: expected_kgds(MAXKGDS) = (/ 0, 1000, 1000, 1, 1, 136, 1, 1, 1, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)

  print *, 'Testing gdt2gds()...'

  ! Lat/lon grid.
  igds(5) = 0
  do i = 1, MAXIGDS
     igdstmpl(i) = 1000
  end do
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
       igrid, iret)
  if (iret .ne. 0) stop 4
  do i = 1, MAXKGDS
     print *, i, kgds(i), expected_kgds(i)
     if (kgds(i) .ne. expected_kgds(i)) stop 10
  end do
  
  print *, 'SUCCESS!'
end program test_gdt2gds
