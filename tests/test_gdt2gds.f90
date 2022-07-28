! This is a test program for NCEPLIBS-g2.
!
! This program tests the gdt2gds.F90 file.
!
! Ed Hartnett 7/28/22
program test_gdt2gds
  implicit none
  
  integer :: idefnum = 0
  integer :: igds(5), igdstmpl(19), ideflist(1)
  integer :: kgds(200), igrid, iret

  print *, 'Testing gdt2gds()...'

  ! Lat/lon grid.
  igds(5) = 0
  call gdt2gds(igds, igdstmpl, idefnum, ideflist, kgds,  &
     igrid, iret)
  
  print *, 'SUCCESS!'
end program test_gdt2gds
