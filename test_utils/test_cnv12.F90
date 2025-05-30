! This is a test program for NCEPLIBS-grib_util.
!
! This program tests the cnv12 subroutines.
program test_cnv12
   implicit none
   integer, parameter :: MAXKGDS = 22, MAXIGDS = 22, MAXLEN = 200
   integer :: igds(5), igdstmpl(MAXIGDS), ideflist(1), idefnum
   integer :: kgds(MAXKGDS) = (/ 0, 1000, 1000, 1, 1, 136, 1, 1, 1, 1, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0 /)
   integer :: exp_gds_latlon(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 1000, 1000, 56, 1000, 1000, 1000, &
      1000, 1000, 0, 0, 0 /)
   integer :: exp_gds_mercator(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 56, 1000, 1000, 1000, 1000, 0, &
      1000, 1000, 0, 0, 0 /)
   integer :: exp_ngrd = 1000000, exp_ngrdi = 65535
   integer :: exp_gdt_latlon = 0, exp_gdt_merc = 10, exp_gdt_lamb = 30, exp_gdt_gauss = 40, exp_gdt_polar = 20, &
      exp_gdt_curv = 204, exp_gdt_rot = 32768, exp_gdt_rot2 = 32769 
   integer :: i, iret

   print *, 'testing with Lat/lon grid...'
   igdstmpl = 0
   igds = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 11
   if (igds(2) .ne. exp_ngrd) stop 21
   if (igds(5) .ne. exp_gdt_latlon) stop 31
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_latlon(i)) stop 41
   end do

   print *, 'testing with Mercator grid...'
   kgds(1) = 1
   kgds(10) = 0
   kgds(12) = 1
   kgds(13) = 1
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 12
   if (igds(2) .ne. exp_ngrd) stop 22
   if (igds(5) .ne. exp_gdt_merc) stop 32
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_mercator(i)) stop 42
   end do

   print *, 'SUCCESS!'
end program test_cnv12
