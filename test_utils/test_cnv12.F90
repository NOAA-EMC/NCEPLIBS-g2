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
   integer :: exp_gds_mercator(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 1000, 1000, 56, 1000, 1000, 1000, 1000, 0, 1000, 1000, 0, 0, 0 /)
   integer :: exp_gds_lambert(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 1000, 1000, 56, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000 /)
   integer :: exp_gds_gaussian(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 1000, 1000, 56, 1000, 1000, 1000, 1000, 1000, 0, 0, 0/)
   integer :: exp_gds_polar(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 1000, 1000, 56, -60000000, 1000, 1000, 1000, 1000, 1000, 0, 0, 0, 0/)
   integer :: exp_gds_curvilinear(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 0, 0, 56, 0, 0, 0, 0, 1000, 0, 0, 0 /)
   integer :: exp_gds_rotlatlon(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 1000, 1000, 56, 1000, 1000, 1000, 1000, 1000, 0, 0, 0/)
   integer :: exp_gds_rotlatlon2(MAXIGDS) = (/ 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 0, 0, 1000, 1000, 56, 1000, 1000, 1000, 1000, 1000, 1000, 0/)
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

   print *, 'testing with Lambert conformal grid...'
   kgds(1) = 3
   kgds(10) = 1000
   kgds(14) = 1
   kgds(15) = 1
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 13
   if (igds(2) .ne. exp_ngrd) stop 23
   if (igds(5) .ne. exp_gdt_lamb) stop 33
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_lambert(i)) stop 43
   end do

   print *, 'testing with Gaussian grid...'
   kgds(1) = 4
   kgds(10) = 1000
   kgds(12) = 0
   kgds(13) = 0
   kgds(14) = 0
   kgds(15) = 0
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 14
   if (igds(2) .ne. exp_ngrd) stop 24
   if (igds(5) .ne. exp_gdt_gauss) stop 34
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_gaussian(i)) stop 44
   end do

   print *, 'testing with polar stereographic grid...'
   kgds(1) = 5
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 14
   if (igds(2) .ne. exp_ngrd) stop 24
   if (igds(5) .ne. exp_gdt_polar) stop 34
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_polar(i)) stop 44
   end do

   print *, 'testing with curvilinear orthogonal grid...'
   kgds(1) = 204
   kgds(4) = 0
   kgds(5) = 0
   kgds(7) = 0
   kgds(8) = 0
   kgds(9) = 0
   kgds(10) = 0
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 15
   if (igds(2) .ne. exp_ngrd) stop 25
   if (igds(5) .ne. exp_gdt_curv) stop 35
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_curvilinear(i)) stop 45
   end do

   print *, 'testing with rotate lat/lon grid...'
   kgds(1) = 203
   kgds(4) = 1
   kgds(5) = 1
   kgds(7) = 1
   kgds(8) = 1
   kgds(9) = 1
   kgds(10) = 1
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 16
   if (igds(2) .ne. exp_ngrd) stop 26
   if (igds(5) .ne. exp_gdt_rot) stop 36
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_rotlatlon(i)) stop 46
   end do

   print *, 'testing with second rotate lat/lon grid...'
   kgds(1) = 205
   kgds(12) = 1
   kgds(13) = 1
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)

   if (iret .ne. 0) stop 17
   if (igds(2) .ne. exp_ngrd) stop 27
   if (igds(5) .ne. exp_gdt_rot2) stop 37
   do i = 1, MAXIGDS
      if (igdstmpl(i) .ne. exp_gds_rotlatlon2(i)) stop 47
   end do

   print *, 'testing with incorrect grid number...'
   kgds(1) = 999
   igdstmpl = 0
   call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, iret)
   if (iret .ne. 1) stop 4

   print *, 'SUCCESS!'
end program test_cnv12
