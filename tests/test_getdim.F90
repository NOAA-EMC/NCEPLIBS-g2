!Test for the getdim source file
!Brian Curtis 2021-12-08

! Andrew King 7-31-2023

program test_getdim
     implicit none

     integer, parameter :: lcsec3 = 120
     character(len=1) :: csec3(lcsec3)
     integer :: width, height, iscan

     interface
          subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
               mapgridlen,ideflist,idefnum,ierr)
          character(len=1),intent(in) :: cgrib(lcgrib)
          integer,intent(in) :: lcgrib
          integer,intent(inout) :: iofst
          integer,pointer,dimension(:) :: igdstmpl, ideflist
          integer,intent(out) :: igds(5)
          integer,intent(out) :: ierr, idefnum
          end subroutine gf_unpack3
     end interface

     csec3 = (/ achar(0), achar(0), achar(0), achar(72), &
     achar(3), achar(0), achar(0), achar(0), achar(0), achar(4), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(1), achar(0), achar(0), achar(0), achar(2), achar(3), achar(0), achar(0), achar(0), achar(4), achar(5), &
     achar(0), achar(0), achar(0), achar(6), achar(0), achar(0), achar(0), achar(7), achar(0), achar(0), achar(0), &
     achar(8), achar(0), achar(0), achar(0), achar(9), achar(0), achar(0), achar(0), achar(10), achar(0), achar(0), &
     achar(0), achar(11), achar(0), achar(0), achar(0), achar(12), achar(13), achar(0), achar(0), achar(0), achar(14), &
     achar(0), achar(0), achar(0), achar(15), achar(0), achar(0), achar(0), achar(16), achar(0), achar(0), achar(0), &
     achar(17), achar(18), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), achar(0), &
     achar(0), achar(0), achar(0), achar(0), achar(0), achar(0) /)

     ! Template num at pos 14 of section 3 = igds(5)

     print *, 'Testing getdim ...'
     
     print *, 'Template num 0 (Lat/Lon)'
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 10
     if (height .ne. 8) stop 20
     if (iscan .ne. 18) stop 30

     print *, 'Template num 10 (Mercator)'
     csec3(14) = achar(10)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 11
     if (height .ne. 8) stop 21
     if (iscan .ne. 0) stop 31

     print *, 'Template num 20 (Polar Stereographic)'
     csec3(14) = achar(20)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 12
     if (height .ne. 8) stop 22
     if (iscan .ne. 0) stop 32

     print *, 'Template num 30 (Lambert Conformal)'
     csec3(14) = achar(30)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 13
     if (height .ne. 8) stop 23
     if (iscan .ne. 0) stop 33

     print *, 'Template num 40 (Gaussian)'
     csec3(14) = achar(40)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 14
     if (height .ne. 8) stop 24
     if (iscan .ne. 18) stop 34

     print *, 'Template num 90 (Space View/Orthographic)'
     csec3(14) = achar(90)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 15
     if (height .ne. 8) stop 25
     if (iscan .ne. 0) stop 35

     print *, 'Template num 110 (Equatorial Azimuthal)'
     csec3(14) = achar(110)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 7) stop 16
     if (height .ne. 8) stop 26
     if (iscan .ne. 0) stop 36

     print *, 'Template num 5 (Default case)'
     csec3(14) = achar(5)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 0) stop 17
     if (height .ne. 0) stop 27
     if (iscan .ne. 0) stop 37

     print *, 'Error in gf_unpack3 call'
     csec3(14) = achar(99)
     csec3(5) = achar(0)
     call getdim(csec3, lcsec3, width, height, iscan)
     if (width .ne. 0) stop 18
     if (height .ne. 0) stop 28
     if (iscan .ne. 0) stop 38

     print *, 'SUCCESS!'

end program test_getdim
