!>    This test is a test for the NCEPLIBS-g2 library.
!>    It fully decode all grib2 message. Code referenced to the g2c test by Dusan.
!>    Extra functions and code for fortran are added. Data was designed for 
!>    a typical grib2 file in fortran, with zero values to the variables. 
!>    
!>    Hang Lei 2021-11-21
!>

       program tst_decode
       use grib_mod
       use g2grids
       use params
       use gridtemplates


       integer :: fgrib_len, iret, i, Nan
       character, dimension(269) :: fgrib
       real, dimension(225) :: fld_ok
       integer :: G2_ERROR, numfields, numlocal, maxlocal
       integer, dimension(3) :: listsec0, listsec0_ok
       integer, dimension(13) :: listsec1, listsec1_ok
       type(gribfield) :: gfld
       integer :: k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
       integer :: n1, n2, n3

       print *,"Testing decoding full grib2 message.\n"

       ! To initialize large parameters in typical grib2 file.       
       k1=6371200                   ! Radius of the earth
       k2=1073                      ! Nx for lambert 5km
       k3=689                       ! Ny for lambert 5km
       k4=2019199                   ! La1 - latitude of first grid point 
       k5=2384459                   ! Lo1 - longitude of first grid point
       k6=25000000                  ! Latitude where grid spacing is defined
       k7=265000000                 ! Orientation longitude
       k8=2539703                   ! X & Y direction grid length
       k9=-90000000                 ! Latitude of pole 
       k10=739297                   ! Number of grid Nx*Ny
       Nan=9999                     ! Missing value
       fgrib_len=269                ! Grib message length

       ! Large numbers in message initials
       n1=229
       n2=254
       n3=255

       ! The grib message for a typical grib2 file in fortran.
       fgrib(:)=(/                                                    &
       ! section 0
     & "G", "R", "I", "B", achar(0), achar(0),                        &
     & achar(0), achar(2), achar(0), achar(0), achar(0), achar(0),    &
     & achar(0), achar(0), achar(1), achar(13),                       &
       ! section 1
     & achar(0), achar(0), achar(0), achar(21), achar(1), achar(0),   &
     & achar(8), achar(0), achar(0), achar(1), achar(0), achar(1),    &
     & achar(7), achar(n1), achar(11), achar(15), achar(10),          &
     & achar(10), achar(10), achar(0), achar(1),                      &
       ! section 2
     & achar(0), achar(0), achar(0), achar(11), achar(2),             &
     & achar(1), "H", "K", "T", "A", "R",                             &
       ! section 3
     & achar(0), achar(0), achar(0), achar(81), achar(3), achar(0),   &
     & achar(0), achar(0), achar(0), achar(k10), achar(0), achar(0),  &
     & achar(0), achar(30), achar(1), achar(0), achar(k1), achar(k1), &
     & achar(k1), achar(k1), achar(0), achar(0), achar(0), achar(0),  &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(k2), achar(k2), achar(k2), achar(k2), achar(k3),         &
     & achar(k3), achar(k3), achar(k3), achar(k4), achar(k4),         &
     & achar(k4), achar(k4),achar(k5),achar(k5), achar(k5),           &
     & achar(k5), achar(0), achar(k6), achar(k6), achar(k6),          &
     & achar(k6), achar(k7), achar(k7), achar(k7), achar(k7),         &
     & achar(k8), achar(k8), achar(k8), achar(k8), achar(k8),         &
     & achar(k8), achar(k8), achar(k8), achar(0), achar(80),          &
     & achar(k6), achar(k6), achar(k6), achar(k6), achar(k6),         &
     & achar(k6), achar(k6), achar(k6), achar(k9), achar(k9),         &
     & achar(k9), achar(k9), achar(0), achar(0), achar(0), achar(0),  &
       ! section 4
     & achar(0), achar(0), achar(0), achar(71), achar(4), achar(0),   &
     & achar(0), achar(0), achar(9), achar(1), achar(8), achar(2),    &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(0), achar(3), achar(6), achar(9), achar(1), achar(0),    &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(0), achar(0), achar(0),  achar(0), achar(0), achar(0),   &
     & achar(1), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(3), achar(0), achar(0), achar(0), achar(n2), achar(7),   & 
     & achar(n1), achar(11), achar(15), achar(12), achar(20),         &
     & achar(10), achar(1), achar(0), achar(0), achar(0), achar(0),   &
     & achar(1), achar(0), achar(1), achar(0), achar(0), achar(0),    &
     & achar(12), achar(1), achar(0), achar(0), achar(0), achar(0),   &
       ! section 5
     & achar(0), achar(0), achar(0), achar(47), achar(5),  achar(0),  &
     & achar(0), achar(0), achar(k10), achar(0), achar(2),  achar(0), &
     & achar(0), achar(0), achar(0), achar(0), achar(0),  achar(0),   &
     & achar(1), achar(8), achar(1), achar(1), achar(1), achar(Nan),  &
     & achar(Nan), achar(Nan), achar(Nan), achar(0), achar(0),        &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
     & achar(0), achar(0), achar(0), achar(0), achar(0), achar(0),    &
       ! section 6
     & achar(0), achar(0), achar(0), achar(6), achar(6),  achar(n3),  &
       ! section 7
     & achar(0), achar(0), achar(0), achar(12), achar(7),  achar(1),  &
     & achar(0), achar(0), achar(0), achar(0), achar(0),  achar(0),   &
       ! section 8
     & "7", "7", "7", "7" /) 
   
      ! Test g2_gbytesc.f90 to get grib header info.
       call g2_gbytec(fgrib,i,32,8)
       if (i .ne. 0) stop 101
       call G2_GBYTESC(fgrib,i,56,8,0,1)
       if (i .ne. 2) stop 102    ! check grib version.

      ! Test gb_info.f90 to get full grib2 messages.

       G2_ERROR = 2
       listsec0_ok(:) = (/ 0, 2, 269 /)
       listsec1_ok(:) = (/ 8, 0, 1, 0, 1, 2021, 11, 15, 10, 10, 10, 0, 1 /)
       fld_ok(:) = 0                  ! zero data field.
       
       call gb_info(fgrib, fgrib_len, listsec0, listsec1,             &
     &  numfields, numlocal, maxlocal, iret)
        
       if (iret .ne. 0) stop 103

       do i =1,3
         if (listsec0(i) .ne. listsec0_ok(i)) stop 104
       end do

       do i =1,13
         if (listsec1(i) .ne. listsec1_ok(i)) stop 105
       end do

       if (numfields .ne. 1) stop 106
       if (numlocal .ne. 1)  stop 107

      ! Test gf_getfld.f90 to get data from grib2 message.       

       call gf_getfld(fgrib, fgrib_len, 1, 1, 1, gfld, iret)

       if (iret .ne. 0) stop 108

       if (gfld%version .ne. 2) stop 109

       if (gfld%ndpts .ne. 225) stop 110

       do i =1,(gfld%ndpts) 
        if (gfld%fld(i) .ne. fld_ok(i)) stop 111
       end do
      ! Release gfld
       call gf_free(gfld)

       print *,"SUCCESS!\n"
       
       end program tst_decode
