!> This test is for decoding full grib2 message. Code was translated
!> from the C version originated from Dusan. 
!> Dusan Jovic July 2021
!>

       program tst_decode
       use grib_mod
       use g2grids
       use params
       use gridtemplates

       integer fgrib_len, iret, i
       character, dimension(195) :: fgrib
       real, dimension(121) :: fld_ok
       integer G2_ERROR, numfields, numlocal, maxlocal
       character, dimension(195) :: hex
       integer, dimension(3) :: listsec0, listsec0_ok
       integer, dimension(13) :: listsec1, listsec1_ok
       type(gribfield) :: gfld
    
       print *,"Testing decoding full grib2 message.\n"

!        read(fgrib(1:75),'(z4)')
         fgrib(:)=(/                    & 
     &  "G", "R", "I", "B", "2", "0", &
     &  "2", "2", "0", "0", "0", "0", &
     &  "0", "0", "0", "3", "0", "0", &
     &  "0", "4", "7", "0", "1", "0", &
     &  "0", "2", "1", "1", "1", "2", &
     &  "1", "5", "4", "0", "0", "0", &
     &  "3", "0", "0", "0", "H", "6", &
     &  "0", "0", "0", "0", "y", "0", &
     &  "0", "0", "0", "4", "0", "0", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "0", "0", "0", "0", "5", "0", &
     &  "0", "0", "5", "0", "0", "0", &
     &  "0", "0", "0", "0", "0", "3", &
     &  "I", "C", "B", "3", "1", "5", &
     &  "0", "0", "2", "9", "Z", "0", &
     &  "3", "1", "1", "1", "0", "S", &
     &  "B", "6", "0", "1", "B", "0", &
     &  "0", "0", "0", "0", "4", "E", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "2", "0", "`", "0", "0", "0", &
     &  "3", "0", "0", "0", "4", "3", &
     &  "0", "0", "0", "0", "0", "8", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "0", "0", "9", "7", "0", "0", &
     &  "0", "y", "0", "0", "0", "0", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "3", "0", "0", "0", "0", "4", &
     &  "4", "ÿ", "0", "0", "0","7", &
     &  "0", "0", "0", "0", "0", "0", &
     &  "0", "0", "5", "0", "3", "8", &
     &  "8", "5", "6", "9", "7", "7", &
     &  "7", "7", "7" /)
        print*, 101
!       read(fgrib(1:195),'(195z4)')hex(1:195)      
        fgrib_len = 195
        G2_ERROR = 2
       listsec0_ok(:) = (/ 2, 2, 195 /)
       listsec1_ok(:) = (/ 7, 0, 2, 1, 1, 2021, 7, 14, 6, 0, 0, 0, 1 /)
        print*, 102
       fld_ok(:) = (/                    &
     &  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
     &  0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, &
     &  1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, &
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     &  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, &
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     &  0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, &
     &  0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, &
     &  0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, &
     &  1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1 /)
       
        print*, 03
        call gb_info(fgrib, fgrib_len, listsec0, listsec1, &
     &  numfields, numlocal, maxlocal, iret)
       if (iret .ne. 0) stop 101

       do i =0,2
         if (listsec0(i) .ne. listsec0_ok(i)) stop 102
       end do

       do i =0,12
         if (listsec1(i) .ne. listsec1_ok(i)) stop 103
       end do

       if (numfields .ne. 1) stop 104
       if (numlocal .ne. 0)  stop 105
        print*, 04
       call gf_getfld(fgrib, fgrib_len, 1, 1, 1, gfld, iret)
       if (iret .ne. 0) stop 106

       if (gfld%version .ne. 2) stop 107

       if (gfld%ndpts .ne. 121) stop 108

       do i =0,(gfld%ndpts-1) 
        if (gfld%fld(i) .ne. fld_ok(i)) stop 109
       end do
       print*, 05
       call gf_free(gfld)

       print *,"SUCCESS!\n"
       
       end program tst_decode
