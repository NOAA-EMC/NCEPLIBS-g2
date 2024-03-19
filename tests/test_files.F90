! This is a test program for NCEPLIBS-g2.
!
! This program tests reading a GRIB2 file.
!
! Ed Hartnett 9/3/22
program test_files
  use grib_mod
  implicit none
  
  integer :: lugb = 10
  integer :: jdisc = -1, jpdtn = -1, jgdtn = -1, jskp = 0
  integer, dimension(200) :: jids, jpdt, jgdt
  logical :: unpack = .true.
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2021, 11, 30, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(19) = (/ 6, 0, 0, 0, 0, 0, 0, 241, 151, 0, 0, 50000000, &
       210000000, 48, 25000000, 250000000, 166667, 166667, 0 /)
  integer :: expected_ipdtmpl(15) = (/ 2, 1, 2, 0, 11, 0, 0, 1, 0, 1, 0, 1, 255, 0, 0 /)
  integer :: expected_idrtmpl(7) = (/ 1092616192, 0, 2, 11, 0, 0, 255 /)
  integer :: i, idxver, iret
  
  print *, 'Testing reading GRIB2 file gdaswave.t00z.wcoast.0p16.f000.grib2...'

  do idxver = 1, 2
     ! Open the file.
     call baopenr(lugb, 'gdaswave.t00z.wcoast.0p16.f000.grib2', iret)
     if (iret .ne. 0) stop 2

     ! Learn about the file.
     jids = -9999
     jpdt = -9999
     jgdt = -9999
     call getgb2i2(lugb, 0, jskp, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
          unpack, idxver, jskp, gfld, iret)
     if (iret .ne. 0) stop 3
     if (gfld%version .ne. 2 .or. gfld%discipline .ne. 0 .or. gfld%idsectlen .ne. 13 .or. &
          gfld%locallen .ne. 0 .or. gfld%ifldnum .ne. 1 .or. gfld%griddef .ne. 0 .or. &
          gfld%ngrdpts .ne. 36391 .or. gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. &
          gfld%num_opt .ne. 0 .or. gfld%igdtnum .ne. 0 .or. gfld%ipdtnum .ne. 0 .or. &
          gfld%ipdtlen .ne. 15 .or. gfld%ndpts .ne. 11041 .or. gfld%idrtnum .ne. 40 .or. &
          gfld%idrtlen .ne. 7 .or. gfld%unpacked .neqv. .false. .or. gfld%expanded .neqv. .true. .or. &
          gfld%ibmap .ne. 0) stop 10
     do i = 1, 13
        if (gfld%idsect(i) .ne. expected_idsect(i)) stop 20
     end do
     do i = 1, 19
        if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) stop 30
     end do
     do i = 1, 15
        if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) stop 40
     end do
     do i = 1, 7
        if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) stop 50
     end do
     do i = 1, 100
        print *, gfld%fld(1)
     end do

     ! Close the file.
     call baclose(lugb, iret)  
     if (iret .ne. 0) stop 200

     ! Free memory.
     call gf_free(gfld)
  end do


  ! Free internal library memory.
  call gf_finalize(iret)
  if (iret .ne. 0) stop 5
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_files
