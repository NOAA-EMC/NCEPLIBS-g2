! This is a test program for NCEPLIBS-g2.
!
! This program tests reading the very large FV3 GRIB2 file, using
! getgb2i2().
!
! Ed Hartnett 3/20/24
program test_files_fv3
  use grib_mod
  implicit none
  
  integer :: lugb = 10
  integer :: jdisc = -1, jpdtn = -1, jgdtn = -1, jskp = 0
  integer, dimension(200) :: jids, jpdt, jgdt
  logical :: unpack = .true.
  type(gribfield) :: gfld
  integer :: expected_idsect(13) = (/ 7, 0, 2, 1, 1, 2022, 6, 21, 0, 0, 0, 0, 1 /)
  integer :: expected_igdtmpl(22) = (/ 6, 0, 0, 0, 0, 0, 0, 4881, 2961, 0, 0, -37000000, &
       299000000, 48, 37000000, 61000000, 25000, 25000, 64, -35000000, 247000000, 0 /)
  integer :: expected_ipdtmpl(15) = (/ 3, 0, 2, 0, 134, 0, 0, 1, 0, 1, 0, 0, 255, 0, 0 /)
  integer :: expected_idrtmpl(18) = (/ 1216637952, 0, 3, 17, 0, 1, 0, 0, 0, 560759, 0, &
       5, 1, 1, 63, 6, 2, 3 /)
  integer :: i, idxver, iret, k
  
  print *, 'Testing reading GRIB2 file fv3lam.t00z.prslev.f000.grib2...'

  idxver = 2
  ! Open the file.
  call baopenr(lugb, 'data/fv3lam.t00z.prslev.f000.grib2', iret)
  if (iret .ne. 0) stop 2

  ! Find the last message in the file.
  jids = -9999
  jpdt = -9999
  jgdt = -9999
  jskp = 1080 ! skip to the last message
  call getgb2i2(lugb, 0, jskp, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt,  &
       unpack, idxver, k, gfld, iret)
  if (iret .ne. 0) stop 3
  ! Make sure we found message 1081 in the file.
  if (k .ne. 1081) stop 4
  if (gfld%version .ne. 2 .or. gfld%discipline .ne. 10 .or. gfld%idsectlen .ne. 13 .or. &
       gfld%locallen .ne. 0 .or. gfld%ifldnum .ne. 1 .or. gfld%griddef .ne. 0 .or. &
       gfld%ngrdpts .ne. 14452641 .or. gfld%numoct_opt .ne. 0 .or. gfld%interp_opt .ne. 0 .or. &
       gfld%num_opt .ne. 0 .or. gfld%igdtnum .ne. 1 .or. gfld%ipdtnum .ne. 0 .or. &
       gfld%ipdtlen .ne. 15 .or. gfld%ndpts .ne. 8793914 .or. gfld%idrtnum .ne. 3 .or. &
       gfld%idrtlen .ne. 18 .or. gfld%unpacked .neqv. .false. .or. gfld%expanded .neqv. .true. .or. &
       gfld%ibmap .ne. 0) stop 10
  do i = 1, 13
     if (gfld%idsect(i) .ne. expected_idsect(i)) stop 20
  end do
  do i = 1, 22
     if (gfld%igdtmpl(i) .ne. expected_igdtmpl(i)) stop 30
  end do
  do i = 1, 15
     if (gfld%ipdtmpl(i) .ne. expected_ipdtmpl(i)) then
        print *, 'got gfld%ipdtmpl', gfld%ipdtmpl
        print *, 'expected ', expected_ipdtmpl
        stop 40
     endif
  end do
  do i = 1, 18
     if (gfld%idrtmpl(i) .ne. expected_idrtmpl(i)) stop 50
  end do
  ! do i = 1, 100
  !    print *, gfld%fld(1)
  ! end do

  ! Close the file.
  call baclose(lugb, iret)  
  if (iret .ne. 0) stop 200

  ! Free memory.
  call gf_free(gfld)

  ! Free internal library memory.
  call gf_finalize(iret)
  if (iret .ne. 0) stop 5

  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_files_fv3
