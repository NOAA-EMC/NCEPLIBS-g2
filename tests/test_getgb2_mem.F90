! This is a test for NCEPLIBS-g2.
!
! This tests for a memory leak in getgb2().
!
! Bo Cui, Ed Hartnett, 6/1/23
program test_getgb2_mem
  implicit none

  character(*) :: TEST_FILE_GEP19_BCF144
  parameter (TEST_FILE_GEP19_BCF144 = 'data/gep19.t00z.pgrb2a.0p50_bcf144')
  character(*) :: TEST_FILE_GEAVG
  parameter (TEST_FILE_GEAVG = 'data/geavg.t00z.pgrb2a.0p50_mecomf144')
  character(*) :: TEST_FILE_GEC00
  parameter (TEST_FILE_GEC00 = 'data/gec00.t00z.pgrb2a.0p50.f144')
  character(*) :: TEST_FILE_GEGFS_F144
  parameter (TEST_FILE_GEGFS_F144 = 'data/gegfs.t00z.pgrb2a.0p50.f144')
  character(*) :: TEST_FILE_GEGFS_MEF144
  parameter (TEST_FILE_GEGFS_MEF144 = 'data/gegfs.t00z.pgrb2a.0p50_mef144')
  character(*) :: TEST_FILE_GEP19_F144
  parameter (TEST_FILE_GEP19_F144 = 'data/gep19.t00z.pgrb2a.0p50.f144')
  integer :: NUM_TEST_FILES
  parameter (NUM_TEST_FILES = 6)
  character(len=120), dimension(NUM_TEST_FILES) :: test_file
  
  integer :: inver = 0,outver = 0,ipack = -1
  character(len = 500) :: gfilein
  INTEGER(4) NARG,IARGC
  logical :: usemiss = .false., uvvect = .true.
  integer lugb
  parameter(lugb = 10)
  integer :: ios, ncgb
  integer :: i
  
  test_file = [character(len=120) :: TEST_FILE_GEP19_BCF144, TEST_FILE_GEAVG, TEST_FILE_GEC00, &
       TEST_FILE_GEGFS_F144, TEST_FILE_GEGFS_MEF144, TEST_FILE_GEP19_F144]
  
  do i = 1, NUM_TEST_FILES
     print *, 'Opening GRIB2 file ', test_file(i)
     
     ! Open input GRIB2 file.
     call baopenr(lugb + i, test_file(i), ios)
     if (ios .ne. 0) stop 10

     ! Readd grib file.
     call gb2read(lugb + i)

     ! Close grib file.
     call baclose(lugb + i, ios)
     if (ios .ne. 0) stop 11
  end do

end program test_getgb2_mem

! Read GRIB2 file.
subroutine gb2read(ifl1)
  use grib_mod
  use params
  implicit none
  !     integer,intent(in) :: ifl1
  integer ifl1, icnt
  real dmax, dmin

  CHARACTER(len = 1),allocatable,dimension(:) :: cgrib
  CHARACTER(len = 8) :: ctemp
  type(gribfield) :: gfld
  integer,dimension(20) :: jpdt,jgdt
  integer,dimension(13) :: jids
  integer :: kpds(200),kgds(200),kens(200),kprob(2)
  integer :: kclust(16),kmembr(80)
  integer :: currlen = 0
  integer :: igds(5) = (/0,0,0,0,0/)
  real :: xprob(2)
  logical*1,target,dimension(1) :: dummy
  logical :: unpack = .true.

  integer nvar

  parameter (nvar = 5)

  integer     ipd1(nvar),ipd2(nvar),ipd10(nvar),ipd3(nvar)
  integer     ipd11(nvar),ipd12(nvar),ipdn(nvar)
  integer     ipdnm(nvar),ipdnr(nvar),ipdna(nvar)
  integer     ipd11_cmc(nvar),ipd12_cmc(nvar)
  integer :: kf, newlen, jdisc, jgdtn, jskp, jpdtn, j, ivar, is
  integer :: iret, imug, igrid, ifli1, i, ibs, icount
  real :: rmiss1, rmiss2

  ! 01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19  20
  ! 21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
  !  Z   Z   Z   Z   Z   Z   Z   Z   Z   Z   T   T   T   T   T   T   T   T   T   T
  !  U   U   U   U   U   U   U   U   U   U   V   V   V   V   V   V   V   V   V   V
  !
  ! 41   42  43  44   45   46   47   48         49             50         51   52    53
  ! pres slp t2m u10m v10m tmax tmin ULWRF(OLR) ULWRF(Surface) VVEL(850w) rh2m dpt2m tcdc
  !
  ! 54
  ! wspd10m
  !
  ! -----------------------------------------------------------------------------------------

  ! ipdn: product definition template number grib2 - code table 4.0
  ! ipd1:  parameter category (3 means mass field))
  ! ipd2:  parameter number
  ! ipd10: type of first fixed surface
  ! ipd11: scale factor of first fixed surface
  ! ipd12: Scaled value of first fixed surface

  ! ipdnm: ensemble average forecast, product template 4.2 or 4.12

  data ipdnm/  2,  2,  2,  2,  2/

  ! ipdn: ensemble control forecast, product template 4.1 or 4.11

  data ipdn /  1,  1,  1,  1,  1/

  data ipd1 /  3,  3,  3,  3,  3 /

  data ipd2 /  5,  5,  5,  5,  5/
  data ipd3 /  4,  4,  4,  4,  4/

  data ipd10/100,100,100,100,100/

  data ipd11/  0,  0,  0,  0,  0/

  data ipd12/100000,92500, 85000, 70000, 50000/

  IFLI1 = 0
  jdisc = -1
  jids = -9999
  jpdt = -9999
  jgdt = -9999
  jpdtn = -1
  jgdtn = -1

  icnt = 0
  dmax = 0.0
  dmin = 0.0
  icount = 0
  !     jskp = 0
  !     do
  do ivar  =  1, 5
     jskp = 0
     jdisc = -1
     jids = -9999
     jpdt = -9999
     jgdt = -9999
     jpdtn = -1
     jgdtn = -1

     jids(6) = 2023 ! year
     jids(7) = 4     ! mon
     jids(8) = 30          ! day
     jids(9) = 0           ! hr

     jgdt(1) = 6
     jgdt(8) = 720
     jgdt(9) = 361

     !       jgdtn = 0

     jpdt(1) = ipd1(ivar)
     jpdt(2) = ipd2(ivar)
     jpdt(3) = ipd3(ivar)
     jpdt(10) = ipd10(ivar)
     jpdt(11) = ipd11(ivar)
     jpdt(12) = ipd12(ivar)
     jpdtn = ipdn(ivar)

     call getgb2(ifl1,0,0,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt, &
          unpack,jskp,gfld,iret)

     if (iret.ne.0) then
        print *,' getgb2 returned ',iret
!        if (iret .eq. 99) stop 2
        cycle
        !call errexit(17)
     endif
     icount = icount+1

     !  Ensure that cgrib array is large enough
     newlen = 4*gfld%ngrdpts
     if (newlen.gt.currlen) then
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(newlen),stat = is)
        currlen = newlen
     endif

     !   Construct GDS
     igds(1) = gfld%griddef
     igds(2) = gfld%ngrdpts
     igds(3) = gfld%numoct_opt
     igds(4) = gfld%interp_opt
     igds(5) = gfld%igdtnum
     if (.NOT. associated(gfld%list_opt)) allocate(gfld%list_opt(1))
     call gdt2gds(igds,gfld%igdtmpl,gfld%num_opt,gfld%list_opt, &
          kgds,igrid,iret)
     if (iret.ne.0) then
        print *,'cnv21: could not create gds'
        cycle
     endif
     !print *,' SAGT: NCEP GRID: ',igrid

     !   Construct PDS
     call makepds(gfld%discipline,gfld%idsect,gfld%ipdtnum, &
          gfld%ipdtmpl,gfld%ibmap,gfld%idrtnum, gfld%idrtmpl,kpds,iret)
     if (iret.ne.0) then
        print *,'cnv21: could not create pds'
        cycle
     endif
     kpds(3) = igrid

     !   Construct Ensemble info, if necessary
     if ((gfld%ipdtnum.ge.1.AND.gfld%ipdtnum.le.6).OR. &
          (gfld%ipdtnum.ge.9.AND.gfld%ipdtnum.le.14)) then
        call makepdsens(gfld%ipdtnum,gfld%ipdtmpl,kpds,kens,kprob, &
             xprob,kclust,kmembr,iret)
     endif

     !   If not using bit-map, must assign dummy bit-map
     if (gfld%ibmap.ne.0 .AND. gfld%ibmap.ne.254) then
        !gfld%bmap  = > dummy
        if ((gfld%idrtnum .eq. 2 .OR. gfld%idrtnum .eq. 3) .AND. &
             gfld%idrtmpl(7).ne.0) then       ! convert missings to bitmap
           allocate(gfld%bmap(gfld%ngrdpts))
           kpds(4) = ior(kpds(4),64)
           if (gfld%idrtmpl(7) .eq. 1) then
              call rdieee(gfld%idrtmpl(8),rmiss1,1)
              do i = 1,gfld%ngrdpts
                 if (gfld%fld(i)  .eq.  rmiss1) then
                    gfld%bmap(i) = .false.
                 else
                    gfld%bmap(i) = .true.
                 endif
              enddo
           endif
           if (gfld%idrtmpl(7) .eq. 2) then
              call rdieee(gfld%idrtmpl(8),rmiss1,1)
              call rdieee(gfld%idrtmpl(9),rmiss2,1)
              do i = 1,gfld%ngrdpts
                 if (gfld%fld(i) .eq. rmiss1 .OR. gfld%fld(i) .eq. rmiss2) then
                    gfld%bmap(i) = .false.
                 else
                    gfld%bmap(i) = .true.
                 endif
              enddo
           endif
        endif
     endif

     !   Pack and write GRIB 1 field
     ibs = gfld%idrtmpl(2)
     !print *,'SAGT:before putgbexn'
     if (.NOT. associated(gfld%bmap)) allocate(gfld%bmap(1))
     imug = 0
     ! print out
     kf = gfld%ngrdpts
     !      do i  =  1, kf
     !       data(i)  =  gfld%fld(i)
     !      enddo
     !      call grange(kf,lb,gfld%fld,dmin,dmax)
     !      call grange(kf,gfld%bmap,gfld%fld,dmin,dmax)

     dmin = gfld%fld(1)
     dmax = gfld%fld(1)

     kf = gfld%ngrdpts
     do j = 2,kf
        if (gfld%fld(j).gt.dmax) dmax = gfld%fld(j)
        if (gfld%fld(j).lt.dmin) dmin = gfld%fld(j)
     enddo
     if (icnt.ne.1) then
        write(*,887)
        icnt = 1
     endif
     write(*,888) jskp,(kpds(i),i = 5,11),kpds(14),kpds(15),kpds(16), &
          (kens(i),i = 2,4),kf, dmax,dmin,gfld%fld(8601)

887  format(' REC  PD5 PD6 PD7 YEAR MN  DY  HR  F1  F2  FU  ', &
          'E2  E3  E4   LEN      MAX        MIN       Sample ')
888  format(3i4,i5,7i4,3i4,i8,3f11.2)


     !        call putgbexn(ifl2,gfld%ngrdpts,kpds,kgds,kens,kprob,
     !    &                 xprob,kclust,kmembr,ibs,imug,gfld%bmap,
     !    &                 gfld%fld,iret)
     !print *,'SAGT:after putgbexn'
     !        if (iret.ne.0) then
     !           print *,' putgbexn error  =  ',iret
     !           cycle
     !call errexit(17)
     !        endif

     call gf_free(gfld)

  enddo

  if (allocated(cgrib)) deallocate(cgrib)

  return
end subroutine gb2read

! Create the GRIB1 NCEP Ensemble PDS extension information from
! appropriate information from a GRIB2 Product Definition Template.
!
subroutine makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob, &
     xprob,kclust,kmembr,iret)
  use params
  implicit none
  
  integer,intent(in) :: ipdstmpl(*)
  integer,intent(in) :: ipdsnum
  integer,intent(inout) :: kpds(*)
  integer,intent(out) :: kens(5),kprob(2)
  integer,intent(out) :: kclust(16),kmembr(80)
  real,intent(out) :: xprob(2)
  integer,intent(out) :: iret
  real :: rscale

  iret = 0
  kpds(23) = 2          !  subcenter  =  ensemble

  kens(1:5) = 0
  kprob(1:2) = 0
  xprob(1:2) = 0.
  kclust(1:16) = 0
  kmembr(1:80) = 0

  !  Individual Ensemble Fcst
  if (ipdsnum .eq. 1.OR.ipdsnum .eq. 11) then
     kens(1) = 1
     selectcase (ipdstmpl(16))
     case(0)
        kens(2) = 1
        kens(3) = 1
     case(1)
        kens(2) = 1
        kens(3) = 2
     case(2)
        kens(2) = 2
        kens(3) = ipdstmpl(17)
     case(3)
        kens(2) = 3
        kens(3) = ipdstmpl(17)
     end select
     kens(4) = 1
     kens(5) = 255

     !  Probability Fcst
  elseif (ipdsnum .eq. 5.OR.ipdsnum .eq. 9) then
     kens(1) = 1
     kens(2) = 5
     kens(3) = 0
     kens(4) = 0
     kens(5) = 255
     kprob(1) = kpds(5)
     kpds(5) = 191
     kprob(2) = ipdstmpl(18)+1
     if (kprob(2) .eq. 1) then
        rscale = 10.**ipdstmpl(19)
        xprob(1) = real(ipdstmpl(20))*rscale
        xprob(2) = 0.0
     elseif (kprob(2) .eq. 2) then
        xprob(1) = 0.0
        rscale = 10.**ipdstmpl(21)
        xprob(2) = real(ipdstmpl(22))*rscale
     elseif (kprob(2) .eq. 3) then
        rscale = 10.**ipdstmpl(19)
        xprob(1) = real(ipdstmpl(20))*rscale
        rscale = 10.**ipdstmpl(21)
        xprob(2) = real(ipdstmpl(22))*rscale
     endif
     kclust(1) = ipdstmpl(17)

     !  Derived Ensemble Fcst
  elseif (ipdsnum .eq. 2.OR.ipdsnum .eq. 12) then
     kens(1) = 1
     kens(2) = 5
     kens(3) = 0
     selectcase (ipdstmpl(16))
     case(0)
        kens(4) = 1
     case(1)
        kens(4) = 2
     case(2)
        kens(4) = 11
     case(3)
        kens(4) = 12
     end select
     !kens(5) = 89
     kens(5) = 0
     kclust(1) = ipdstmpl(17)
  else
     print *,'makepdsens: Don:t know GRIB2 PDT 4.',ipdsnum
     iret = 2
  endif

  return
end subroutine makepdsens

! This routine creates a GRIB1 PDS (Section 1)
!   from appropriate information from a GRIB2 Product Definition Template.
!
subroutine makepds(idisc,idsect,ipdsnum,ipdstmpl,ibmap,   &
     idrsnum,idrstmpl,kpds,iret)

  use params
  implicit none

  integer,intent(in) :: idsect(*),ipdstmpl(*),idrstmpl(*)
  integer,intent(in) :: ipdsnum,idisc,idrsnum,ibmap
  integer,intent(out) :: kpds(*)
  integer,intent(out) :: iret
  integer :: ipos

  iret = 0
  kpds(1:24) = 0
  if ((ipdsnum.lt.0).OR.(ipdsnum.gt.14)) then
     print *,'makepds: Don:t know GRIB2 PDT 4.',ipdsnum
     iret = 2
     return
  endif

  kpds(1) = idsect(1)
  kpds(2) = ipdstmpl(5)
  kpds(3) = 255
  kpds(4) = 128
  if (ibmap.ne.255) kpds(4) = kpds(4)+64
  if (ibmap.ge.1.AND.ibmap.le.253) then
     print *,'makepds: Don:t know about predefined bit-map ',ibmap
     iret = 1
     return
  endif
  call param_g2_to_g1(idisc,ipdstmpl(1),ipdstmpl(2),kpds(5), kpds(19))
  call levelcnv(ipdstmpl,kpds(6),kpds(7))      ! level
  kpds(8) = mod(idsect(6),100)
  if (kpds(8) .eq. 0) kpds(8) = 100
  kpds(9) = idsect(7)                            ! Year
  kpds(10) = idsect(8)                           ! Month
  kpds(11) = idsect(9)                           ! Day
  kpds(12) = idsect(10)                          ! Hour
  if (ipdstmpl(8).ne.13) then
     kpds(13) = ipdstmpl(8)                      ! Time Unit
  else
     kpds(13) = 254
  endif
  kpds(14) = ipdstmpl(9)                         ! P1
  if (ipdsnum.le.7) then                     ! P2
     kpds(15) = 0
     kpds(16) = 0
     if (kpds(14) .eq. 0) kpds(16) = 1
     if (kpds(14).gt.255) kpds(16) = 10
     if (ipdstmpl(5) .eq. 77.OR.ipdstmpl(5) .eq. 81.OR. &
          ipdstmpl(5) .eq. 96.OR.ipdstmpl(5) .eq. 80.OR. &
          ipdstmpl(5) .eq. 82) kpds(16) = 10
     kpds(20) = 0
  else
     selectcase (ipdsnum)
     case(8)
        ipos = 24
     case(9)
        ipos = 31
     case(10)
        ipos = 25
     case(11)
        ipos = 27
     case(12)
        ipos = 26
     case(13)
        ipos = 40
     case(14)
        ipos = 39
     end select
     kpds(15) = ipdstmpl(ipos+3)+kpds(14)
     selectcase (ipdstmpl(ipos))
     case (255)
        kpds(16) = 2
     case (0)
        kpds(16) = 3
     case (1)
        kpds(16) = 4
     case (2)
        kpds(16) = 2
     case (3)
        kpds(16) = 2
     case (4)
        kpds(16) = 5
     end select
     kpds(20) = ipdstmpl(ipos-1)
  endif
  kpds(17) = 0
  kpds(18) = 1                                   ! GRIB edition
  kpds(21) = (idsect(6)/100)+1                   ! Century
  if (kpds(8) .eq. 100) kpds(21) = idsect(6)/100
  kpds(22) = idrstmpl(3)                         ! Decimal scale factor
  kpds(23) = idsect(2)                           ! Sub-center

  return
end subroutine makepds

! This routine converts Level/layer information
!   from a GRIB2 Product Definition Template to GRIB1
!   Level type and Level value.
subroutine levelcnv(ipdstmpl,ltype,lval)
  implicit none
  integer,intent(in) :: ipdstmpl(*)
  integer,intent(out) :: ltype,lval
  integer :: lval1, lval2, ltype1, ltype2
  real:: rscal1, rscal2

  ltype = 255
  lval = 0
  ltype1 = ipdstmpl(10)
  ltype2 = ipdstmpl(13)

  if (ltype1.lt.100.AND.ltype2 .eq. 255) then
     ltype = ltype1
     lval = 0
  elseif (ltype1.ge.200.AND.ltype2 .eq. 255 ) then
     ltype = ltype1
     lval = 0
  elseif (ltype1 .eq. 100.AND.ltype2 .eq. 255 ) then
     ltype = 100
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1/100.)
  elseif (ltype1 .eq. 100.AND.ltype2 .eq. 100 ) then
     ltype = 101
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1/1000.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2/1000.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 101.AND.ltype2 .eq. 255 ) then
     ltype = 102
     lval = 0
  elseif (ltype1 .eq. 102.AND.ltype2 .eq. 255 ) then
     ltype = 103
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1 .eq. 102.AND.ltype2 .eq. 102 ) then
     ltype = 104
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 103.AND.ltype2 .eq. 255 ) then
     ltype = 105
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1 .eq. 103.AND.ltype2 .eq. 103 ) then
     ltype = 106
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1/100.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2/100.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 104.AND.ltype2 .eq. 255 ) then
     ltype = 107
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1*10000.)
  elseif (ltype1 .eq. 104.AND.ltype2 .eq. 104 ) then
     ltype = 108
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2*100.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 105.AND.ltype2 .eq. 255 ) then
     ltype = 109
     lval = ipdstmpl(12)
  elseif (ltype1 .eq. 105.AND.ltype2 .eq. 105 ) then
     ltype = 110
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 106.AND.ltype2 .eq. 255 ) then
     ltype = 111
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1*100.)
  elseif (ltype1 .eq. 106.AND.ltype2 .eq. 106 ) then
     ltype = 112
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2*100.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 107.AND.ltype2 .eq. 255 ) then
     ltype = 113
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1 .eq. 107.AND.ltype2 .eq. 107 ) then
     ltype = 114
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = 475-nint(real(ipdstmpl(12))*rscal1)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = 475-nint(real(ipdstmpl(15))*rscal2)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 108.AND.ltype2 .eq. 255 ) then
     ltype = 115
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1/100.)
  elseif (ltype1 .eq. 108.AND.ltype2 .eq. 108 ) then
     ltype = 116
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1/100.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2/100.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 109.AND.ltype2 .eq. 255 ) then
     ltype = 117
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1*1000000000.)
  elseif (ltype1 .eq. 111.AND.ltype2 .eq. 255 ) then
     ltype = 119
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1*10000.)
  elseif (ltype1 .eq. 111.AND.ltype2 .eq. 111 ) then
     ltype = 120
     rscal1 = 10.**(-ipdstmpl(11))
     lval1 = nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2 = 10.**(-ipdstmpl(14))
     lval2 = nint(real(ipdstmpl(15))*rscal2*100.)
     lval = (lval1*256)+lval2
  elseif (ltype1 .eq. 160.AND.ltype2 .eq. 255 ) then
     ltype = 160
     rscal1 = 10.**(-ipdstmpl(11))
     lval = nint(real(ipdstmpl(12))*rscal1)
  else
     print *,'levelcnv: GRIB2 Levels ',ltype1,ltype2, &
          ' not recognized.'
     ltype = 255
  endif

  !  High resolution stuff
  !        elseif (ltype .eq. 121) then
  !           ipdstmpl(10) = 100
  !           ipdstmpl(12) = (1100+(lval/256))*100
  !           ipdstmpl(13) = 100
  !           ipdstmpl(15) = (1100+mod(lval,256))*100
  !        elseif (ltype .eq. 125) then
  !           ipdstmpl(10) = 103
  !           ipdstmpl(11) = -2
  !           ipdstmpl(12) = lval
  !        elseif (ltype .eq. 128) then
  !           ipdstmpl(10) = 104
  !           ipdstmpl(11) = -3
  !           ipdstmpl(12) = 1100+(lval/256)
  !           ipdstmpl(13) = 104
  !           ipdstmpl(14) = -3
  !           ipdstmpl(15) = 1100+mod(lval,256)
  !        elseif (ltype .eq. 141) then
  !           ipdstmpl(10)=100
  !           ipdstmpl(12)=(lval/256)*100
  !           ipdstmpl(13)=100
  !           ipdstmpl(15)=(1100+mod(lval,256))*100

  return
end subroutine levelcnv
