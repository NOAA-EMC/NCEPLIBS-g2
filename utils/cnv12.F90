!> @file
!> @brief Convert every GRIB1 field in a file to a GRIB2 field.
!> @author Stephen Gilbert @date 2003-06-11

!> This subroutine converts every GRIB1 field in a file to a GRIB2 field.
!> U and V wind component fields are combined into a single GRIB2
!> message.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-11 | Gilbert | Initial
!> 2003-05-19 | Gilbert | Changed Master Table Version Number from 1 to 2. Added check for grib1 table version with params 191 and 192 for ensemble probs.
!> 2007-03-26 | Gordon | Added check for ECMWF data to reference ECMWF Conversion tables.
!> 2007-10-11 | Vuong | Added check for ensemble probs if the kpds > 28
!> 2008-01-28 | Vuong | Fixed the V-GRD BY SETTING THE LPDS(22) = -1 and increase the array size MAXPTS
!> 2008-05-14 | Vuong | Add option -m0 No explicit missing values included within data values
!> 2010-12-02 | Vuong | Changed Master Table Version Number from 2 to 6. - Add option -mastertable_ver_x where x is mater table version 2 to 10
!> 2011-07-22 | Vuong | Changed variable kprob(1) to kpds(5) in calling routine param_g1_to_g2
!> 2012-03-21 | Vuong | Set the Shape of Earth to 2 (oblate spheroid earth) for IMSSNOW (Polar Stereo graphic) Grid.
!>
!> @param[in] ifl1 Fortran unit number of input GRIB1 file.
!> @param[in] ifl2 Fortran unit number of output GRIB2 file.
!> @param[in] ipack GRIB2 packing option:
!> value | option
!> ------|-------
!> 0     | simple packing
!> 2     | group packing
!> 31    | group pack with 1st order differencing
!> 32    | group pack with 2nd order differencing
!> 40    | JPEG2000 encoding
!> 40000 | JPEG2000 encoding (obsolete)
!> 41    | PNG encoding
!> 40010 | PNG encoding (obsolete)
!> If ipack .ne. one of the values above, 31 is used as a default.
!> @param[in] usemiss uses missing value management (instead of
!> bitmaps), for use ipack options 2, 31, and 32.
!> @param[in] imiss Missing value management:
!> - 0 No explicit missing values included within data values
!> - 1 Primary missing values included within data values
!> @param[in] uvvect U/V vector control:
!> - .true. combine U and V wind components into one GRIB2 msg.
!> - .flase. does not combine U and V wind components
!> @param table_ver Master Table version where x is number from 2 to 10.
!>
!> @author Stephen Gilbert @date 2003-06-11
subroutine cnv12(ifl1, ifl2, ipack, usemiss, imiss, uvvect, table_ver)

  use params
  use params_ecmwf
  integer, intent(in) :: ifl1, ifl2, ipack
  logical, intent(in) :: usemiss, uvvect

  PARAMETER (MAXPTS = 40000000, msk1 = 32000)
  CHARACTER(len = 1), allocatable, dimension(:) :: cgrib, cgribin
  integer KPDS(200), KGDS(200), KPTR(200)
  integer LPDS(200), LGDS(200), KENS(200), LENS(200)
  integer KPROB(2), KCLUST(16), KMEMBR(80)
  real XPROB(2)
  real, allocatable, dimension(:) :: FLD
  real, allocatable, dimension(:) :: FLDV
  real, allocatable, dimension(:) :: coordlist
  integer :: listsec0(2) = (/0, 2/), imiss
  integer :: listsec1(13) = (/7, 0, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/)
  integer :: ideflist(MAXPTS), idefnum
  integer :: igds(5) = (/0, 0, 0, 0, 0/), igdstmpl(200), ipdstmpl(200)
  integer :: ipdstmplv(200)
  integer :: idrstmpl(200), idrstmplv(200)
  integer :: currlen = 0, table_ver
  integer, parameter :: mingrib = 500
  logical :: ensemble, ecmwf
  Logical*1, allocatable, dimension(:) :: bmp, bmpv
  !
  ICND = 0
  IFLI1 = 0
  allocate(fld(maxpts))
  allocate(coordlist(maxpts))
  allocate(bmp(maxpts))
  listsec1(3) = table_ver
  !
  iseek = 0
  do
     call skgb(ifl1, iseek, msk1, lskip, lgrib)
     if (lgrib .eq. 0) exit                ! end loop at EOF or problem
     if (lgrib.gt.currlen) then
        if (allocated(cgribin)) deallocate(cgribin)
        allocate(cgribin(lgrib), stat = is)
        currlen = lgrib
        lcgrib = lgrib*2
        if (lcgrib .lt. mingrib) lcgrib = mingrib
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(lcgrib), stat = is)
     endif
     call baread(ifl1, lskip, lgrib, lengrib, cgribin)
     if (lgrib .eq. lengrib) then
        call w3fi63(cgribin, KPDS, KGDS, BMP, FLD, KPTR, IRET)
        numpts = KPTR(10)
        if (iret .ne. 0) then
           print *, ' cnvgrib: Error unpacking GRIB field.', iret
           iseek = lskip+lgrib
           cycle
        endif
     else
        print *, ' cnvgrib: IO Error on input GRIB file.'
        cycle
     endif
     iseek = lskip+lgrib
     !print *, 'kpds:', kpds(1:28)
     !print *, 'kpds:', kpds(1:45)
     if ((kpds(5) .eq. 34).AND.uvvect) cycle ! V-comp already processed with U
     listsec1(1) = kpds(1)
     listsec1(2) = kpds(23)
     listsec1(5) = 1
     if (kpds(16) .eq. 1) listsec1(5) = 0
     listsec1(6) = ((kpds(21)-1)*100)+kpds(8)
     listsec1(7) = kpds(9)
     listsec1(8) = kpds(10)
     listsec1(9) = kpds(11)
     listsec1(10) = kpds(12)
     listsec1(13) = 1
     if (kpds(16) .eq. 1) listsec1(13) = 0
     ensemble = .false.
     if ((kpds(23) .eq. 2) .or. &
          (kptr(3).gt.28 .and. kpds(19) .eq. 2 .and. &
          (kpds(5) .eq. 191 .or. kpds(5) .eq. 192))) then ! ensemble forecast
        ensemble = .true.
     endif
     if (ensemble) then    ! ensemble forecast
        call g2_gbytec(cgribin(9), ilast, 0, 24)
        call pdseup(kens, kprob, xprob, kclust, kmembr, ilast, cgribin(9))
        if (kens(2) .eq. 1) listsec1(13) = 3
        if (kens(2) .eq. 2 .OR. kens(2) .eq. 3) listsec1(13) = 4
        if (kens(2) .eq. 5) listsec1(13) = 5
     endif
     ecmwf = .false.
     if (kpds(1) .eq. 98) ecmwf = .true.
     if (ecmwf) then         ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5), kpds(19), listsec0(1), idum, &
             jdum)                ! set discipline
     else
        if (ensemble.and.(kpds(5) .eq. 191 .or. kpds(5) .eq. 192).and. &
             kpds(19) .eq. 2) then
           !kprob(1) = 61
           call param_g1_to_g2(kprob(1), kpds(19), listsec0(1), idum, &
                jdum)             ! set discipline
        else
           call param_g1_to_g2(kpds(5), kpds(19), listsec0(1), idum, &
                jdum)             ! set discipline
        endif
     endif
     call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
     if (ierr .ne. 0) then
        write(6, *) ' ERROR creating new GRIB2 field = ', ierr
        cycle
     endif
     !
     ! convert grid info
     call gds2gdt(kgds, igds, igdstmpl, idefnum, ideflist, ierr)
     if (ierr .ne. 0) then
        cycle
     endif
     if (listsec1(1)  .eq.  7) igdstmpl(1) = 6    ! FOR NWS/NCEP
     if ((listsec1(1)  .eq.  7 .and. igds(5) .eq. 20   & ! For Snow Cover Analysis
          .and. kpds(2) .eq. 25) .and. & ! Polar Stereographic Grid
          (kpds(5) .eq. 91  .or.  kpds(5) .eq. 238)) then
        igdstmpl(1) = 2
     end if
     call addgrid(cgrib, lcgrib, igds, igdstmpl, 200, ideflist, &
          idefnum, ierr)
     if (ierr .ne. 0) then
        write(6, *) ' ERROR adding GRIB2 grid = ', ierr
        cycle
     endif

     ! set PDS Template
     if (ensemble) then    ! ensemble forecast
        call pds2pdtens(kpds, kens, kprob, xprob, kclust, kmembr, &
             ipdsnum, ipdstmpl, numcoord, coordlist, ierr)
     else
        call pds2pdt(kpds, ipdsnum, ipdstmpl, numcoord, coordlist, ierr)
     endif
     if (ierr .ne. 0) then
        cycle
     endif

     ! set bitmap flag
     idrstmpl = 0
     if (btest(kpds(4), 6)) then
        ibmap = 0
        !fld = pack(fld, mask = bmp(1:numpts))
        !itemp = count(bmp(1:numpts))
        !numpts = itemp
        !
        !   convert bitmap to "missing" values, if requested.
        !
        if ((usemiss) .AND. (ipack .eq. 2  .OR.  ipack .eq. 31 .OR. &
             ipack .eq. 32)) then
           ibmap = 255
           rmiss = minval(fld(1:numpts))
           if (rmiss .lt. -9999.0) then
              rmiss = rmiss*10.0
           else
              rmiss = -9999.0
           endif
           do i = 1, numpts
              if (.NOT. bmp(i)) then
                 fld(i) = rmiss
                 bmp(i) = .true.
              endif
           enddo
           idrstmpl(7) = imiss                   ! Missing value management
           call mkieee(rmiss, idrstmpl(8), 1)
        endif
     else
        ibmap = 255
        idrstmpl(7) = 0                   ! No missing values
     endif

     !   Set DRT info  (packing info)
     if (ipack .eq. 0) then
        idrsnum = 0
     elseif (ipack .eq. 2) then
        idrsnum = 2
        idrstmpl(6) = 1                   ! general group split
     elseif (ipack .eq. 31 .OR. ipack .eq. 32) then
        idrsnum = ipack/10
        idrstmpl(6) = 1                   ! general group split
        idrstmpl(17) = mod(ipack, 10)      ! order of s.d.
     elseif (ipack .eq. 40  .OR.  ipack .eq. 41 .OR. &
          ipack .eq. 40000  .OR.  ipack .eq. 40010) then
        idrsnum = ipack
        idrstmpl(6) = 0
        idrstmpl(7) = 255
        !idrstmpl(6) = 1
        !idrstmpl(7) = 15
     else
        idrsnum = 3
        idrstmpl(17) = 1                  ! order of s.d.
        idrstmpl(6) = 1                   ! general group split
        if (kpds(5) .eq. 61) idrsnum = 2
     endif
     idrstmpl(2) = KPTR(19)       ! binary scale
     idrstmpl(3) = kpds(22)       ! decimal scale
     !idrstmpl(2) = -4       ! binary scale
     !idrstmpl(3) = 0       ! decimal scale
     call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, 200, &
          coordlist, numcoord, idrsnum, idrstmpl, 200, &
          fld, numpts, ibmap, bmp, ierr)
     !       print *, 'done with addfield'
     if (ierr .ne. 0) then
        write(6, *) ' ERROR adding GRIB2 field  =  ', ierr
        cycle
     endif

     if ((kpds(5) .eq. 33) .AND. uvvect) then
        if (.not.allocated(fldv)) allocate(fldv(maxpts))
        if (.not.allocated(bmpv)) allocate(bmpv(maxpts))
        LGDS = KGDS
        LENS = KENS
        LPDS = KPDS
        LPDS(22) = -1
        LPDS(5) = 34
        jsrch = 0
        CALL GETGBE(IFL1, IFLI1, MAXPTS, jsrch, LPDS, LGDS, LENS, NUMPTSO, &
             jsrch, KPDS, KGDS, KENS, BMPV, FLDV, ICND)
        if (icnd .ne. 0) then
           write(6, *) ' ERROR READING/UNPACKING GRIB1 V = ', icnd
           exit
        endif
        ipdstmplv = ipdstmpl
        if (ecmwf) then       ! treat ecmwf data conversion seperately
           !            print *, ' param_ecmwf call 2'
           call param_ecmwf_g1_to_g2(kpds(5), kpds(19), idum, &
                ipdstmplv(1), ipdstmplv(2))
           !            print *, ' done with call 2'
        else
           call param_g1_to_g2(kpds(5), kpds(19), idum, ipdstmplv(1), &
                ipdstmplv(2))
        endif
        ! set bitmap flag
        idrstmplv = 0
        if (btest(kpds(4), 6)) then
           !fldv = pack(fldv, mask = bmpv(1:numpts))
           if (ANY(bmp(1:igds(2)) .NEQV. bmpv(1:igds(2)))) then
              !print *, 'SAGT: BITMAP different'
              ibmap = 0
              !   convert bitmap to "missing" values, if requested.
              if ((usemiss) .AND. (ipack .eq. 2  .OR.  ipack .eq. 31 .OR. &
                   ipack .eq. 32)) then
                 ibmap = 255
                 rmiss = minval(fldv(1:numpts))
                 if (rmiss .lt. -9999.0) then
                    rmiss = rmiss*10.0
                 else
                    rmiss = -9999.0
                 endif
                 do i = 1, numpts
                    if (.NOT. bmpv(i)) then
                       fldv(i) = rmiss
                       bmpv(i) = .true.
                    endif
                 enddo
                 idrstmplv(7) = imiss                   ! Missing values management
                 call mkieee(rmiss, idrstmplv(8), 1)
              endif
           else
              !print *, 'SAGT: BITMAP SAME'
              ibmap = 254
           endif
        else
           ibmap = 255
           idrstmplv(7) = 0                   ! No missing values
        endif
        !   Set DRT info  (packing info)
        if (ipack .eq. 0) then
           idrsnum = 0
        elseif (ipack .eq. 2) then
           idrsnum = 2
           idrstmplv(6) = 1                   ! general group split
        elseif (ipack .eq. 31 .OR. ipack .eq. 32) then
           idrsnum = ipack/10
           idrstmplv(6) = 1                   ! general group split
           idrstmplv(17) = mod(ipack, 10)      ! order of s.d.
        elseif (ipack .eq. 40  .OR.  ipack .eq. 41 .OR. &
             ipack .eq. 40000 .OR. ipack .eq. 40010) then
           idrsnum = ipack
           idrstmplv(6) = 0
           idrstmplv(7) = 255
           !idrstmplv(6) = 1
           !idrstmplv(7) = 15
        else
           idrsnum = 3
           idrstmplv(17) = 1                  ! order of s.d.
           idrstmplv(6) = 1                   ! general group split
           if (kpds(5) .eq. 61) idrsnum = 2
        endif
        idrstmplv(2) = KPTR(19)       ! binary scale
        idrstmplv(3) = kpds(22)       ! decimal scale
        !idrstmplv(2) = -4       ! binary scale
        !idrstmplv(3) = 0       ! decimal scale
        call addfield(cgrib, lcgrib, ipdsnum, ipdstmplv, 200, &
             coordlist, numcoord, idrsnum, idrstmplv, 200, &
             fldv, numpts, ibmap, bmpv, ierr)
        if (ierr .ne. 0) then
           write(6, *) ' ERROR adding second GRIB2 field  =  ', ierr
           cycle
        endif
     endif
     ! End GRIB2 field
     call gribend(cgrib, lcgrib, lengrib, ierr)
     if (ierr .ne. 0) then
        write(6, *) ' ERROR ending new GRIB2 message = ', ierr
        cycle
     endif
     !        print *, ' writing ', lengrib, ' bytes...'
     call wryte(ifl2, lengrib, cgrib)

  enddo

  if (allocated(cgribin)) deallocate(cgribin)
  if (allocated(cgrib)) deallocate(cgrib)
  if (allocated(fld)) deallocate(fld)
  if (allocated(fldv)) deallocate(fldv)
  if (allocated(coordlist)) deallocate(coordlist)
  if (allocated(bmp)) deallocate(bmp)
  if (allocated(bmpv)) deallocate(bmpv)

  return
end subroutine cnv12

!> This routine converts a GRIB1 GDS (in format specfied in
!> w3fi63.f) to necessary info for a GRIB2 Grid Definition Section.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-17 | Gilbert | Initial.
!> 2004-04-27 | Gilbert | Added support for Gaussian grids.
!> 2007-04-16 | Vuong | Added Curvilinear Orthogonal grids.
!> 2007-05-29 | Vuong | Added Rotate Lat/Lon E-grid (203)
!> 2010-05-10 | Vuong | Added Rotate Lat/Lon for Non-E Stagger grid (205)
!> 2011-05-04 | Vuong | Corrected Arakawa Lat/Lon of grid points for Non-E Stagger grid (205)
!>
!> @param[in] kgds GRIB1 GDS info as returned by w3fi63.f.
!> @param[out] igds Contains information read from the appropriate GRIB
!> Grid Definition Section 3 for the field being returned. Must be
!> dimensioned >= 5.
!> - igds(1) Source of grid definition (see Code Table 3.0)
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!>   definition. Used to define number of points in each row (or column)
!>   for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points definition. (Code Table 3.11)
!> - igds(5) Grid Definition Template Number (Code Table 3.1)
!> @param[out] igdstmpl Grid Definition Template values for GDT 3.igds(5)
!> @param[out] idefnum The number of entries in array
!> ideflist. i.e. number of rows (or columns) for which optional grid
!> points are defined.
!> @param[out] ideflist Optional integer array containing the number of
!> grid points contained in each row (or column).
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 1 Unrecognized GRIB1 grid data representation type
!>
!> @author Stephen Gilbert @date 2003-06-17
subroutine gds2gdt(kgds,igds,igdstmpl,idefnum,ideflist,iret)

  integer,intent(in) :: kgds(*)
  integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
  integer,intent(out) :: idefnum,iret

  iret=0
  if (kgds(1).eq.0) then       !  Lat/Lon grid
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=0                 ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)      !Ni
     igdstmpl(9)=kgds(3)      !Nj
     igdstmpl(10)=0
     igdstmpl(11)=0
     igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
     if (kgds(5).lt.0) then       ! Lon of 1st grid point
        igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(13)=kgds(5)*1000
     endif
     igdstmpl(14)=0                 ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(14)=48
     if (btest(kgds(6),3)) igdstmpl(14)=igdstmpl(14)+8
     igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
     if (kgds(8).lt.0) then       ! Lon of last grid point
        igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
     else
        igdstmpl(16)=kgds(8)*1000
     endif
     igdstmpl(17)=kgds(9)*1000      ! Di
     igdstmpl(18)=kgds(10)*1000     ! Dj
     igdstmpl(19)=kgds(11)          ! Scanning mode
     if (kgds(20).ne.255) then         !  irregular grid (eg WAFS)
        igds(2)=kgds(21)              ! num of grid points
        !idefnum=kgds(19)
        if (kgds(2).eq.65535) idefnum=kgds(3)
        if (kgds(3).eq.65535) idefnum=kgds(2)
        imax=0
        do j=1,idefnum
           ideflist(j)=kgds(21+j)
           if (ideflist(j).gt.imax) imax=ideflist(j)
        enddo
        igds(3)=1                 ! octets for further grid definition
        if (imax.gt.255) igds(3)=2
        if (imax.gt.65535) igds(3)=3
        if (imax.gt.16777215) igds(3)=4
        igds(4)=1                 ! interpretation of optional list
        igdstmpl(8)=-1
        igdstmpl(17)=-1
     endif
  elseif (kgds(1).eq.1) then       !  Mercator grid
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=10                 ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)              ! Ni
     igdstmpl(9)=kgds(3)              ! Nj
     igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
     if (kgds(5).lt.0) then         ! Lon of 1st grid point
        igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(11)=kgds(5)*1000
     endif
     igdstmpl(12)=0                   ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(12)=48
     if (btest(kgds(6),3)) igdstmpl(12)=igdstmpl(12)+8
     igdstmpl(13)=kgds(9)*1000        ! Lat intersects earth
     igdstmpl(14)=kgds(7)*1000        ! Lat of last grid point
     if (kgds(8).lt.0) then         ! Lon of last grid point
        igdstmpl(15)=(360000+kgds(8))*1000    ! convert W to E
     else
        igdstmpl(15)=kgds(8)*1000
     endif
     igdstmpl(16)=kgds(11)            ! Scanning mode
     igdstmpl(17)=0                   ! Orientation of grid
     igdstmpl(18)=kgds(12)*1000       ! Di
     igdstmpl(19)=kgds(13)*1000       ! Dj
  elseif (kgds(1).eq.3) then       ! Lambert Conformal Grid
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=30                 ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)              ! Nx
     igdstmpl(9)=kgds(3)              ! Ny
     igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
     if (kgds(5).lt.0) then         ! Lon of 1st grid point
        igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(11)=kgds(5)*1000
     endif
     igdstmpl(12)=0                   ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(12)=48
     if (btest(kgds(6),3)) igdstmpl(12)=igdstmpl(12)+8
     igdstmpl(13)=kgds(12)*1000       ! Lat where Dx and Dy specified
     if (kgds(7).lt.0) then         ! Lon of orientation
        igdstmpl(14)=(360000+kgds(7))*1000    ! convert W to E
     else
        igdstmpl(14)=kgds(7)*1000
     endif
     igdstmpl(15)=kgds(8)*1000        ! Dx
     igdstmpl(16)=kgds(9)*1000        ! Dy
     igdstmpl(17)=kgds(10)            ! Projection Center Flag
     igdstmpl(18)=kgds(11)            ! Scanning mode
     igdstmpl(19)=kgds(12)*1000       ! Latin 1
     igdstmpl(20)=kgds(13)*1000       ! Latin 2
     igdstmpl(21)=kgds(14)*1000       ! Lat of S. Pole of projection
     if (kgds(15).lt.0) then        ! Lon of S. Pole of projection
        igdstmpl(22)=(360000+kgds(15))*1000    ! convert W to E
     else
        igdstmpl(22)=kgds(15)*1000
     endif
  elseif (kgds(1).eq.4) then       !  Gaussian Lat/Lon grid
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=40                ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)      !Ni
     igdstmpl(9)=kgds(3)      !Nj
     igdstmpl(10)=0
     igdstmpl(11)=0
     igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
     if (kgds(5).lt.0) then       ! Lon of 1st grid point
        igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(13)=kgds(5)*1000
     endif
     igdstmpl(14)=0                 ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(14)=48
     if (btest(kgds(6),3)) igdstmpl(14)=igdstmpl(14)+8
     igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
     if (kgds(8).lt.0) then       ! Lon of last grid point
        igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
     else
        igdstmpl(16)=kgds(8)*1000
     endif
     igdstmpl(17)=kgds(9)*1000      ! Di
     igdstmpl(18)=kgds(10)          ! D - Number of parallels
     igdstmpl(19)=kgds(11)          ! Scanning mode
  elseif (kgds(1).eq.5) then       ! Polar Stereographic Grid
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=20                 ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)              ! Nx
     igdstmpl(9)=kgds(3)              ! Ny
     igdstmpl(10)=kgds(4)*1000        ! Lat of 1st grid point
     if (kgds(5).lt.0) then         ! Lon of 1st grid point
        igdstmpl(11)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(11)=kgds(5)*1000
     endif
     igdstmpl(12)=0                   ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(12)=48
     if (btest(kgds(6),3)) igdstmpl(12)=igdstmpl(12)+8
     igdstmpl(13)=60000000            ! Lat where Dx and Dy specified
     if (btest(kgds(10),7)) igdstmpl(13)=-60000000
     if (kgds(7).lt.0) then         ! Lon of orientation
        igdstmpl(14)=(360000+kgds(7))*1000    ! convert W to E
     else
        igdstmpl(14)=kgds(7)*1000
     endif
     igdstmpl(15)=kgds(8)*1000        ! Dx
     igdstmpl(16)=kgds(9)*1000        ! Dy
     igdstmpl(17)=kgds(10)            ! Projection Center Flag
     igdstmpl(18)=kgds(11)            ! Scanning mode
  elseif (kgds(1).eq.204) then       ! Curivilinear Orthogonal Grid (Used by RTOFS)
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=204               ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)      !Ni - No of points along x-grid direction
     igdstmpl(9)=kgds(3)      !Nj - No of points along y-grid direction
     igdstmpl(10)=0
     igdstmpl(11)=0
     igdstmpl(12)=0
     igdstmpl(13)=0
     igdstmpl(14)=0                 ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(14)=48
     if (btest(kgds(6),3)) igdstmpl(14)=igdstmpl(14)+8
     igdstmpl(15)=0
     igdstmpl(16)=0
     igdstmpl(17)=0
     igdstmpl(18)=0
     igdstmpl(19)=kgds(11)          ! Scanning mode
  elseif (kgds(1).eq.203) then    !  Rot Lat/Lon grid (Arakawa)
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=32768             ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)      !Ni
     igdstmpl(9)=kgds(3)      !Nj
     igdstmpl(10)=0
     igdstmpl(11)=0
     igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
     if (kgds(5).lt.0) then       ! Lon of 1st grid point
        igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(13)=kgds(5)*1000
     endif
     igdstmpl(14)=0                 ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(14)=48
     if (btest(kgds(6),3)) igdstmpl(14)=igdstmpl(14)+8
     igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
     if (kgds(8).lt.0) then       ! Lon of last grid point
        igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
     else
        igdstmpl(16)=kgds(8)*1000
     endif
     igdstmpl(17)=kgds(9)*1000      ! Di
     igdstmpl(18)=kgds(10)*1000     ! Dj
     igdstmpl(19)=kgds(11)          ! Scanning mode
  elseif (kgds(1).eq.205) then    !  Rot Lat/Lon for Non-E Stagger grid (Arakawa)
     idefnum=0
     igds(1)=0                 ! grid def specfied in template
     igds(2)=kgds(2)*kgds(3)   ! num of grid points
     igds(3)=0                 ! octets for further grid definition
     igds(4)=0                 ! interpretation of optional list
     igds(5)=32769             ! Grid Definition Template number
     if (btest(kgds(6),6)) then     ! shape of Earth
        igdstmpl(1)=2
     else
        igdstmpl(1)=0
     endif
     igdstmpl(2)=0
     igdstmpl(3)=0
     igdstmpl(4)=0
     igdstmpl(5)=0
     igdstmpl(6)=0
     igdstmpl(7)=0
     igdstmpl(8)=kgds(2)      !Ni
     igdstmpl(9)=kgds(3)      !Nj
     igdstmpl(10)=0
     igdstmpl(11)=0
     igdstmpl(12)=kgds(4)*1000      ! Lat of 1st grid point
     if (kgds(5).lt.0) then       ! Lon of 1st grid point
        igdstmpl(13)=(360000+kgds(5))*1000    ! convert W to E
     else
        igdstmpl(13)=kgds(5)*1000
     endif
     igdstmpl(14)=0                 ! Resolution and Component flags
     if (btest(kgds(6),7)) igdstmpl(14)=48
     if (btest(kgds(6),3)) igdstmpl(14)=igdstmpl(14)+8
     igdstmpl(15)=kgds(7)*1000      ! Lat of last grid point
     if (kgds(8).lt.0) then       ! Lon of last grid point
        igdstmpl(16)=(360000+kgds(8))*1000    ! convert W to E
     else
        igdstmpl(16)=kgds(8)*1000
     endif
     igdstmpl(17)=kgds(9)*1000      ! Di
     igdstmpl(18)=kgds(10)*1000     ! Dj
     igdstmpl(19)=kgds(11)          ! Scanning mode
     igdstmpl(20)=kgds(12)*1000
     igdstmpl(21)=kgds(13)*1000
  else
     Print *,'gds2gdt: Unrecognized GRIB1 Grid type = ',kgds(1)
     iret=1
  endif
  return
end subroutine gds2gdt

!> This routine converts a GRIB1 PDS (Section 1) info to a GRIB2 PDS
!> (Section 4) info with appropriate Product Definition Template.
!>
!> @note Use pds2pdtens for ensemble related PDS.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial.
!> 2005-04-19 | Gilbert | Changed scaling factor used with potential vorticity surfaces.
!> 2007-02-07 | Gilbert | fixed end date calculation
!> 2007-03-26 | Gordon | Added check for ECMWF data to reference ECMWF Conversion tables.
!> 2007-05-14 | Boi Vuong | Added Time Range Indicator 51 (Climatological Mean Value)
!> 2009-05-20 | Boi Vuong | Added check for WAFS to use PDT 4.8 for Max Wind
!> 2009-12-14 | Boi Vuong | Added check for WAFS to use PDT 4.15 for Icing, Turbulence and Cumulonimbus
!> 2010-02-18 | Boi Vuong | Added Time Range Indicator 7
!> 2010-08-10 | Boi Vuong | Removed check for WAFS to use PDT 4.8 for Max Wind
!> 2011-10-24 | Boi Vuong | Added check for parameters (MAXUW, MAXVW, to set type of statistical processing (MIN and MAX)
!>
!> @param[in] kpds GRIB1 PDS info as specified in W3FI63.
!> @param[out] ipdsnum GRIB2 Product Definition Template Number
!> @param[out] ipdstmpl GRIB2 Product Definition Template entries for PDT 4.ipdsnum
!> @param[out] numcoord number of vertical discretisation values (not implemented)
!> @param[out] coordlist rtical discretisation values (not implemented)
!> @param[out] iret Error return value:
!> - 0  Successful
!> - 1  Unrecognized GRIB1 Time Range Indicator
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine pds2pdt(kpds,ipdsnum,ipdstmpl,numcoord,coordlist, &
    iret)

  use params
  use params_ecmwf

  integer,intent(in) :: kpds(*)
  integer,intent(out) :: ipdstmpl(*)
  real,intent(out) :: coordlist(*)
  integer,intent(out) :: ipdsnum,numcoord,iret

  integer :: idat(8),jdat(8)
  real :: rinc(5)
  logical :: ecmwf

  iret=0
  numcoord=0
  ecmwf=.false.

  if (kpds(1).eq.98) ecmwf=.true.
  !
  !  Special check for WAFS products for parameters (Max Icing, TP and CAT)
  !  to PDT 4.15
  !
  if ((kpds(2).eq.96 .AND. kpds(3).eq.45 .AND. &
      kpds(16).eq.10) .AND. &
      (kpds(19).eq.140 .AND. (kpds(5).ge.168 .AND. &
      kpds(5).le.173))) then
    ipdsnum=15
    !  get GRIB2 parameter category and number from GRIB1
    !  parameter number
    if (ecmwf) then       ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum, &
            ipdstmpl(1),ipdstmpl(2))
    else
        call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
    endif
    ipdstmpl(3)=2
    ipdstmpl(4)=0
    ipdstmpl(5)=kpds(2)
    ipdstmpl(6)=0
    ipdstmpl(7)=0
    ipdstmpl(8)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(8)=13
    ipdstmpl(9)=kpds(14)
    call cnvlevel(kpds(6),kpds(7),ipdstmpl)
    if (kpds(5).eq.168.or.kpds(5).eq.170.or.  & ! statistical process WAFS-ICAO
          kpds(5).eq.172) ipdstmpl(16)=0           ! for Mean Icing, CT, CAT
    if (kpds(5).eq.169.or.kpds(5).eq.171.or.  & ! statistical process WAFS-ICAO
          kpds(5).eq.173) ipdstmpl(16)=2           ! for MAX Icing, CT, CAT
    ipdstmpl(17)=3                             ! Neighbor interpolation
    ! Output values is set to nearest input values
    ipdstmpl(18)=1                             ! Number of data point (grid 45)
  elseif (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
    ipdsnum=0
    !  get GRIB2 parameter category and number from GRIB1
    !  parameter number
    if (ecmwf) then       ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum, &
            ipdstmpl(1),ipdstmpl(2))
    else
        call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
    endif
    if (kpds(16).eq.1) then
        ipdstmpl(3)=0
    else
        ipdstmpl(3)=2
    endif
    ipdstmpl(4)=0
    ipdstmpl(5)=kpds(2)
    ipdstmpl(6)=0
    ipdstmpl(7)=0
    ipdstmpl(8)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(8)=13
    !if (kpds(16).eq.10) then
    !  ipdstmpl(9)=(kpds(14)*256)+kpds(15)
    !else
    ipdstmpl(9)=kpds(14)
    !endif
    call cnvlevel(kpds(6),kpds(7),ipdstmpl)
    if (kpds(2).eq.96 .AND. kpds(3).eq.45 .AND. &
          kpds(16).eq.10) then
        if (kpds(5).eq.174) ipdstmpl(10) = 10
        if (kpds(5).eq.179) ipdstmpl(10) = 11
        if (kpds(5).eq.180) ipdstmpl(10) = 12
    end if
  elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
    ipdsnum=8
    !  get GRIB2 parameter category and number from GRIB1
    !  parameter number
    if (ecmwf) then       ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum, &
            ipdstmpl(1),ipdstmpl(2))
    else
        call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
    endif
    ipdstmpl(3)=2
    ipdstmpl(4)=0
    ipdstmpl(5)=kpds(2)
    ipdstmpl(6)=0
    ipdstmpl(7)=0
    ipdstmpl(8)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(8)=13
    ipdstmpl(9)=kpds(14)
    call cnvlevel(kpds(6),kpds(7),ipdstmpl)
    !  calculate ending time using initial ref-time, idat,
    !  and increment rinc.
    idat=0
    idat(1)=((kpds(21)-1)*100)+kpds(8)
    idat(2)=kpds(9)
    idat(3)=kpds(10)
    idat(4)=-500     ! EST
    idat(5)=kpds(11)
    idat(6)=kpds(12)
    rinc=0.0
    if ( ipdstmpl(8).eq.0 ) then
        rinc(3)=kpds(15)
    elseif ( ipdstmpl(8).eq.1 ) then
        rinc(2)=kpds(15)
    elseif ( ipdstmpl(8).eq.2 ) then
        rinc(1)=kpds(15)
    elseif ( ipdstmpl(8).eq.10 ) then
        rinc(2)=kpds(15) * 3
    elseif ( ipdstmpl(8).eq.11 ) then
        rinc(2)=kpds(15) * 6
    elseif ( ipdstmpl(8).eq.12 ) then
        rinc(2)=kpds(15) * 12
    elseif ( ipdstmpl(8).eq.13 ) then
        rinc(4)=kpds(15)
    endif
    call w3movdat(rinc,idat,jdat)     ! calculate end date/time
    ipdstmpl(16)=jdat(1)                       ! year of end time
    ipdstmpl(17)=jdat(2)                       ! month of end time
    ipdstmpl(18)=jdat(3)                       ! day of end time
    ipdstmpl(19)=jdat(5)                       ! hour of end time
    ipdstmpl(20)=jdat(6)                       ! minute of end time
    ipdstmpl(21)=jdat(7)                       ! second of end time
    ipdstmpl(22)=1                             ! # of time ranges
    ipdstmpl(23)=kpds(20)                      ! # of values missing
    if (kpds(16).eq.2) then                    ! statistical process
        ipdstmpl(24)=255
    elseif (kpds(16).eq.3) then
        ipdstmpl(24)=0
    elseif (kpds(16).eq.4) then
        ipdstmpl(24)=1
    elseif (kpds(16).eq.5) then
        ipdstmpl(24)=4
    endif
    ipdstmpl(25)=2
    if (kpds(19).eq.129 .AND. &
          (kpds(5).eq.235 .or. kpds(5).eq.236 .or. &
          kpds(5).eq.237 .or. kpds(5).eq.238 .or. &
          kpds(5).eq.239 .or. kpds(5).eq.253 .or. &
          kpds(5).eq.254 )) then
        ipdstmpl(24)=2
    endif
    ipdstmpl(26)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(26)=13
    ipdstmpl(27)=kpds(15)-kpds(14)
    ipdstmpl(28)=255
    ipdstmpl(29)=0
  elseif (kpds(16).eq.7) then
    ipdsnum=8
    !  get GRIB2 parameter category and number from GRIB1
    !  parameter number
    if (ecmwf) then       ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum, &
            ipdstmpl(1),ipdstmpl(2))
    else
        call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
    endif
    ipdstmpl(3)=2
    ipdstmpl(4)=0
    ipdstmpl(5)=kpds(2)
    ipdstmpl(6)=0
    ipdstmpl(7)=0
    ipdstmpl(8)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(8)=13
    ipdstmpl(9)= - kpds(14)
    call cnvlevel(kpds(6),kpds(7),ipdstmpl)
    !  calculate ending time using initial ref-time, idat,
    !  and increment rinc.
    idat=0
    idat(1)=((kpds(21)-1)*100)+kpds(8)
    idat(2)=kpds(9)
    idat(3)=kpds(10)
    idat(4)=-500     ! EST
    idat(5)=kpds(11)
    idat(6)=kpds(12)
    rinc=0.0
    if ( ipdstmpl(8).eq.0 ) then
        rinc(3)=kpds(15)
    elseif ( ipdstmpl(8).eq.1 ) then
        rinc(2)=kpds(15)
    elseif ( ipdstmpl(8).eq.2 ) then
        rinc(1)=kpds(15)
    elseif ( ipdstmpl(8).eq.10 ) then
        rinc(2)=kpds(15) * 3
    elseif ( ipdstmpl(8).eq.11 ) then
        rinc(2)=kpds(15) * 6
    elseif ( ipdstmpl(8).eq.12 ) then
        rinc(2)=kpds(15) * 12
    elseif ( ipdstmpl(8).eq.13 ) then
        rinc(4)=kpds(15)
    endif
    call w3movdat(rinc,idat,jdat)     ! calculate end date/time
    ipdstmpl(16)=jdat(1)                       ! year of end time
    ipdstmpl(17)=jdat(2)                       ! month of end time
    ipdstmpl(18)=jdat(3)                       ! day of end time
    ipdstmpl(19)=jdat(5)                       ! hour of end time
    ipdstmpl(20)=jdat(6)                       ! minute of end time
    ipdstmpl(21)=jdat(7)                       ! second of end time
    ipdstmpl(22)=1                             ! # of time ranges
    ipdstmpl(23)=kpds(20)                      ! # of values missing
    ipdstmpl(24)=0
    ipdstmpl(25)=2
    ipdstmpl(26)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(26)=13
    ipdstmpl(27)=kpds(15) + kpds(14)
    ipdstmpl(28)=255
    ipdstmpl(29)=0
  elseif (kpds(16).eq.51) then
    ipdsnum=8
    !  get GRIB2 parameter category and number from GRIB1
    !  parameter number
    if (ecmwf) then       ! treat ecmwf data conversion seperately
        call param_ecmwf_g1_to_g2(kpds(5),kpds(19),idum, &
            ipdstmpl(1),ipdstmpl(2))
    else
        call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
    endif
    ipdstmpl(3)=2
    ipdstmpl(4)=0
    ipdstmpl(5)=kpds(2)
    ipdstmpl(6)=0
    ipdstmpl(7)=0
    ipdstmpl(8)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(8)=13
    ipdstmpl(9)=kpds(14)
    call cnvlevel(kpds(6),kpds(7),ipdstmpl)
    !  calculate ending time using initial ref-time, idat,
    !  and increment rinc.
    idat=0
    idat(1)=((kpds(21)-1)*100)+kpds(8)
    idat(2)=kpds(9)
    idat(3)=kpds(10)
    idat(4)=-500     ! EST
    idat(5)=kpds(11)
    idat(6)=kpds(12)
    rinc=0.0
    if ( ipdstmpl(8).eq.0 ) then
        rinc(3)=kpds(15)
    elseif ( ipdstmpl(8).eq.1 ) then
        rinc(2)=kpds(15)
    elseif ( ipdstmpl(8).eq.2 ) then
        rinc(1)=kpds(15)
    elseif ( ipdstmpl(8).eq.10 ) then
        rinc(2)=kpds(15) * 3
    elseif ( ipdstmpl(8).eq.11 ) then
        rinc(2)=kpds(15) * 6
    elseif ( ipdstmpl(8).eq.12 ) then
        rinc(2)=kpds(15) * 12
    elseif ( ipdstmpl(8).eq.13 ) then
        rinc(4)=kpds(15)
    endif
    call w3movdat(rinc,idat,jdat)     ! calculate end date/time
    ipdstmpl(16)=jdat(1)                       ! year of end time
    ipdstmpl(17)=jdat(2)                       ! month of end time
    ipdstmpl(18)=jdat(3)                       ! day of end time
    ipdstmpl(19)=jdat(5)                       ! hour of end time
    ipdstmpl(20)=jdat(6)                       ! minute of end time
    ipdstmpl(21)=jdat(7)                       ! second of end time
    ipdstmpl(22)=1                             ! # of time ranges
    ipdstmpl(23)=kpds(20)                      ! # of values missing
    ipdstmpl(24)=51                            ! Climatological Mean
    ipdstmpl(25)=2
    ipdstmpl(26)=kpds(13)
    if (kpds(13).eq.254) ipdstmpl(26)=13
    ipdstmpl(27)=kpds(15)-kpds(14)
    ipdstmpl(28)=255
    ipdstmpl(29)=0
  else
    Print *,' Unrecognized Time Range Indicator = ',kpds(16)
    Print *,'pds2pdt: Couldn:t construct  PDS Template '
    iret=1
  endif

  return
end subroutine pds2pdt

!> This routine converts a GRIB1 Level type and Level value to GRIB2
!> values and fills in the appropriate PDT values for the level/layer
!> information.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial.
!> 2011-01-13 | Boi Vuong | Added level/layer values from 235 to 239
!>
!> @param[in] ltype GRIB1 level type (PDS octet 10)
!> @param[in] lval GRIB1 level/layer value(s) (PDS octets 11 and 12)
!> @param[out] ipdstmpl GRIB2 Product Definition Template values.
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine cnvlevel(ltype,lval,ipdstmpl)

  integer,intent(in) :: ltype,lval
  integer,intent(inout) :: ipdstmpl(*)

  ipdstmpl(10)=ltype
  ipdstmpl(11)=0
  ipdstmpl(12)=0
  ipdstmpl(13)=255
  ipdstmpl(14)=0
  ipdstmpl(15)=0

  if (ltype.eq.100) then
     ipdstmpl(12)=lval*100
  elseif (ltype.eq.101) then
     ipdstmpl(10)=100
     ipdstmpl(12)=(lval/256)*1000
     ipdstmpl(13)=100
     ipdstmpl(15)=mod(lval,256)*1000
  elseif (ltype.eq.102) then
     ipdstmpl(10)=101
  elseif (ltype.eq.103) then
     ipdstmpl(10)=102
     ipdstmpl(12)=lval
  elseif (ltype.eq.104) then
     ipdstmpl(10)=102
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=102
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.eq.105) then
     ipdstmpl(10)=103
     ipdstmpl(12)=lval
  elseif (ltype.eq.106) then
     ipdstmpl(10)=103
     ipdstmpl(12)=(lval/256)*100
     ipdstmpl(13)=103
     ipdstmpl(15)=mod(lval,256)*100
  elseif (ltype.eq.107) then
     ipdstmpl(10)=104
     ipdstmpl(11)=4
     ipdstmpl(12)=lval
  elseif (ltype.eq.108) then
     ipdstmpl(10)=104
     ipdstmpl(11)=2
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=104
     ipdstmpl(14)=2
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.eq.109) then
     ipdstmpl(10)=105
     ipdstmpl(12)=lval
  elseif (ltype.eq.110) then
     ipdstmpl(10)=105
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=105
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.eq.111) then
     ipdstmpl(10)=106
     ipdstmpl(11)=2
     ipdstmpl(12)=lval
  elseif (ltype.eq.112) then
     ipdstmpl(10)=106
     ipdstmpl(11)=2
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=106
     ipdstmpl(14)=2
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.eq.113) then
     ipdstmpl(10)=107
     ipdstmpl(12)=lval
  elseif (ltype.eq.114) then
     ipdstmpl(10)=107
     ipdstmpl(12)=475+(lval/256)
     ipdstmpl(13)=107
     ipdstmpl(15)=475+mod(lval,256)
  elseif (ltype.eq.115) then
     ipdstmpl(10)=108
     ipdstmpl(12)=lval*100
  elseif (ltype.eq.116) then
     ipdstmpl(10)=108
     ipdstmpl(12)=(lval/256)*100
     ipdstmpl(13)=108
     ipdstmpl(15)=mod(lval,256)*100
  elseif (ltype.eq.117) then
     ipdstmpl(10)=109
     ipdstmpl(11)=9
     ipdstmpl(12)=lval
     if ( btest(lval,15) ) then
        ipdstmpl(12)=-1*mod(lval,32768)
     endif
  elseif (ltype.eq.119) then
     ipdstmpl(10)=111
     ipdstmpl(11)=4
     ipdstmpl(12)=lval
  elseif (ltype.eq.120) then
     ipdstmpl(10)=111
     ipdstmpl(11)=2
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=111
     ipdstmpl(14)=2
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.eq.121) then
     ipdstmpl(10)=100
     ipdstmpl(12)=(1100+(lval/256))*100
     ipdstmpl(13)=100
     ipdstmpl(15)=(1100+mod(lval,256))*100
  elseif (ltype.eq.125) then
     ipdstmpl(10)=103
     ipdstmpl(11)=2
     ipdstmpl(12)=lval
  elseif (ltype.eq.126) then
     ipdstmpl(10)=100
     ipdstmpl(12)=lval
  elseif (ltype.eq.128) then
     ipdstmpl(10)=104
     ipdstmpl(11)=3
     ipdstmpl(12)=1100+(lval/256)
     ipdstmpl(13)=104
     ipdstmpl(14)=3
     ipdstmpl(15)=1100+mod(lval,256)
  elseif (ltype.eq.141) then
     ipdstmpl(10)=100
     ipdstmpl(12)=(lval/256)*100
     ipdstmpl(13)=100
     ipdstmpl(15)=(1100+mod(lval,256))*100
  elseif (ltype.eq.160) then
     ipdstmpl(10)=160
     ipdstmpl(12)=lval
  elseif (ltype.gt.99.AND.ltype.lt.200) then
     print *,'cnvlevel: GRIB1 Level ',ltype,' not recognized.'
     ipdstmpl(10)=255
  elseif (ltype.eq.235) then
     ipdstmpl(10)=ltype
     ipdstmpl(12)=lval
  elseif (ltype.eq.236) then
     ipdstmpl(10)=ltype
     ipdstmpl(12)=lval/256
     ipdstmpl(13)=ltype
     ipdstmpl(15)=mod(lval,256)
  elseif (ltype.ge.237.AND.ltype.le.239) then
     ipdstmpl(10)=ltype
     ipdstmpl(12)=lval
  endif

  return
end subroutine cnvlevel

!> This routine converts a GRIB1 PDS (Section 1) that includes NCEP
!> ensemble PDS extensions to a GRIB2 PDS (Section 4) info with
!> appropriate Product Definition Template.
!>
!> @note Use routine pds2pdt for non ensemble related PDS.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial.
!> 2007-02-07 | Gilbert | fixed end date calculation
!> 2007-05-14 | Boi Vuong | Added Time Range Indicator 51 (Climatological Mean Value)
!>
!> @param[in] kpds GRIB1 PDS info as specified in W3FI63.
!> @param[in] kens Ensemble identification from PDS octets 41-45.
!> @param[in] kprob Ensemble probability info from PDS octets 46 & 47.
!> @param[in] xprob Ensemble probability info from PDS octets 48-55.
!> @param[in] kclust Ensemble cluster info from PDS octets 61-76.
!> @param[in] kmember- semble membership info from PDS octest 77-86.
!> @param[out] ipdsnum GRIB2 Product Definition Template Number.
!> @param[out] ipdstmpl GRIB2 Product Definition Template entries for PDT 4.ipdsnum.
!> @param[out] numcoord number of vertical discretisation values (not implemented).
!> @param[out] coordlist- rtical discretisation values (not implemented).
!> @param[out] iret Error return value:
!> - 0  = Successful
!> - 1  = Unrecognized GRIB1 Time Range Indicator for ensembles
!> - 2  = Unrecognized GRIB1 Ensemble type
!> - 10 = Unrecognized GRIB1 Time Range Indicator for probabilities
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine pds2pdtens(kpds,kens,kprob,xprob,kclust,kmember, &
    ipdsnum,ipdstmpl,numcoord,coordlist, &
    iret)

  use params

  integer,intent(in) :: kpds(*),kens(*),kprob(*),kclust(*)
  integer,intent(in) :: kmember(*)
  real,intent(in) :: xprob(*)
  integer,intent(out) :: ipdstmpl(*)
  real,intent(out) :: coordlist(*)
  integer,intent(out) :: ipdsnum,numcoord,iret

  integer :: idat(8),jdat(8)
  real :: rinc(5)

  iret=0
  numcoord=0
  if (kens(2).eq.1.or.kens(2).eq.2.or.kens(2).eq.3) then
    !     individual ensemble fcst...
    if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
      !   At specific point in time...
      ipdsnum=1
      !  get GRIB2 parameter category and number from GRIB1
      !  parameter number
      call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
      ipdstmpl(3)=4
      ipdstmpl(4)=0
      ipdstmpl(5)=kpds(2)
      ipdstmpl(6)=0
      ipdstmpl(7)=0
      ipdstmpl(8)=kpds(13)
      if (kpds(13).eq.254) ipdstmpl(8)=13
      !if (kpds(16).eq.10) then
      !  ipdstmpl(9)=(kpds(14)*256)+kpds(15)
      !else
      ipdstmpl(9)=kpds(14)
      !endif
      call cnvlevel(kpds(6),kpds(7),ipdstmpl)
      if (kens(2).eq.1) then
          !               if (kens(3).eq.1) ipdstmpl(16)=0
          !               if (kens(3).eq.2) ipdstmpl(16)=1
          ipdstmpl(16)=kens(3)-1
          ipdstmpl(17)=0
      elseif (kens(2).eq.2) then
          ipdstmpl(16)=2
          ipdstmpl(17)=kens(3)
      elseif (kens(2).eq.3) then
          ipdstmpl(16)=3
          ipdstmpl(17)=kens(3)
      endif
      ipdstmpl(18)=10
    elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
      !    over time range...
      ipdsnum=11
      !  get GRIB2 parameter category and number from GRIB1
      !  parameter number
      call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
      ipdstmpl(3)=4
      ipdstmpl(4)=0
      ipdstmpl(5)=kpds(2)
      ipdstmpl(6)=0
      ipdstmpl(7)=0
      ipdstmpl(8)=kpds(13)
      if (kpds(13).eq.254) ipdstmpl(8)=13
      ipdstmpl(9)=kpds(14)
      call cnvlevel(kpds(6),kpds(7),ipdstmpl)
      !ipdstmpl(9)=kpds(15)
      if (kens(2).eq.1) then
          !                if (kens(3).eq.1) ipdstmpl(16)=0
          !                if (kens(3).eq.2) ipdstmpl(16)=1
          ipdstmpl(16)=kens(3)-1
          ipdstmpl(17)=0
      elseif (kens(2).eq.2) then
          ipdstmpl(16)=2
          ipdstmpl(17)=kens(3)
      elseif (kens(2).eq.3) then
          ipdstmpl(16)=3
          ipdstmpl(17)=kens(3)
      endif
      ipdstmpl(18)=10
      !  calculate ending time using initial ref-time, idat,
      !  and increment rinc.
      idat=0
      idat(1)=((kpds(21)-1)*100)+kpds(8)
      idat(2)=kpds(9)
      idat(3)=kpds(10)
      idat(4)=-500     ! EST
      idat(5)=kpds(11)
      idat(6)=kpds(12)
      rinc=0
      if ( ipdstmpl(8).eq.0 ) then
          rinc(3)=kpds(15)
      elseif ( ipdstmpl(8).eq.1 ) then
          rinc(2)=kpds(15)
      elseif ( ipdstmpl(8).eq.2 ) then
          rinc(1)=kpds(15)
      elseif ( ipdstmpl(8).eq.10 ) then
          rinc(2)=kpds(15) * 3
      elseif ( ipdstmpl(8).eq.11 ) then
          rinc(2)=kpds(15) * 6
      elseif ( ipdstmpl(8).eq.12 ) then
          rinc(2)=kpds(15) * 12
      elseif ( ipdstmpl(8).eq.13 ) then
          rinc(4)=kpds(15)
      endif
      call w3movdat(rinc,idat,jdat)     ! calculate end date/time
      ipdstmpl(19)=jdat(1)              ! year of end time
      ipdstmpl(20)=jdat(2)              ! month of end time
      ipdstmpl(21)=jdat(3)              ! day of end time
      ipdstmpl(22)=jdat(5)              ! hour of end time
      ipdstmpl(23)=jdat(6)              ! minute of end time
      ipdstmpl(24)=jdat(7)              ! second of end time
      ipdstmpl(25)=1
      ipdstmpl(26)=0
      if (kpds(16).eq.2) then
          ipdstmpl(27)=255
          if (kpds(5).eq.15) ipdstmpl(27)=2
          if (kpds(5).eq.16) ipdstmpl(27)=3
      elseif (kpds(16).eq.3) then
          ipdstmpl(27)=0
      elseif (kpds(16).eq.4) then
          ipdstmpl(27)=1
      elseif (kpds(16).eq.5) then
          ipdstmpl(27)=4
      endif
      ipdstmpl(28)=2
      ipdstmpl(29)=kpds(13)
      if (kpds(13).eq.254) ipdstmpl(29)=13
      ipdstmpl(30)=kpds(15)-kpds(14)
      ipdstmpl(31)=255
      ipdstmpl(32)=0
    elseif (kpds(16).eq.51) then
      !    over time range...
      ipdsnum=11
      !  get GRIB2 parameter category and number from GRIB1
      !  parameter number
      call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
            ipdstmpl(2))
      ipdstmpl(3)=4
      ipdstmpl(4)=0
      ipdstmpl(5)=kpds(2)
      ipdstmpl(6)=0
      ipdstmpl(7)=0
      ipdstmpl(8)=kpds(13)
      if (kpds(13).eq.254) ipdstmpl(8)=13
      ipdstmpl(9)=kpds(14)
      call cnvlevel(kpds(6),kpds(7),ipdstmpl)
      !ipdstmpl(9)=kpds(15)
      if (kens(2).eq.1) then
          !                if (kens(3).eq.1) ipdstmpl(16)=0
          !                if (kens(3).eq.2) ipdstmpl(16)=1
          ipdstmpl(16)=kens(3)-1
          ipdstmpl(17)=0
      elseif (kens(2).eq.2) then
          ipdstmpl(16)=2
          ipdstmpl(17)=kens(3)
      elseif (kens(2).eq.3) then
          ipdstmpl(16)=3
          ipdstmpl(17)=kens(3)
      endif
      ipdstmpl(18)=10
      !  calculate ending time using initial ref-time, idat,
      !  and increment rinc.
      idat=0
      idat(1)=((kpds(21)-1)*100)+kpds(8)
      idat(2)=kpds(9)
      idat(3)=kpds(10)
      idat(4)=-500     ! EST
      idat(5)=kpds(11)
      idat(6)=kpds(12)
      rinc=0
      if ( ipdstmpl(8).eq.0 ) then
          rinc(3)=kpds(15)
      elseif ( ipdstmpl(8).eq.1 ) then
          rinc(2)=kpds(15)
      elseif ( ipdstmpl(8).eq.2 ) then
          rinc(1)=kpds(15)
      elseif ( ipdstmpl(8).eq.10 ) then
          rinc(2)=kpds(15) * 3
      elseif ( ipdstmpl(8).eq.11 ) then
          rinc(2)=kpds(15) * 6
      elseif ( ipdstmpl(8).eq.12 ) then
          rinc(2)=kpds(15) * 12
      elseif ( ipdstmpl(8).eq.13 ) then
          rinc(4)=kpds(15)
      endif
      call w3movdat(rinc,idat,jdat)     ! calculate end date/time
      ipdstmpl(19)=jdat(1)              ! year of end time
      ipdstmpl(20)=jdat(2)              ! month of end time
      ipdstmpl(21)=jdat(3)              ! day of end time
      ipdstmpl(22)=jdat(5)              ! hour of end time
      ipdstmpl(23)=jdat(6)              ! minute of end time
      ipdstmpl(24)=jdat(7)              ! second of end time
      ipdstmpl(25)=1
      ipdstmpl(26)=0
      ipdstmpl(27)=51
      ipdstmpl(28)=2
      ipdstmpl(29)=kpds(13)
      if (kpds(13).eq.254) ipdstmpl(29)=13
      ipdstmpl(30)=kpds(15)-kpds(14)
      ipdstmpl(31)=255
      ipdstmpl(32)=0
    else
      Print *,' Unrecognized Time Range Ind for ensembles = ', &
            kpds(16),kens(2)
      Print *,'pds2pdtens: Couldn:t construct  PDS Template '
      iret=1
    endif

  elseif (kens(2).eq.5) then         ! WHOLE or CLUSTERENSEMBLE type
    if (kpds(5).eq.191.OR.kpds(5).eq.192) then   ! probs
      if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
          !   At specific point in time...
          ipdsnum=5
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=5
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          !if (kpds(16).eq.10) then
          !  ipdstmpl(9)=(kpds(14)*256)+kpds(15)
          !else
          ipdstmpl(9)=kpds(14)
          !endif
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          ipdstmpl(16)=0   !?
          ipdstmpl(17)=kclust(1)   !?
          ipdstmpl(18)=kprob(2)-1
          if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(19)=3
            ipdstmpl(20)=nint(xprob(1)*1000.0)
          else
            ipdstmpl(19)=0
            ipdstmpl(20)=0
          endif
          if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(21)=3
            ipdstmpl(22)=nint(xprob(2)*1000.0)
          else
            ipdstmpl(21)=0
            ipdstmpl(22)=0
          endif
      elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
          !    over time range...
          ipdsnum=9
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=5
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !ipdstmpl(9)=kpds(15)
          ipdstmpl(16)=0   !?
          ipdstmpl(17)=kclust(1)   !?
          ipdstmpl(18)=kprob(2)-1
          if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(19)=3
            ipdstmpl(20)=nint(xprob(1)*1000.0)
          else
            ipdstmpl(19)=0
            ipdstmpl(20)=0
          endif
          if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(21)=3
            ipdstmpl(22)=nint(xprob(2)*1000.0)
          else
            ipdstmpl(21)=0
            ipdstmpl(22)=0
          endif
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0
          if ( ipdstmpl(8).eq.0 ) then
            rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
            rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
            rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
            rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
            rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
            rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
            rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(23)=jdat(1)              ! year of end time
          ipdstmpl(24)=jdat(2)              ! month of end time
          ipdstmpl(25)=jdat(3)              ! day of end time
          ipdstmpl(26)=jdat(5)              ! hour of end time
          ipdstmpl(27)=jdat(6)              ! minute of end time
          ipdstmpl(28)=jdat(7)              ! second of end time
          ipdstmpl(29)=1
          ipdstmpl(30)=0
          if (kpds(16).eq.2) then
            ipdstmpl(31)=255
            if (kpds(5).eq.15) ipdstmpl(31)=2
            if (kpds(5).eq.16) ipdstmpl(31)=3
          elseif (kpds(16).eq.3) then
            ipdstmpl(31)=0
          elseif (kpds(16).eq.4) then
            ipdstmpl(31)=1
          elseif (kpds(16).eq.5) then
            ipdstmpl(31)=4
          endif
          ipdstmpl(32)=2
          ipdstmpl(33)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(33)=13
          ipdstmpl(34)=kpds(15)-kpds(14)
          ipdstmpl(35)=255
          ipdstmpl(36)=0
      elseif (kpds(16).eq.51) then
          !    over time range...
          ipdsnum=9
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kprob(1),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=5
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !ipdstmpl(9)=kpds(15)
          ipdstmpl(16)=0   !?
          ipdstmpl(17)=kclust(1)   !?
          ipdstmpl(18)=kprob(2)-1
          if (ipdstmpl(18).eq.0.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(19)=3
            ipdstmpl(20)=nint(xprob(1)*1000.0)
          else
            ipdstmpl(19)=0
            ipdstmpl(20)=0
          endif
          if (ipdstmpl(18).eq.1.OR.ipdstmpl(18).eq.2) then
            ipdstmpl(21)=3
            ipdstmpl(22)=nint(xprob(2)*1000.0)
          else
            ipdstmpl(21)=0
            ipdstmpl(22)=0
          endif
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0
          if ( ipdstmpl(8).eq.0 ) then
            rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
            rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
            rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
            rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
            rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
            rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
            rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(23)=jdat(1)              ! year of end time
          ipdstmpl(24)=jdat(2)              ! month of end time
          ipdstmpl(25)=jdat(3)              ! day of end time
          ipdstmpl(26)=jdat(5)              ! hour of end time
          ipdstmpl(27)=jdat(6)              ! minute of end time
          ipdstmpl(28)=jdat(7)              ! second of end time
          ipdstmpl(29)=1
          ipdstmpl(30)=0
          ipdstmpl(31)=51
          ipdstmpl(32)=2
          ipdstmpl(33)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(33)=13
          ipdstmpl(34)=kpds(15)-kpds(14)
          ipdstmpl(35)=255
          ipdstmpl(36)=0
      else
          Print *,' Unrecognized Time Range Ind for Probs = ', &
              kpds(16),kens(2)
          Print *,'pds2pdtens: Couldn:t construct  PDS Template '
          iret=10
      endif
    else      ! Non-probablility Whole Ens Fcst
      if (kpds(16).eq.0.or.kpds(16).eq.1.or.kpds(16).eq.10) then
          !   At specific point in time...
          ipdsnum=2
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=4
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          !if (kpds(16).eq.10) then
          !  ipdstmpl(9)=(kpds(14)*256)+kpds(15)
          !else
          ipdstmpl(9)=kpds(14)
          !endif
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          if (kens(4).eq.1) then
            ipdstmpl(16)=0
          elseif (kens(4).eq.2) then
            ipdstmpl(16)=1
          elseif (kens(4).eq.11) then
            ipdstmpl(16)=2
          elseif (kens(4).eq.12) then
            ipdstmpl(16)=3
          endif
          ipdstmpl(17)=kclust(1)
      elseif (kpds(16).ge.2.AND.kpds(16).le.5) then
          !    over time range...
          ipdsnum=12
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=4
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !ipdstmpl(9)=kpds(15)
          if (kens(4).eq.1) then
            ipdstmpl(16)=0
          elseif (kens(4).eq.2) then
            ipdstmpl(16)=1
          elseif (kens(4).eq.11) then
            ipdstmpl(16)=2
          elseif (kens(4).eq.12) then
            ipdstmpl(16)=3
          endif
          ipdstmpl(17)=kclust(1)
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0
          if ( ipdstmpl(8).eq.0 ) then
            rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
            rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
            rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
            rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
            rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
            rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
            rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(18)=jdat(1)              ! year of end time
          ipdstmpl(19)=jdat(2)              ! month of end time
          ipdstmpl(20)=jdat(3)              ! day of end time
          ipdstmpl(21)=jdat(5)              ! hour of end time
          ipdstmpl(22)=jdat(6)              ! minute of end time
          ipdstmpl(23)=jdat(7)              ! second of end time
          ipdstmpl(24)=1
          ipdstmpl(25)=0
          if (kpds(16).eq.2) then
            ipdstmpl(26)=255
            if (kpds(5).eq.15) ipdstmpl(26)=2
            if (kpds(5).eq.16) ipdstmpl(26)=3
          elseif (kpds(16).eq.3) then
            ipdstmpl(26)=0
          elseif (kpds(16).eq.4) then
            ipdstmpl(26)=1
          elseif (kpds(16).eq.5) then
            ipdstmpl(26)=4
          endif
          ipdstmpl(27)=2
          ipdstmpl(28)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(28)=13
          ipdstmpl(29)=kpds(15)-kpds(14)
          ipdstmpl(30)=255
          ipdstmpl(31)=0
      elseif (kpds(16).eq.51) then
          !    over time range...
          ipdsnum=12
          !  get GRIB2 parameter category and number from GRIB1
          !  parameter number
          call param_g1_to_g2(kpds(5),kpds(19),idum,ipdstmpl(1), &
              ipdstmpl(2))
          ipdstmpl(3)=4
          ipdstmpl(4)=0
          ipdstmpl(5)=kpds(2)
          ipdstmpl(6)=0
          ipdstmpl(7)=0
          ipdstmpl(8)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(8)=13
          ipdstmpl(9)=kpds(14)
          call cnvlevel(kpds(6),kpds(7),ipdstmpl)
          !ipdstmpl(9)=kpds(15)
          if (kens(4).eq.1) then
            ipdstmpl(16)=0
          elseif (kens(4).eq.2) then
            ipdstmpl(16)=1
          elseif (kens(4).eq.11) then
            ipdstmpl(16)=2
          elseif (kens(4).eq.12) then
            ipdstmpl(16)=3
          endif
          ipdstmpl(17)=kclust(1)
          !  calculate ending time using initial ref-time, idat,
          !  and increment rinc.
          idat=0
          idat(1)=((kpds(21)-1)*100)+kpds(8)
          idat(2)=kpds(9)
          idat(3)=kpds(10)
          idat(4)=-500     ! EST
          idat(5)=kpds(11)
          idat(6)=kpds(12)
          rinc=0
          if ( ipdstmpl(8).eq.0 ) then
            rinc(3)=kpds(15)
          elseif ( ipdstmpl(8).eq.1 ) then
            rinc(2)=kpds(15)
          elseif ( ipdstmpl(8).eq.2 ) then
            rinc(1)=kpds(15)
          elseif ( ipdstmpl(8).eq.10 ) then
            rinc(2)=kpds(15) * 3
          elseif ( ipdstmpl(8).eq.11 ) then
            rinc(2)=kpds(15) * 6
          elseif ( ipdstmpl(8).eq.12 ) then
            rinc(2)=kpds(15) * 12
          elseif ( ipdstmpl(8).eq.13 ) then
            rinc(4)=kpds(15)
          endif
          call w3movdat(rinc,idat,jdat)     ! calculate end date/time
          ipdstmpl(18)=jdat(1)              ! year of end time
          ipdstmpl(19)=jdat(2)              ! month of end time
          ipdstmpl(20)=jdat(3)              ! day of end time
          ipdstmpl(21)=jdat(5)              ! hour of end time
          ipdstmpl(22)=jdat(6)              ! minute of end time
          ipdstmpl(23)=jdat(7)              ! second of end time
          ipdstmpl(24)=1
          ipdstmpl(25)=0
          ipdstmpl(26)=51
          ipdstmpl(27)=2
          ipdstmpl(28)=kpds(13)
          if (kpds(13).eq.254) ipdstmpl(28)=13
          ipdstmpl(29)=kpds(15)-kpds(14)
          ipdstmpl(30)=255
          ipdstmpl(31)=0
      endif
    endif
  else
    Print *,' Unrecognized Ensemble type = ',kens(2)
    Print *,'pds2pdtens: Couldn:t construct  PDS Template '
    iret=2
  endif

  return
end subroutine pds2pdtens
