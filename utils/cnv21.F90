!> @file
!> @brief Convert every GRIB2 field to individual GRIB1 messages.
!> @author Stephen Gilbert @date 2003-06-11

!> This subroutine converts every GRIB2 field in a file to a GRIB1
!> field. If a GRIB2 message contains more than one data field, then
!> each field is saved in individual GRIB1 messages.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-11 | Gilbert | Initial
!> 2008-05-14 | Vuong | Add option -m0 No explicit missing values included within data values.
!>
!> @param[in] ifl1 Fortran unit number of input GRIB2 file.
!> @param[in] ifl2 Fortran unit number of output GRIB1 file.
!>
!> @author Stephen Gilbert @date 2003-06-11
subroutine cnv21(ifl1,ifl2)

  use grib_mod
  use params
  integer,intent(in) :: ifl1,ifl2

  CHARACTER(len=1),allocatable,dimension(:) :: cgrib
  type(gribfield) :: gfld
  integer,dimension(200) :: jids,jpdt,jgdt
  integer :: kpds(200),kgds(200),kens(200),kprob(2)
  integer :: kclust(16),kmembr(80)
  integer :: currlen=0
  integer :: igds(5)=(/0,0,0,0,0/)
  real :: xprob(2)
  logical :: unpack=.true.
  !
  !
  IFLI1=0
  jdisc=-1
  jids=-9999
  jpdt=-9999
  jgdt=-9999
  jpdtn=-1
  jgdtn=-1
  !
  icount=0
  jskp=0
  do
     call getgb2(ifl1,ifli1,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt, &
          unpack,jskp,gfld,iret)
     if (iret.ne.0) then
        if (iret.eq.99) exit
        print *,' getgb2 error = ',iret
        cycle
        !call errexit(17)
     endif
     icount=icount+1
     !
     !  Ensure that cgrib array is large enough
     !
     newlen=4*gfld%ngrdpts
     if (newlen.gt.currlen) then
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(newlen),stat=is)
        currlen=newlen
     endif
     !
     !   Construct GDS
     !
     igds(1)=gfld%griddef
     igds(2)=gfld%ngrdpts
     igds(3)=gfld%numoct_opt
     igds(4)=gfld%interp_opt
     igds(5)=gfld%igdtnum
     if (.NOT. associated(gfld%list_opt)) &
          allocate(gfld%list_opt(1))
     call gdt2gds(igds,gfld%igdtmpl,gfld%num_opt,gfld%list_opt, &
          kgds,igrid,iret)
     if (iret.ne.0) then
        print *,'cnv21: could not create gds'
        cycle
     endif
     !print *,' SAGT: NCEP GRID: ',igrid
     !
     !   Construct PDS
     !
     call makepds(gfld%discipline,gfld%idsect,gfld%ipdtnum, &
          gfld%ipdtmpl,gfld%ibmap,gfld%idrtnum, &
          gfld%idrtmpl,kpds,iret)
     if (iret.ne.0) then
        print *,'cnv21: could not create pds in GRIB1'
        cycle
     endif
     kpds(3)=igrid
     !
     !  Check for Coastal Ocean circulation and UKMET grib grid id.
     !  ON 388 defined grid id 238 same as grid 244
     !  If the process model is 45, and UK Met(74), the grid id is 2 or 45
     !  If the process model is 121, the grid id is 238
     !  If the process model is 123, the grid id is 244
     !
     if (kpds(1).eq.7.AND.kpds(2).eq.121) kpds(3)=238
     if (kpds(1).eq.7.AND.kpds(2).eq.123) kpds(3)=244
     if (kpds(1).eq.74) then
        if (kpds(2).eq.45.AND.kpds(3).eq.2)  kpds(3)=2
        if (kpds(2).eq.15.AND.kpds(3).eq.45) kpds(3)=45
        if (kpds(2).eq.45.AND.kpds(3).eq.45) kpds(3)=45
     end if
     !
     !   Construct Ensemble info, if necessary
     !
     if ((gfld%ipdtnum.ge.1.AND.gfld%ipdtnum.le.6).OR. &
          (gfld%ipdtnum.ge.9.AND.gfld%ipdtnum.le.14)) then
        call makepdsens(gfld%ipdtnum,gfld%ipdtmpl,kpds,kens,kprob, &
             xprob,kclust,kmembr,iret)
     endif
     !
     !   If not using bit-map, must assign dummy bit-map
     !
     if (gfld%ibmap.ne.0 .AND. gfld%ibmap.ne.254) then
        !gfld%bmap => dummy
        if ((gfld%idrtnum.eq.2 .OR. gfld%idrtnum.eq.3) .AND. &
             gfld%idrtmpl(7).ne.0) then       ! convert missings to bitmap
           allocate(gfld%bmap(gfld%ngrdpts))
           kpds(4)=ior(kpds(4),64)
           if (gfld%idrtmpl(7).eq.1) then
              call rdieee(gfld%idrtmpl(8),rmiss1,1)
              do i=1,gfld%ngrdpts
                 if (gfld%fld(i) .eq. rmiss1) then
                    gfld%bmap(i)=.false.
                 else
                    gfld%bmap(i)=.true.
                 endif
              enddo
           endif
           if (gfld%idrtmpl(7).eq.2) then
              call rdieee(gfld%idrtmpl(8),rmiss1,1)
              call rdieee(gfld%idrtmpl(9),rmiss2,1)
              do i=1,gfld%ngrdpts
                 if (gfld%fld(i).eq.rmiss1 .OR. &
                      gfld%fld(i).eq.rmiss2) then
                    gfld%bmap(i)=.false.
                 else
                    gfld%bmap(i)=.true.
                 endif
              enddo
           endif
        endif
        if ((gfld%idrtnum.eq.2 .OR. gfld%idrtnum.eq.3) .AND. &
             gfld%idrtmpl(7).eq.0) then       ! convert missings to bitmap
           allocate(gfld%bmap(gfld%ngrdpts))
           kpds(4)=ior(kpds(4),64)
           call rdieee(gfld%idrtmpl(8),rmiss1,1)
           if (rmiss1 .lt. -9999.0) then
              rmiss1=rmiss1*10.0
           else
              rmiss1=-9999.0
           endif
           do i=1,gfld%ngrdpts
              if (gfld%fld(i) .eq. rmiss1) then
                 gfld%bmap(i)=.false.
              else
                 gfld%bmap(i)=.true.
              endif
           enddo
        endif
     endif
     !
     !   Pack and write GRIB 1 field
     !
     ibs=gfld%idrtmpl(2)
     !print *,'SAGT:before putgbexn'
     if (.NOT. associated(gfld%bmap)) allocate(gfld%bmap(1))
     imug=0
     call putgbexn(ifl2,gfld%ngrdpts,kpds,kgds,kens,kprob, &
          xprob,kclust,kmembr,ibs,imug,gfld%bmap, &
          gfld%fld,iret)
     !print *,'SAGT:after putgbexn'
     if (iret.ne.0) then
        print *,' putgbexn error = ',iret
        cycle
        !call errexit(17)
     endif

     call gf_free(gfld)

  enddo

  if (allocated(cgrib)) deallocate(cgrib)

  return
end subroutine cnv21

!> This routine creates a GRIB1 PDS (Section 1) from appropriate
!> information from a GRIB2 Product Definition Template.
!>
!> @note Use pds2pdtens for ensemble related PDS.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial.
!> 2005-04-19 | Gilbert | Changed scaling factor used with potential vorticity surfaces.
!> 2007-05-08 | VUONG | Add Product Definition Template entries 120-124, 131, 88, 45, 47.
!> 2007-05-14 | Boi Vuong | Added Time Range Indicator 51 (Climatological Mean Value)
!> 2007-10-24 | Boi Vuong | Added level 8 (Nominal top of atmosphere)
!> 2009-05-19 | Boi Vuong | Added levels 10(Entire Atmosphere), 11(Cumulonimbus Base),12(Cumulonimbus Top) and level 126(Isobaric Pa)
!> 2009-12-14 | Boi Vuong | Added check for WAFS to use PDT 4.15 for Icing, Turbulence and Cumulonimbus
!> 2010-08-10 | Boi Vuong | Added check for FNMOC to use TMP as TMAX and TMIN - Removed check WAFS MAX wind level
!> 2011-10-24 | Boi Vuong | Added check for NAM (NMM-B) parameters to set statistical processing as MAX and MIN
!> 2012-03-29 | Boi Vuong | Added check Time Range for APCP in FNMOC
!> 2014-05-20 | Boi Vuong | Added check Time Range after F252
!> 2014-11-14 | Boi Vuong | Added check Time Range for 15-hr or 18-hr or 21-hr or 24-hr Accumulation for APCP after F240
!> 2018-07-26 | Boi Vuong | Added check Time Range for continuous accumulated APCP after F252 when convert from grib2 to grib1
!>
!> @param[in] idisc GRIB2 discipline from Section 0.
!> @param[in] idsect GRIB2 Section 1 info.
!> - idsect(1) Id of orginating centre (Common Code Table C-1)
!> - idsect(2) Id of orginating sub-centre (local table)
!> - idsect(3) GRIB Master Tables Version Number (Code Table 1.0)
!> - idsect(4) GRIB Local Tables Version Number (Code Table 1.1)
!> - idsect(5) Significance of Reference Time (Code Table 1.2)
!> - idsect(6) Reference Time - Year (4 digits)
!> - idsect(7) Reference Time - Month
!> - idsect(8) Reference Time - Day
!> - idsect(9) Reference Time - Hour
!> - idsect(10) Reference Time - Minute
!> - idsect(11) Reference Time - Second
!> - idsect(12) Production status of data (Code Table 1.3)
!> - idsect(13) Type of processed data (Code Table 1.4)
!> @param[in] ipdsnum GRIB2 Product Definition Template Number
!> @param[in] ipdstmpl GRIB2 Product Definition Template entries for PDT 4.ipdsnum
!> @param[in] ibmap GRIB2 bitmap indicator from octet 6, Section 6.
!> @param[in] idrsnum GRIB2 Data Representation Template Number
!> @param[in] idrstmpl GRIB2 Data Representation Template entries
!> @param[out] kpds GRIB1 PDS info as specified in W3FI63.
!> - 1 id of center
!> - 2 generating process id number
!> - 3 grid definition
!> - 4 gds/bms flag (right adj copy of octet 8)
!> - 5 indicator of parameter
!> - 6 type of level
!> - 7 height/pressure , etc of level
!> - 8 year including (century-1)
!> - 9 month of year
!> - 10 day of month
!> - 11 hour of day
!> - 12 minute of hour
!> - 13 indicator of forecast time unit
!> - 14 time range 1
!> - 15 time range 2
!> - 16 time range flag
!> - 17 number included in average
!> - 18 version nr of grib specification
!> - 19 version nr of parameter table
!> - 20 nr missing from average/accumulation
!> - 21 century of reference time of data
!> - 22 units decimal scale factor
!> - 23 subcenter number
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 1 Don't know what to do with pre-defined bitmap.
!> - 2 Unrecognized GRIB2 PDT 4.ipdsnum
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine makepds(idisc,idsect,ipdsnum,ipdstmpl,ibmap, &
    idrsnum,idrstmpl,kpds,iret)

  use params

  integer,intent(in) :: idsect(*),ipdstmpl(*),idrstmpl(*)
  integer,intent(in) :: ipdsnum,idisc,idrsnum,ibmap
  integer,intent(out) :: kpds(*)
  integer,intent(out) :: iret

  iret=0
  ipos=0
  kpds(1:24)=0
  if ( (ipdsnum.lt.0).OR.(ipdsnum.gt.15) ) then
    print *,'makepds: Don:t know GRIB2 PDT 4.',ipdsnum
    iret=2
    return
  endif

  kpds(1)=idsect(1)
  kpds(2)=ipdstmpl(5)
  kpds(3)=255
  kpds(4)=128
  if ( ibmap.ne.255 ) kpds(4)=kpds(4)+64
  if ( ibmap.ge.1.AND.ibmap.le.253 ) then
    print *,'makepds: Don:t know about predefined bit-map ',ibmap
    iret=1
    return
  endif
  call param_g2_to_g1(idisc,ipdstmpl(1),ipdstmpl(2),kpds(5), &
      kpds(19))
  !
  !  Special parameters for ICAO WAFS (Max Icing, TP and CAT)
  !
  If (ipdstmpl(16).eq.2.and.ipdstmpl(1).eq.19.and. &
      ipdstmpl(2).eq.20) kpds(5) = 169
  If (ipdstmpl(16).eq.2.and.ipdstmpl(1).eq.19.and. &
      ipdstmpl(2).eq.21) kpds(5) = 171
  If (ipdstmpl(16).eq.2.and.ipdstmpl(1).eq.19.and. &
      ipdstmpl(2).eq.22) kpds(5) = 173
  !
  !  Special parameters for NAM (NMMB)
  !
  If (idisc.eq.0.and.ipdstmpl(1).eq.2) then
    if (ipdstmpl(2).eq.220) then
        kpds(5) = 237
        kpds(19) = 129
    end if
    if (ipdstmpl(2).eq.221) then
        kpds(5) = 238
        kpds(19) = 129
    end if
    if (ipdstmpl(2).eq.222) then
        kpds(5) = 253
        kpds(19) = 129
    end if
    if (ipdstmpl(2).eq.223) then
        kpds(5) = 254
        kpds(19) = 129
    end if
  endif
  !
  If (idisc.eq.0.and.ipdstmpl(2).eq.16 &
      .and.ipdstmpl(3).eq.198) then
    kpds(5) = 235
    kpds(19) = 129
  endif
  !
  If (idisc.eq.0.and.ipdstmpl(2).eq.7 &
      .and.ipdstmpl(3).eq.199) then
    kpds(5) = 236
    kpds(19) = 129
  endif
  !
  !  Special parameters for ICAO Height at CB Base and Top
  !  in GRIB1 Table 140
  !
  If (ipdstmpl(1).eq.3.and.ipdstmpl(2).eq.3) then
    If (ipdstmpl(10).eq.11) then
        kpds(19) = 140
        kpds(5)  = 179
    end if
    If (ipdstmpl(10).eq.12) then
        kpds(19) = 140
        kpds(5)  = 180
    end if
  end if
  !
  call levelcnv(ipdstmpl,kpds(6),kpds(7))      ! level
  kpds(8)=mod(idsect(6),100)
  if ( kpds(8).eq.0 ) kpds(8)=100
  kpds(9)=idsect(7)                            ! Year
  kpds(10)=idsect(8)                           ! Month
  kpds(11)=idsect(9)                           ! Day
  kpds(12)=idsect(10)                          ! Hour
  if ( ipdstmpl(8).ne.13 ) then
    kpds(13)=ipdstmpl(8)                      ! Time Unit
  else
    kpds(13)=254
  endif
  kpds(14)=ipdstmpl(9)                         ! P1
  if ( ipdsnum.le.7 ) then                     ! P2
    kpds(15)=0
    kpds(16)=0
    kpds(20)=0
    if ( kpds(14).eq.0 ) kpds(16)=1
    if ( kpds(14).gt.255 ) kpds(16)=10
    if ( ipdstmpl(5).eq.77.OR.ipdstmpl(5).eq.81.OR. &
          ipdstmpl(5).eq.96.OR.ipdstmpl(5).eq.80.OR. &
          ipdstmpl(5).eq.82.OR.ipdstmpl(5).eq.120.OR. &
          ipdstmpl(5).eq.47.OR.ipdstmpl(5).eq.11 ) then
        kpds(16)=10
    end if
    if (ipdstmpl(5).eq.84.AND.kpds(5).eq.154)kpds(16) = 10
    !
    !          NOAA Wave Watch III and Coastal Ocean Circulation
    !          and Alaska Waters Regional Wave Model
    !
    if ( ipdstmpl(5).eq.88.OR.ipdstmpl(5).eq.121 &
          .OR.ipdstmpl(5).eq.122.OR.ipdstmpl(5).eq.123 &
          .OR.ipdstmpl(5).eq.124.OR.ipdstmpl(5).eq.125 &
          .OR.ipdstmpl(5).eq.131.OR.ipdstmpl(5).eq.45 &
          .OR.ipdstmpl(5).eq.11 ) then
        kpds(16) = 0
        !
        ! Level Surface set to 1
        !
        if (kpds(5).eq.80.OR.kpds(5).eq.82.OR. &
            kpds(5).eq.88.OR.kpds(5).eq.49.OR. &
            kpds(5).eq.50) kpds(7)=1  ! Level Surface
        if (ipdstmpl(5).eq.122.OR.ipdstmpl(5).eq.124.OR. &
            ipdstmpl(5).eq.131.OR.ipdstmpl(5).eq.123.OR. &
            ipdstmpl(5).eq.125.OR.ipdstmpl(5).eq.88.OR. &
            ipdstmpl(5).eq.121) kpds(7)=1
        if (idsect(1).eq.54.AND.ipdstmpl(5).eq.45) kpds(16) = 10
    endif
  else
    selectcase (ipdsnum)
    case(8)
        ipos=24
    case(9)
        ipos=31
    case(10)
        ipos=25
    case(11)
        ipos=27
    case(12)
        ipos=26
    case(13)
        ipos=40
    case(14)
        ipos=39
    end select
    kpds(15)=ipdstmpl(ipos+3)+kpds(14)  ! P2
    selectcase (ipdstmpl(ipos))
    case (255)
        kpds(16)=2
    case (0)
        kpds(16)=3
    case (1)
        kpds(16)=4
    case (2)
        kpds(16)=2
    case (3)
        kpds(16)=2
    case (4)
        kpds(16)=5
    case (51)
        kpds(16)=51
    end select
    kpds(20)=ipdstmpl(ipos-1)
  endif
  if (ipdstmpl(9) .ge. 252) then
    if (ipdstmpl(ipos+3).eq.3) then
        kpds(13)= 10                          ! Forecast time unit is 3-hour
        kpds(14)=ipdstmpl(9)/3                ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/3+kpds(14)  ! Time range P2
    else if (ipdstmpl(ipos+3).eq.6) then
        kpds(13)= 11                          ! Forecast time unit is 6-hour
        kpds(14)=ipdstmpl(9)/6                ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/6+kpds(14)  ! Time range P2
    else if (ipdstmpl(ipos+3).eq.12) then
        kpds(13)= 12                          ! Forecast time unit is 12-hour
        kpds(14)=ipdstmpl(9)/12               ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/12+kpds(14) ! Time range P2
    end if
  end if
  if (ipdsnum .eq. 8 .AND. ipdstmpl(9) .eq. 0) then
    if (ipdstmpl(ipos+3).ge.252) then
        kpds(13)= 10                          ! Forecast time unit is hour
        kpds(14)=ipdstmpl(9)/3                ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/3+kpds(14)  ! Time range P2
    end if
  end if
  !
  !  Checking total preciptation for 15-hr or 18-hr or 21-hr or 24-hr accumulation
  !  after forecast hour F240
  !
  if (ipdstmpl(9) .ge. 240 )then
    if ( ipdstmpl(ipos+3).eq.15 .OR. ipdstmpl(ipos+3).eq.18 &
          .OR. ipdstmpl(ipos+3).eq.21 .OR. &
          ipdstmpl(ipos+3).eq.24 ) then
        kpds(13)= 10                          ! Forecast time unit is 3-hour
        kpds(14)=ipdstmpl(9)/3                ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/3+kpds(14)  ! Time range P2
    end if
  end if
  !
  !   Checking Unit of Time Range for FNMOC (APCP)
  !
  if (ipdstmpl(4).eq.58 .AND. ipdsnum.eq.11 .AND. &
      (ipdstmpl(1).eq.1 .AND.ipdstmpl(2).eq.8) &
      .AND. (ipdstmpl(10).eq.1)) then
    if (ipdstmpl(9) .ge. 252) then
        kpds(13)= 11      !  Forecast time unit is 6-hour
        kpds(14)=ipdstmpl(9)/6      ! Time range P1
        kpds(15)=ipdstmpl(ipos+3)/6+kpds(14)  ! Time range P2
    else
        kpds(13)= 1       !  Forecast time unit is 1-hour
        kpds(14)=ipdstmpl(9)  ! Time range P1
    end if
  end if
  !
  !   Special case for FNMOC (TMAX and TMIN)
  !
  if (ipdstmpl(4).eq.58 .AND. ipdsnum.eq.11 .AND. &
      (ipdstmpl(1).eq.0 &
      .AND.ipdstmpl(2).eq.0).AND.(ipdstmpl(10).eq.103)) then
    kpds(16) = 2
    !   For Maximum Temperature
    If (ipdstmpl(27).eq.2 .AND. ipdstmpl(1).eq.0 .AND. &
          ipdstmpl(2).eq.0) kpds(5) = 15
    !   For Minimum Temperature
    If (ipdstmpl(27).eq.3 .AND. ipdstmpl(1).eq.0 .AND. &
          ipdstmpl(2).eq.0) kpds(5) = 16
  end if
  !
  !   Special case for WAFS (Mean/MAx IP,CTP and CAT)
  !
  if (ipdstmpl(5).eq.96.AND.((ipdstmpl(1).eq.19) &
      .AND.(ipdstmpl(2).eq.20.or.ipdstmpl(2).eq.21.or. &
      ipdstmpl(2).eq.22)).AND.(ipdstmpl(10).eq.100)) then
    kpds(16) = 10
  end if
  !
  kpds(17)=0
  kpds(18)=1                                   ! GRIB edition
  kpds(21)=(idsect(6)/100)+1                   ! Century
  if ( kpds(8).eq.100 ) kpds(21)=idsect(6)/100
  kpds(22)=idrstmpl(3)                         ! Decimal scale factor
  kpds(23)=idsect(2)                           ! Sub-center
  return
end subroutine makepds

!> This routine converts Level/layer information from a GRIB2 Product
!> Definition Template to GRIB1 Level type and Level value.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial
!> 2007-10-24 | Boi Vuong | Added level 8 (Nominal top of atmosphere)
!> 2011-01-13 | Boi Vuong | Added level/layer values from 235 to 239
!>
!> @param[in] ipdstmpl GRIB2 Product Definition Template values
!> @param[out] ltype GRIB1 level type (PDS octet 10)
!> @param[out] lval GRIB1 level/layer value(s) (PDS octets 11 and 12)
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine levelcnv(ipdstmpl,ltype,lval)
  integer,intent(in) :: ipdstmpl(*)
  integer,intent(out) :: ltype,lval

  ltype=255
  lval=0
  ltype1=ipdstmpl(10)
  ltype2=ipdstmpl(13)

  if ( ltype1.eq.10.AND.ltype2.eq.255 ) then
     ltype=200
     lval=0
  elseif ( ltype1.eq.11.AND.ltype2.eq.255 ) then
     ltype=216
     lval=0
  elseif ( ltype1.eq.12.AND.ltype2.eq.255 ) then
     ltype=217
     lval=0
  elseif ( ltype1.lt.100.AND.ltype2.eq.255 ) then
     ltype=ltype1
     lval=0
  elseif ( ltype1.eq.1.AND.ltype2.eq.8 ) then
     ltype=ltype1
     lval=0
  elseif ( ltype1.eq.10.AND.ltype2.eq.255 ) then
     ltype=200
     lval=0
  elseif ( ltype1.eq.235.AND.ltype2.eq.255 ) then
     ltype=235
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1)
  elseif ( ltype1.ge.200.AND.ltype2.eq.255 ) then
     ltype=ltype1
     lval=0
  elseif (ltype1.eq.100.AND.ltype2.eq.255 ) then
     ltype=100
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1/100.)
  elseif (ltype1.eq.100.AND.ltype2.eq.100 ) then
     ltype=101
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1/1000.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2/1000.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.101.AND.ltype2.eq.255 ) then
     ltype=102
     lval=0
  elseif (ltype1.eq.102.AND.ltype2.eq.255 ) then
     ltype=103
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1.eq.102.AND.ltype2.eq.102 ) then
     ltype=104
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.103.AND.ltype2.eq.255 ) then
     ltype=105
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1.eq.103.AND.ltype2.eq.103 ) then
     ltype=106
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1/100.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2/100.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.104.AND.ltype2.eq.255 ) then
     ltype=107
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1*10000.)
  elseif (ltype1.eq.104.AND.ltype2.eq.104 ) then
     ltype=108
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2*100.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.105.AND.ltype2.eq.255 ) then
     ltype=109
     lval=ipdstmpl(12)
  elseif (ltype1.eq.105.AND.ltype2.eq.105 ) then
     ltype=110
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.106.AND.ltype2.eq.255 ) then
     ltype=111
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1*100.)
  elseif (ltype1.eq.106.AND.ltype2.eq.106 ) then
     ltype=112
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2*100.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.107.AND.ltype2.eq.255 ) then
     ltype=113
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1)
  elseif (ltype1.eq.107.AND.ltype2.eq.107 ) then
     ltype=114
     rscal1=10.**(-ipdstmpl(11))
     lval1=475-nint(real(ipdstmpl(12))*rscal1)
     rscal2=10.**(-ipdstmpl(14))
     lval2=475-nint(real(ipdstmpl(15))*rscal2)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.108.AND.ltype2.eq.255 ) then
     ltype=115
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1/100.)
  elseif (ltype1.eq.108.AND.ltype2.eq.108 ) then
     ltype=116
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1/100.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2/100.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.109.AND.ltype2.eq.255 ) then
     ltype=117
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1*1000000000.)
  elseif (ltype1.eq.111.AND.ltype2.eq.255 ) then
     ltype=119
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1*10000.)
  elseif (ltype1.eq.111.AND.ltype2.eq.111 ) then
     ltype=120
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1*100.)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2*100.)
     lval=(lval1*256)+lval2
  elseif (ltype1.eq.160.AND.ltype2.eq.255 ) then
     ltype=160
     rscal1=10.**(-ipdstmpl(11))
     lval=nint(real(ipdstmpl(12))*rscal1)
  elseif ((ltype1.ge.236.AND.ltype1.le.239).AND. &
       (ltype2.ge.236.AND.ltype2.le.239)) then
     ltype=ltype1
     rscal1=10.**(-ipdstmpl(11))
     lval1=nint(real(ipdstmpl(12))*rscal1)
     rscal2=10.**(-ipdstmpl(14))
     lval2=nint(real(ipdstmpl(15))*rscal2)
     lval=(lval1*256)+lval2
  else
     print *,'levelcnv: GRIB2 Levels ',ltype1,ltype2, &
          ' not recognized.'
     ltype=255
  endif

  !  High resolution stuff
  !        elseif (ltype.eq.121) then
  !           ipdstmpl(10)=100
  !           ipdstmpl(12)=(1100+(lval/256))*100
  !           ipdstmpl(13)=100
  !           ipdstmpl(15)=(1100+mod(lval,256))*100
  !        elseif (ltype.eq.125) then
  !           ipdstmpl(10)=103
  !           ipdstmpl(11)=-2
  !           ipdstmpl(12)=lval
  !        elseif (ltype.eq.128) then
  !           ipdstmpl(10)=104
  !           ipdstmpl(11)=-3
  !           ipdstmpl(12)=1100+(lval/256)
  !           ipdstmpl(13)=104
  !           ipdstmpl(14)=-3
  !           ipdstmpl(15)=1100+mod(lval,256)
  !        elseif (ltype.eq.141) then
  !           ipdstmpl(10)=100
  !           ipdstmpl(12)=(lval/256)*100
  !           ipdstmpl(13)=100
  !           ipdstmpl(15)=(1100+mod(lval,256))*100

  return
end subroutine levelcnv

!> This routine creates the GRIB1 NCEP Ensemble PDS extension
!> information from appropriate information from a GRIB2 Product
!> Definition Template.
!>
!> @note  Use pds2pdtens for ensemble related PDS.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-12 | Gilbert | Initial
!> 2007-05-14 | Boi Vuong | Corrected scale factor probabilities
!> 2010-07-26 | Boi Vuong | Added two type of ensemblers (4 and 192)
!>
!> @param[in] ipdsnum GRIB2 Product Definition Template Number
!> @param[in] ipdstmpl GRIB2 Product Definition Template entries for
!> PDT 4.ipdsnum
!> @param[inout] kpds GRIB1 PDS info as specified in W3FI63.
!> - 1 id of center
!> - 2 generating process id number
!> - 3 grid definition
!> - 4 gds/bms flag (right adj copy of octet 8)
!> - 5 indicator of parameter
!> - 6 type of level
!> - 7 height/pressure , etc of level
!> - 8 year including (century-1)
!> - 9 month of year
!> - 10 day of month
!> - 11 hour of day
!> - 12 minute of hour
!> - 13 indicator of forecast time unit
!> - 14 time range 1
!> - 15 time range 2
!> - 16 time range flag
!> - 17 number included in average
!> - 18 version nr of grib specification
!> - 19 version nr of parameter table
!> - 20 nr missing from average/accumulation
!> - 21 century of reference time of data
!> - 22 units decimal scale factor
!> - 23 subcenter number
!> @param[out] kens Ensemble identification for PDS octets 41-45
!> @param[out] kprob Ensemble probability info for PDS octets 46  47
!> @param[out] xprob Ensemble probability info for PDS octets 48-55
!> @param[out] kclust Ensemble cluster info for PDS octets 61-76
!> @param[out] kmembr Ensemble membership info for PDS octest 77-86
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 2 Unrecognized GRIB2 PDT 4.ipdsnum
!>
!> @author Stephen Gilbert @date 2003-06-12
subroutine makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob, &
    xprob,kclust,kmembr,iret)
  use params

  integer,intent(in) :: ipdstmpl(*)
  integer,intent(in) :: ipdsnum
  integer,intent(inout) :: kpds(*)
  integer,intent(out) :: kens(5),kprob(2)
  integer,intent(out) :: kclust(16),kmembr(80)
  real,intent(out) :: xprob(2)
  integer,intent(out) :: iret

  iret=0
  kpds(23)=2          !  subcenter = ensemble

  kens(1:5)=0
  kprob(1:2)=0
  xprob(1:2)=0.
  kclust(1:16)=0
  kmembr(1:80)=0
  !
  !  Individual Ensemble Fcst
  !
  if (ipdsnum.eq.1.OR.ipdsnum.eq.11) then
    kens(1)=1
    selectcase (ipdstmpl(16))
    case(0)
      kens(2)=1
      kens(3)=1
    case(1)
      kens(2)=1
      kens(3)=2
    case(2)
      kens(2)=2
      kens(3)=ipdstmpl(17)
    case(3)
      kens(2)=3
      kens(3)=ipdstmpl(17)
    case(4)
      kens(2)=3
      kens(3)=ipdstmpl(17)
    case(192)
      kens(2)=3
      kens(3)=ipdstmpl(17)
    end select
    kens(4)=1
    kens(5)=255
    !
    !  Probability Fcst
    !
  elseif (ipdsnum.eq.5.OR.ipdsnum.eq.9) then
    kens(1)=1
    kens(2)=5
    kens(3)=0
    kens(4)=0
    kens(5)=255
    kprob(1)=kpds(5)
    kpds(5)=191
    kprob(2)=ipdstmpl(18)+1
    if (kprob(2).eq.1) then
      rscale=10.**ipdstmpl(19)
      xprob(1)=real(ipdstmpl(20))/rscale
      xprob(2)=0.0
    elseif (kprob(2).eq.2) then
      xprob(1)=0.0
      rscale=10.**ipdstmpl(21)
      xprob(2)=real(ipdstmpl(22))/rscale
    elseif (kprob(2).eq.3) then
      rscale=10.**ipdstmpl(19)
      xprob(1)=real(ipdstmpl(20))/rscale
      rscale=10.**ipdstmpl(21)
      xprob(2)=real(ipdstmpl(22))/rscale
    endif
    kclust(1)=ipdstmpl(17)
    !
    !  Derived Ensemble Fcst
    !
  elseif (ipdsnum.eq.2.OR.ipdsnum.eq.12) then
    kens(1)=1
    kens(2)=5
    kens(3)=0
    selectcase (ipdstmpl(16))
    case(0)
      kens(4)=1
    case(1)
      kens(4)=2
    case(2)
      kens(4)=11
    case(3)
      kens(4)=12
    end select
    !kens(5)=89
    kens(5)=0
    kclust(1)=ipdstmpl(17)
  else
    print *,'makepdsens: Don:t know GRIB2 PDT 4.',ipdsnum
    iret=2
  endif

  return
end subroutine makepdsens

!> This routine converts grid information from a GRIB2 Grid Description
!> Section as well as its Grid Definition Template to GRIB1 GDS info. In
!> addition, a check is made to determine if the grid is an NCEP
!> predefined grid.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-17 | Gilbert | Initial.
!> 2004-04-27 | Gilbert | Added support for gaussian grids.
!> 2007-04-16 | Vuong | Added Curvilinear Orthogonal grids.
!> 2007-05-29 | Vuong | Added Rotate Lat/Lon E-grid (203)
!>
!> @param[in] igds Contains information read from the appropriate GRIB
!> Grid Definition Section 3 for the field being returned. Must be
!> dimensioned >= 5.
!> - igds(1) Source of grid definition (see Code Table 3.0)
!> - igds(2) Number of grid points in the defined grid.
!> - igds(3) Number of octets needed for each additional grid points
!> definition. Used to define number of points in each row (or column)
!> for non-regular grids. = 0, if using regular grid.
!> - igds(4) Interpretation of list for optional points
!> definition. (Code Table 3.11).
!> - igds(5) Grid Definition Template Number (Code Table 3.1)
!> @param[in] igdstmpl Grid Definition Template values for GDT 3.igds(5)
!> @param[in] idefnum The number of entries in array
!> ideflist. i.e. number of rows (or columns) for which optional grid
!> points are defined.
!> @param[in] ideflist Optional integer array containing the number of
!> grid points contained in each row (or column).
!> @param[out] kgds GRIB1 GDS as described in w3fi63 format.
!> @param[out] igrid NCEP predefined GRIB1 grid number
!>                set to 255, if not NCEP grid
!> @param[out] iret Error return value:
!> - 0 Successful
!> - 1 Unrecognized GRIB2 GDT number 3.igds(5).
!>
!> @author Stephen Gilbert @date 2003-06-17 &
subroutine gdt2gds(igds,igdstmpl,idefnum,ideflist,kgds, &
    igrid,iret)

  !
  integer,intent(in) :: idefnum
  integer,intent(in) :: igds(*),igdstmpl(*),ideflist(*)
  integer,intent(out) :: kgds(*),igrid,iret

  integer :: kgds72(200),kgds71(200),idum(200),jdum(200)

  iret=0
  if (igds(5).eq.0) then       !  Lat/Lon grid
    kgds(1)=0
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(13)/1000      ! Long of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5))  &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
    kgds(8)=igdstmpl(16)/1000      ! Long of last grid point
    kgds(9)=igdstmpl(17)/1000      ! Di
    kgds(10)=igdstmpl(18)/1000     ! Dj
    kgds(11)=igdstmpl(19)          ! Scanning mode
    kgds(12)=0
    kgds(13)=0
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
    !
    !  Process irreg grid stuff, if necessary
    !
    if (idefnum.ne.0) then
      if (igdstmpl(8).eq.-1) then
          kgds(2)=65535
          kgds(9)=65535
      endif
      if (igdstmpl(9).eq.-1) then
          kgds(3)=65535
          kgds(10)=65535
      endif
      kgds(19)=0
      kgds(20)=33
      if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
      kgds(21)=igds(2)                   ! num of grid points
      do j=1,idefnum
          kgds(21+j)=ideflist(j)
      enddo
    endif
  elseif (igds(5).eq.10) then       !  Mercator grid
    kgds(1)=1                 ! Grid Definition Template number
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(14)/1000      ! Lat of last grid point
    kgds(8)=igdstmpl(15)/1000      ! Long of last grid point
    kgds(9)=igdstmpl(13)/1000      ! Lat intersects earth
    kgds(10)=0
    kgds(11)=igdstmpl(16)          ! Scanning mode
    kgds(12)=igdstmpl(18)/1000     ! Di
    kgds(13)=igdstmpl(19)/1000     ! Dj
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
  elseif (igds(5).eq.30) then       ! Lambert Conformal Grid
    kgds(1)=3
    kgds(2)=igdstmpl(8)            ! Nx
    kgds(3)=igdstmpl(9)            ! Ny
    kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(14)/1000      ! Lon of orientation
    kgds(8)=igdstmpl(15)/1000      ! Dx
    kgds(9)=igdstmpl(16)/1000      ! Dy
    kgds(10)=igdstmpl(17)          ! Projection Center Flag
    kgds(11)=igdstmpl(18)          ! Scanning mode
    kgds(12)=igdstmpl(19)/1000     ! Lat in 1
    kgds(13)=igdstmpl(20)/1000     ! Lat in 2
    kgds(14)=igdstmpl(21)/1000     ! Lat of S. Pole of projection
    kgds(15)=igdstmpl(22)/1000     ! Lon of S. Pole of projection
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
  elseif (igds(5).eq.40) then       !  Gaussian Lat/Lon grid
    kgds(1)=4
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(13)/1000      ! Long of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5))  &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
    kgds(8)=igdstmpl(16)/1000      ! Long of last grid point
    kgds(9)=igdstmpl(17)/1000      ! Di
    kgds(10)=igdstmpl(18)          ! N - Number of parallels
    kgds(11)=igdstmpl(19)          ! Scanning mode
    kgds(12)=0
    kgds(13)=0
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
  elseif (igds(5).eq.20) then       ! Polar Stereographic Grid
    kgds(1)=5
    kgds(2)=igdstmpl(8)            ! Nx
    kgds(3)=igdstmpl(9)            ! Ny
    kgds(4)=igdstmpl(10)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(11)/1000      ! Long of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5))  &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(12),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(14)/1000      ! Lon of orientation
    kgds(8)=igdstmpl(15)/1000      ! Dx
    kgds(9)=igdstmpl(16)/1000      ! Dy
    kgds(10)=igdstmpl(17)          ! Projection Center Flag
    kgds(11)=igdstmpl(18)          ! Scanning mode
    kgds(12)=0
    kgds(13)=0
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
  elseif (igds(5).eq.204) then      ! Curvilinear Orthogonal
    kgds(1)=204
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=0
    kgds(5)=0
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
    kgds(7)=0
    kgds(8)=0
    kgds(9)=0
    kgds(10)=0
    kgds(11)=igdstmpl(19)          ! Scanning mode
    kgds(12)=0
    kgds(13)=0
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
    !
    !  Process irreg grid stuff, if necessary
    !
    if (idefnum.ne.0) then
      if (igdstmpl(8).eq.-1) then
          kgds(2)=65535
          kgds(9)=65535
      endif
      if (igdstmpl(9).eq.-1) then
          kgds(3)=65535
          kgds(10)=65535
      endif
      kgds(19)=0
      kgds(20)=33
      if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
      kgds(21)=igds(2)                   ! num of grid points
      do j=1,idefnum
          kgds(21+j)=ideflist(j)
      enddo
    endif
  elseif (igds(5).eq.32768) then    ! Rotate Lat/Lon grid
    kgds(1)=203                      ! Arakawa Staggerred E/B grid
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(13)/1000      ! Lon of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
    kgds(8)=igdstmpl(16)/1000      ! Lon of last grid point
    kgds(9)=igdstmpl(17)/1000      ! Di
    kgds(10)=igdstmpl(18)/1000     ! Dj
    kgds(11)=igdstmpl(19)          ! Scanning mode
    kgds(12)=0
    kgds(13)=0
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
    !
    !  Process irreg grid stuff, if necessary
    !
    if (idefnum.ne.0) then
      if (igdstmpl(8).eq.-1) then
          kgds(2)=65535
          kgds(9)=65535
      endif
      if (igdstmpl(9).eq.-1) then
          kgds(3)=65535
          kgds(10)=65535
      endif
      kgds(19)=0
      kgds(20)=33
      if (kgds(1).eq.1.OR.kgds(1).eq.3) kgds(20)=43
      kgds(21)=igds(2)                   ! num of grid points
      do j=1,idefnum
          kgds(21+j)=ideflist(j)
      enddo
    endif
  elseif (igds(5).eq.32769) then    ! Rotate Lat/Lon grid
    kgds(1)=205                    ! Arakawa Staggerred for Non-E Stagger grid
    kgds(2)=igdstmpl(8)            ! Ni
    kgds(3)=igdstmpl(9)            ! Nj
    kgds(4)=igdstmpl(12)/1000      ! Lat of 1st grid point
    kgds(5)=igdstmpl(13)/1000      ! Lon of 1st grid point
    kgds(6)=0                      ! resolution and component flags
    if (igdstmpl(1)==2) kgds(6)=64
    if (btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5)) &
        kgds(6)=kgds(6)+128
    if (btest(igdstmpl(14),3)) kgds(6)=kgds(6)+8
    kgds(7)=igdstmpl(15)/1000      ! Lat of last grid point
    kgds(8)=igdstmpl(16)/1000      ! Lon of last grid point
    kgds(9)=igdstmpl(17)/1000      ! Di
    kgds(10)=igdstmpl(18)/1000     ! Dj
    kgds(11)=igdstmpl(19)          ! Scanning mode
    kgds(12)=igdstmpl(20)/1000
    kgds(13)=igdstmpl(21)/1000
    kgds(14)=0
    kgds(15)=0
    kgds(16)=0
    kgds(17)=0
    kgds(18)=0
    kgds(19)=0
    kgds(20)=255
    kgds(21)=0
    kgds(22)=0
  else
    Print *,'gdt2gds: Unrecognized GRIB2 GDT = 3.',igds(5)
    iret=1
    kgds(1:22)=0
    return
  endif
  !
  !   Can we determine NCEP grid number ?
  !
  igrid=255
  do j=254,1,-1
    !do j=225,225
    kgds71=0
    kgds72=0
    call w3fi71(j,kgds71,ierr)
    if (ierr.ne.0) cycle
    ! convert W to E for longitudes
    if (kgds71(3).eq.0) then    ! lat/lon
      if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
      if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
    elseif (kgds71(3).eq.1) then    ! mercator
      if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
      if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
    elseif (kgds71(3).eq.3) then     ! lambert conformal
      if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
      if (kgds71(9).lt.0) kgds71(9)=360000+kgds71(9)
      if (kgds71(18).lt.0) kgds71(18)=360000+kgds71(18)
    elseif (kgds71(3).eq.4) then     ! Guassian lat/lon
      if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
      if (kgds71(10).lt.0) kgds71(10)=360000+kgds71(10)
    elseif (kgds71(3).eq.5) then     ! polar stereographic
      if (kgds71(7).lt.0) kgds71(7)=360000+kgds71(7)
      if (kgds71(9).lt.0) kgds71(9)=360000+kgds71(9)
    endif
    call r63w72(idum,kgds,jdum,kgds72)
    if (kgds72(3).eq.3) kgds72(14)=0    ! lambert conformal fix
    if (kgds72(3).eq.1) kgds72(15:18)=0    ! mercator fix
    if (kgds72(3).eq.5) kgds72(14:18)=0    ! polar str fix
    !           print *,' kgds71(',j,')= ', kgds71(1:30)
    !           print *,' kgds72       = ', kgds72(1:30)
    if (all(kgds71.eq.kgds72)) then
      igrid=j
      return
    endif
  enddo

  return
end subroutine gdt2gds

!> Pack and write a grib message. This subprogram is nearly the inverse
!> of getgbe.
!>
!> @note Subprogram can be called from a multiprocessing environment.
!> Do not engage the same logical unit from more than one processor.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 94-04-01 | Iredell | Initial.
!> 95-10-31 | Iredell | Removed saves and prints
!> 97-02-11 | Y. Zhu | Included probability and cluster arguments
!> 2002-03-18 | Gilbert | Modified from putgbex to account for binary scale factors.
!>
!> @param[in] lugb teger unit of the unblocked grib data file
!> @param[in] kf teger number of data points
!> @param[in] kpds teger (200) pds parameters
!> - 1 id of center
!> - 2 generating process id number
!> - 3 grid definition
!> - 4 gds/bms flag (right adj copy of octet 8)
!> - 5 indicator of parameter
!> - 6 type of level
!> - 7 height/pressure , etc of level
!> - 8 year including (century-1)
!> - 9 month of year
!> - 10 day of month
!> - 11 hour of day
!> - 12 minute of hour
!> - 13 indicator of forecast time unit
!> - 14 time range 1
!> - 15 time range 2
!> - 16 time range flag
!> - 17 number included in average
!> - 18 version nr of grib specification
!> - 19 version nr of parameter table
!> - 20 nr missing from average/accumulation
!> - 21 century of reference time of data
!> - 22 units decimal scale factor
!> - 23 subcenter number
!> - 24 pds byte 29, for nmc ensemble products, 128 if forecast field
!> error, 64 if bias corrected fcst field, 32 if smoothed field,
!> warning: can be combination of more than 1.
!> - 25 pds byte 30, not used
!> @param[in] kgds teger (200) gds parameters
!> - 1 data representation type
!> - 19 number of vertical coordinate parameters
!> - 20 octet number of the list of vertical coordinate parameters or
!> octet number of the list of numbers of points in each row or 255 if
!> neither are present.
!> - 21 for grids with pl, number of points in grid
!> - 22 number of words in each row latitude/longitude grids
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag (right adj copy of octet 17)
!> - 7 la(2) latitude of extreme point
!> - 8 lo(2) longitude of extreme point
!> - 9 di longitudinal direction of increment
!> - 10 dj latitudinal direction increment
!> - 11 scanning mode flag (right adj copy of octet 28)
!> Gaussian  grids:
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag  (right adj copy of octet 17)
!> - 7 la(2) latitude of extreme point
!> - 8 lo(2) longitude of extreme point
!> - 9 di longitudinal direction of increment
!> - 10 n - nr of circles pole to equator
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 nv - nr of vert coord parameters
!> - 13 pv - octet nr of list of vert coord parameters or pl - location
!> of the list of numbers of points in each row (if no vert coord
!> parameters are present or 255 if neither are present
!> Polar Stereographic grids:
!> - 2 n(i) nr points along lat circle
!> - 3 n(j) nr points along lon circle
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag  (right adj copy of octet 17)
!> - 7 lov grid orientation
!> - 8 dx - x direction increment
!> - 9 dy - y direction increment
!> - 10 projection center flag
!> - 11 scanning mode (right adj copy of octet 28)
!> Spherical Harmonic Coefficients:
!> - 2 j pentagonal resolution parameter
!> - 3 k pentagonal resolution parameter
!> - 4 m pentagonal resolution parameter
!> - 5 representation type
!> - 6 coefficient storage mode
!> Mercator grids:
!> - 2 n(i) nr points on latitude circle
!> - 3 n(j) nr points on longitude meridian
!> - 4 la(1) latitude of origin
!> - 5 lo(1) longitude of origin
!> - 6 resolution flag (right adj copy of octet 17)
!> - 7 la(2) latitude of last grid point
!> - 8 lo(2) longitude of last grid point
!> - 9 latit - latitude of projection intersection
!> - 10 reserved
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 longitudinal dir grid length
!> - 13 latitudinal dir grid length
!> Lambert Conformal Grids:
!> - 2 nx nr points along x-axis
!> - 3 ny nr points along y-axis
!> - 4 la1 lat of origin (lower left)
!> - 5 lo1 lon of origin (lower left)
!> - 6 resolution (right adj copy of octet 17)
!> - 7 lov - orientation of grid
!> - 8 dx - x-dir increment
!> - 9 dy - y-dir increment
!> - 10 projection center flag
!> - 11 scanning mode flag (right adj copy of octet 28)
!> - 12 latin 1 - first lat from pole of secant cone inter
!> - 13 latin 2 - second lat from pole of secant cone inter
!> @param[in] kens teger (200) ensemble pds parms
!> - 1 application identifier
!> - 2 ensemble type
!> - 3 ensemble identifier
!> - 4 product identifier
!> - 5 smoothing flag
!> @param[in] kprob teger (2) probability ensemble parms
!> @param[in] xprob al    (2) probability ensemble parms
!> @param[in] kclust teger (16) cluster ensemble parms
!> @param[in] kmembr teger (8) cluster ensemble parms
!> @param[in] ibs teger binary scale factor (0 to ignore)
!> @param[in] nbits teger number of bits in which to pack (0 to ignore)
!> @param[in] lb gical*1 (kf) bitmap if present
!> @param[in] f al (kf) data
!> @param[out] iret teger return code
!> - 0 Success
!> - Other W3FI72 GRIB packer return code
!>
!> @author Mark Iredell @date 94-04-01
SUBROUTINE PUTGBEXN(LUGB,KF,KPDS,KGDS,KENS, &
    KPROB,XPROB,KCLUST,KMEMBR,IBS,NBITS,LB,F,IRET)

  INTEGER KPDS(200),KGDS(200),KENS(200)
  INTEGER KPROB(2),KCLUST(16),KMEMBR(80)
  REAL XPROB(2)
  LOGICAL*1 LB(KF)
  REAL F(KF)
  !      PARAMETER(MAXBIT=16)
  PARAMETER(MAXBIT=24)
  INTEGER IBM(KF),IPDS(200),IGDS(200),IBDS(200)
  CHARACTER PDS(400),GRIB(1000+KF*(MAXBIT+1)/8)

  !  GET W3FI72 PARAMETERS
  !print *,'SAGT: start putgbexn'
  CALL R63W72(KPDS,KGDS,IPDS,IGDS)
  IBDS=0

  !  COUNT VALID DATA
  KBM=KF
  IF(IPDS(7).NE.0) THEN
    KBM=0
    DO I=1,KF
      IF(LB(I)) THEN
          IBM(I)=1
          KBM=KBM+1
      ELSE
          IBM(I)=0
      ENDIF
    ENDDO
    IF(KBM.EQ.KF) IPDS(7)=0
  ENDIF

  !  GET NUMBER OF BITS AND ROUND DATA
  IF(NBITS.GT.0) THEN
    NBIT=NBITS
  ELSE
    IF(KBM.EQ.0) THEN
      DO I=1,KF
          F(I)=0.
      ENDDO
      NBIT=0
    ELSE
      !print *,'SAGT:',IPDS(7),IBS,IPDS(25),KF
      !print *,'SAGT:',count(ibm.eq.0),count(ibm.eq.1)
      CALL SETBIT(IPDS(7),-IBS,IPDS(25),KF,IBM,F,FMIN,FMAX,NBIT)
      NBIT=MIN(NBIT,MAXBIT)
    ENDIF
  ENDIF

  !  CREATE PRODUCT DEFINITION SECTION
  CALL W3FI68(IPDS,PDS)
  IF(IPDS(24).EQ.2) THEN
    ILAST=45
    IF (IPDS(8).EQ.191.OR.IPDS(8).EQ.192) ILAST=55
    IF (KENS(2).EQ.5) ILAST=76
    IF (KENS(2).EQ.5) ILAST=86
    IF (KENS(2).EQ.4) ILAST=86
    CALL PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
  ENDIF

  !  PACK AND WRITE GRIB DATA
  igflag=1
  igrid=kpds(3)
  if (igrid.ne.255) igflag=0
  !print *,minval(f(1:kf)),maxval(f(1:kf))
  !print *,nbit,kf
  !print *,(ipds(j),j=1,28)
  !write(6,fmt='(28z2)') (pds(j),j=1,28)
  !print *,(kgds(j),j=1,28)
  !print *,(igds(j),j=1,28)
  icomp=0
  CALL W3FI72(0,F,0,NBIT,1,IPDS,PDS, &
      igflag,igrid,IGDS,ICOMP,0,IBM,KF,IBDS, &
      KFO,GRIB,LGRIB,IRET)
  IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)

  RETURN
END SUBROUTINE PUTGBEXN

!> The number of bits required to pack a given field for particular
!> binary and decimal scalings is computed. The minimum and maximum
!> rounded field values are also returned. GRIB bitmap masking for
!> valid data is optionally used.
!>
!> @param[in] ibm integer bitmap flag (=0 for no bitmap).
!> @param[in] ibs integer binary scaling (e.g. ibs=3 to round field to
!> nearest eighth value).
!> @param[in] ids integer decimal scaling (e.g. ids=3 to round field to
!> nearest milli-value) (note that ids and ibs can both be nonzero,
!> e.g. ids=1 and ibs=1 rounds to the nearest twentieth).
!> @param[in] len integer length of the field and bitmap.
!> @param[in] mg integer (len) bitmap if ibm=1 (0 to skip, 1 to keep).
!> @param[in] g real (len) field.
!> @param[out] gmin real minimum valid rounded field value.
!> @param[out] gmax real maximum valid rounded field value.
!> @param[out] nbit integer number of bits to pack.
!>
!> @author Mark Iredell @date 92-10-31
SUBROUTINE SETBIT(IBM,IBS,IDS,LEN,MG,G,GMIN,GMAX,NBIT)

  DIMENSION MG(LEN),G(LEN)

  !  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
  S=2.**IBS*10.**IDS
  IF(IBM.EQ.0) THEN
     GMAX=G(1)
     GMIN=G(1)
     DO I=2,LEN
        GMAX=MAX(GMAX,G(I))
        GMIN=MIN(GMIN,G(I))
     ENDDO
  ELSE
     I1=1
     DO WHILE(I1.LE.LEN.AND.MG(I1).EQ.0)
        I1=I1+1
     ENDDO
     IF(I1.LE.LEN) THEN
        DO I=1,I1-1
           G(I)=0.
        ENDDO
        GMAX=G(I1)
        GMIN=G(I1)
        DO I=I1+1,LEN
           IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
           ELSE
              G(I)=0.
           ENDIF
        ENDDO
     ELSE
        DO I=1,LEN
           G(I)=0.
        ENDDO
        GMAX=0.
        GMIN=0.
     ENDIF
  ENDIF

  !  COMPUTE NUMBER OF BITS
  NBIT=LOG((GMAX-GMIN)*S+0.9)/LOG(2.)+1.

  RETURN
END SUBROUTINE SETBIT
