!> @file
!> @brief Make an inventory of a GRIB2 file.
!> @author Stephen Gilbert @date 2010-09-08

!> This program reads a GRIB2 file and makes an inventory.
!>
!> Usage:
!> degrib2 [filename]
!>
!> @note Command line can have only one file name.
!>
!> @return 0 for success.
!> @author Stephen Gilbert @date 2010-09-08
program degrib2
  use grib_mod
  use params
  implicit none

  integer :: msk2, icount, ifl1, itot, j, lengrib, lgrib
  integer*8 :: iseek8, msk18, lskip8, lgrib8, lengrib8
  integer :: maxlocal, n, ncgb, numfields, numlocal
  real :: fldmax, fldmin, sum
  parameter(msk18 = 32000, msk2 = 4000)
  character(len = 1), allocatable, dimension(:) :: cgrib
  integer :: listsec0(3)
  integer :: listsec1(13)
  character(len = 250) :: gfile1
  character(len = 8) :: pabbrev
  character(len = 40) :: labbrev
  character(len = 110) :: tabbrev
  integer(4) narg, iargc, temparg
  integer :: currlen = 0,  numpts = 0
  logical :: unpack, expand
  type(gribfield) :: gfld
  integer :: ierr, ios, is
  
  call start()
  unpack = .true.
  expand = .false.

  ! Get arguments.
  narg = iargc()
  if (narg .ne. 1) then
     call errmsg('degrib2:  incorrect usage')
     call errmsg('usage: degrib2 grib2file')
     call errexit(2)
  endif

  ! Open the input file with the bacio library.
  ifl1 = 10
  temparg = 1
  call getarg(temparg, gfile1)
  ncgb = len_trim(gfile1)
  call baopenr(ifl1, gfile1(1:ncgb), ios)

  itot = 0
  icount = 0
  iseek8 = 0
  do
     ! Find a GRIB2 message in the file.
     call skgb8(ifl1, iseek8, msk18, lskip8, lgrib8)
     lgrib = lgrib8
     if (lgrib8 .eq. 0) exit    ! end loop at EOF or problem

     ! Read the GRIB2 message from the file.
     if (lgrib8 .gt. currlen) then
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(lgrib8), stat = is)
        currlen = lgrib8
     endif
     call bareadl(ifl1, lskip8, lgrib8, lengrib8, cgrib)
     lengrib = lengrib8
     if (lgrib8 .ne. lengrib) then
        write(6, *)' degrib2: IO Error.'
        call errexit(9)
     endif
     iseek8 = lskip8 + lgrib8
     icount = icount + 1
     write (6, *)
     write(6, '(A,I0,A,I0)') ' GRIB MESSAGE  ', icount, '  starts at ', lskip8 + 1
     write (6, *)

     ! Get info about the message.
     call gb_info(cgrib, lengrib, listsec0, listsec1,  &
          numfields, numlocal, maxlocal, ierr)
     if (ierr .ne. 0) then
        write(6, '(A,I0)') ' ERROR extracting field = ', ierr
        stop 10
     endif
     itot = itot + numfields
     write(6, '(A,3(1x,I0))')'  SECTION 0: ', (listsec0(j), j = 1, 3)
     write(6, '(A,13(1x,I0))')'  SECTION 1: ', (listsec1(j), j = 1, 13)
     write(6, '(A,1x,I0,1x,A,I0,1x,A)') '  Contains ', numlocal,  &
          ' Local Sections  and  ', numfields, ' data fields.'

     ! Read each field in the message.
     do n = 1, numfields
        ! Unpack GRIB2 field.
        call gf_getfld(cgrib, lengrib, n, unpack, expand, gfld, ierr)
        if (ierr .ne. 0) then
           write(6, '(A,I0)') ' ERROR extracting field = ', ierr
           cycle
        endif

        write (6, *)
        write(6, '(A,1x,I0)')'  FIELD ', n
        if (n .eq. 1) then
           write(6, '(A,2(1x,I0))')'  SECTION 0: ', gfld%discipline, gfld%version
           write(6, '(A,20(1x,I0))')'  SECTION 1: ', (gfld%idsect(j), j = 1, gfld%idsectlen)
        endif
        if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
           write(6, '(A,I0,A)')'  SECTION 2: ', gfld%locallen, ' bytes'
        endif
        write(6, '(A,5(1x,I0))')'  SECTION 3: ', gfld%griddef, gfld%ngrdpts,  gfld%numoct_opt, &
             gfld%interp_opt, gfld%igdtnum
        write(6, '(A,1x,I0,A,100(1x,I0))')'  GRID TEMPLATE 3.',  &
             gfld%igdtnum, ' : ',  (gfld%igdtmpl(j), j = 1, gfld%igdtlen)
        if (gfld%num_opt .eq. 0) then
           write(6, *)' NO Optional List Defining Number of Data Points.'
        else
           write(6, '(A,1x,150(1x,I0))')'  Section 3 Optional List:',  &
                (gfld%list_opt(j), j = 1, gfld%num_opt)
        endif

        pabbrev = param_get_abbrev(gfld%discipline, gfld%ipdtmpl(1), gfld%ipdtmpl(2))
        call prlevel(gfld%ipdtnum, gfld%ipdtmpl, labbrev)
        call prvtime(gfld%ipdtnum, gfld%ipdtmpl, listsec1, tabbrev)

        write(6,'(A,1x,I0,A,A,3(1X,I0),A,80(1x,I0))') '  PRODUCT TEMPLATE 4.',  gfld%ipdtnum, &
             ': ( PARAMETER = ', pabbrev, gfld%discipline, gfld%ipdtmpl(1), gfld%ipdtmpl(2), ' ) ', &
             (gfld%ipdtmpl(j), j = 1, gfld%ipdtlen)

        write(6, '(A,A,A,A,A)')'  FIELD: ', pabbrev, trim(labbrev), " ", trim(tabbrev)
        if (gfld%num_coord .eq. 0) then
           write(6, *)' NO Optional Vertical Coordinate List.'
        else
           write(6, '(A,1X,150(1x,I0))') '  Section 4 Optional & Coordinates: ', &
                (gfld%coord_list(j), j = 1, gfld%num_coord)
        endif
        if (gfld%ibmap .ne. 255) then
           write(6, '(A,I0,A,I0)')'  Num. of Data Points =  ', &
                gfld%ndpts, '    with BIT-MAP  ', gfld%ibmap
        else
           write(6, '(A,I0,A)')'  Num. of Data Points =  ', gfld%ndpts, '     NO BIT-MAP '
        endif
        write(6, '(A,I0,A,20(1x,I0))')'  DRS TEMPLATE 5. ', gfld%idrtnum, ' : ', &
             (gfld%idrtmpl(j), j = 1, gfld%idrtlen)
        if (gfld%ndpts .eq. 0) then
           fldmax = 0.0
           fldmin = 0.0
           numpts = 1
           sum = 0.0
        else
           if (gfld%fld(1) .eq. 9.9990003E+20) then  ! checking undefined values
              fldmax = 0.0
              fldmin = 99999.99
              sum = 0.0
              numpts = 0
           else
              fldmax = gfld%fld(1)
              fldmin = gfld%fld(1)
              sum = gfld%fld(1)
              numpts = 1
           endif
           do j = 2, gfld%ndpts
              if (gfld%fld(j) .eq. 9.9990003E+20) then ! checking undefined values
                 cycle
              end if
              if (gfld%fld(j) .gt. fldmax) fldmax = gfld%fld(j)
              if (gfld%fld(j) .lt. fldmin) fldmin = gfld%fld(j)
              sum = sum + gfld%fld(j)
              numpts = numpts + 1
           enddo
        endif

        write(6, *)' Data Values:'
        write(6, '(A,I0,A,I0)')'  Num. of Data Points =  ', &
             gfld%ndpts, '   Num. of Data Undefined = ', gfld%ndpts-numpts
        !print *, trim(pabbrev), fldmin, sum / numpts, fldmax
        write(6, fmt = '( "( PARM= ",A," ) : ", " MIN=",f25.8," AVE=",f25.8, " MAX=",f25.8)') &
             trim(pabbrev), fldmin, sum / numpts, fldmax

        ! Free memory allocated to hold field.
        call gf_free(gfld)
     enddo
  enddo
  write(6, *)" "
  write(6, '(A,I0)')'  Total Number of Fields Found =  ', itot
  if (allocated(cgrib)) deallocate(cgrib)  
end program degrib2

!> Print level information to a character array, given the GRIB2
!> Product Definition Template information.
!>
!> This subroutine finds the "Type of first fixed surface" (see [Code
!> table
!> 4.5](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp4-0.shtml))
!> using the product definition template value. The location in the
!> template array varies depending on the template number.
!>
!> @param[in] ipdtn Product Definition Template Number ([Code Table
!> 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[in] ipdtmpl Array of data values for the Product Definition
!> Template specified by ipdtn.
!> @param[out] labbrev Character array which gets the string
!> describing the level. Must be of length 40.
!>
!> @author Stephen Gilbert @date 2010-09-08
subroutine prlevel(ipdtn, ipdtmpl, labbrev)
  implicit none
  
  integer, intent(in) :: ipdtn
  integer, intent(in) :: ipdtmpl(*)
  character(len = 40), intent(out) :: labbrev
  integer :: ipos
  character(len = 10) :: tmpval1, tmpval2

  labbrev(1:40) = " "

  ! Use the template number to determine which element of the product
  ! template array holds the "Type of first fixed surface" value
  ! (which will be stored in ipos).
  selectcase (ipdtn)
  case(0:15)
     ipos = 10
  case(40:43)
     ipos = 11
  case(44:47)
     ipos = 16
  case(48)
     ipos = 21
  case(50:51)
     ipos = 10
  case(52)
     ipos = 13
  case(91)
     ipos = 10
  case default
     ipos = 10
  end select

  ! Construct level description based on "Type of first fixed surface"
  ! value and some other values from the template.
  if (ipdtmpl(ipos) .eq. 100 .and.          & ! Pressure Level
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1) + 2)
     labbrev = trim(tmpval1)//" mb"
  elseif (ipdtmpl(ipos) .eq. 100 .and.          & ! Pressure Layer
       ipdtmpl(ipos + 3) .eq. 100) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1) + 2)
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4) + 2)
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" mb"
  elseif (ipdtmpl(ipos) .eq. 101) then       ! Mean Sea Level
     labbrev(1:30) = " Mean Sea Level "
  elseif (ipdtmpl(ipos) .eq. 102 .and.       & ! Altitude above MSL
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     labbrev = trim(tmpval1)//" m above MSL"
  elseif (ipdtmpl(ipos) .eq. 103 .and.       & ! Height above Ground
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     labbrev = trim(tmpval1)//" m above ground"
  elseif (ipdtmpl(ipos) .eq. 103 .and.       & ! Height above Ground
       ipdtmpl(ipos + 3) .eq. 103) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4))
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" m AGL"
  elseif (ipdtmpl(ipos) .eq. 104 .and.       & ! Sigma Level
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     labbrev = trim(tmpval1)//" sigma"
  elseif (ipdtmpl(ipos) .eq. 104 .and.       & ! Sigma Layer
       ipdtmpl(ipos + 3) .eq. 104) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4))
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" sigma"
  elseif (ipdtmpl(ipos) .eq. 105 .and.       & ! Hybrid Level
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     labbrev = trim(tmpval1)//" hybrid lvl"
  elseif (ipdtmpl(ipos).eq.105 .and.         & ! Hybrid Level
       ipdtmpl(ipos + 3).eq.105) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4))
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" hybrid lvl"
  elseif (ipdtmpl(ipos) .eq. 106 .and.       & ! Depth Below Land Sfc
       ipdtmpl(ipos + 3) .eq. 255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     labbrev = trim(tmpval1)//" m below land"
  elseif (ipdtmpl(ipos).eq.106 .and.         & ! Depth Below Land Sfc Layer
       ipdtmpl(ipos + 3).eq.106) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1))
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4))
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" m DBLY"
  elseif (ipdtmpl(ipos) .eq. 107) then       ! Isentrophic (theta) level (THEL)
     labbrev(1:30) = " Isentropic level"
  elseif (ipdtmpl(ipos).eq.108 .and.         & ! Press Diff from Ground Layer
       ipdtmpl(ipos + 3).eq.108) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1) + 2)
     call frmt(tmpval2, ipdtmpl(ipos + 5), ipdtmpl(ipos + 4) + 2)
     labbrev = trim(tmpval1)//" - "//trim(tmpval2)//" mb SPDY"
  elseif (ipdtmpl(ipos) .eq. 110) then       ! Layer between two hybrid levels (HYBY)
     labbrev(1:30) = " Layer bet 2-hyb lvl"
  elseif (ipdtmpl(ipos).eq.109 .and.         & ! Potential Vorticity Sfc
       ipdtmpl(ipos + 3).eq.255) then
     call frmt(tmpval1, ipdtmpl(ipos + 2), ipdtmpl(ipos + 1)-6)
     labbrev = trim(tmpval1)//" pv surface"
  elseif (ipdtmpl(ipos) .eq. 111) then       ! Eta Level (EtaL)
     labbrev(1:30) = " Eta level"
  elseif (ipdtmpl(ipos) .eq. 114) then       ! Layer between two isentropic levels (THEY)
     labbrev(1:30) = " Layer bet. 2-isent."
  elseif (ipdtmpl(ipos) .eq. 117) then       ! Mixed layer depth
     labbrev(1:30) = " Mixed layer depth"
  elseif (ipdtmpl(ipos) .eq. 120) then       ! Layer between two Eta levels (EtaY)
     labbrev(1:30) = " Layer bet. 2-Eta lvl"
  elseif (ipdtmpl(ipos) .eq. 121) then       ! Layer between two isobaric surface (IBYH)
     labbrev(1:30) = " Layer bet. 2-isob."
  elseif (ipdtmpl(ipos) .eq. 125) then       ! Specified height level above ground  (HGLH)
     labbrev(1:30) = " Specified height lvl"
  elseif (ipdtmpl(ipos) .eq. 126) then       ! Isobaric Level (ISBP)
     labbrev(1:30) = " Isobaric level"
  elseif (ipdtmpl(ipos) .eq. 160) then       ! Depth below sea level
     labbrev(1:30) = " Depth below sea lvl"
  elseif (ipdtmpl(ipos) .eq. 170) then       ! Ionospheric D-region
     labbrev(1:30) = " Ionospheric D-region lvl"
  elseif (ipdtmpl(ipos) .eq. 1) then         ! Ground/Water Surface
     labbrev(1:30) = " Surface "
  elseif (ipdtmpl(ipos) .eq. 2) then         ! Cloud base level (CBL)
     labbrev(1:30) = " Cloud base lvl"
  elseif (ipdtmpl(ipos) .eq. 3) then         ! Cloud top level (CTL)
     labbrev(1:30) = " Cloud top lvl"
  elseif (ipdtmpl(ipos) .eq. 4) then         ! Freezing Level
     labbrev(1:30) = " 0 Deg Isotherm"
  elseif (ipdtmpl(ipos) .eq. 5) then         ! Level of adiabatic condensation lifted
     labbrev(1:30) = " Level of adiabatic"     ! from the surface
  elseif (ipdtmpl(ipos) .eq. 6) then         ! Max Wind Level
     labbrev(1:30) = " Max wind lvl"
  elseif (ipdtmpl(ipos) .eq. 7) then         ! Tropopause
     labbrev(1:30) = " Tropopause"
  elseif (ipdtmpl(ipos) .eq. 8) then         ! Nominal top of Atmosphere
     labbrev(1:30) = " Nom. top"
  elseif (ipdtmpl(ipos) .eq. 9) then         !  Sea bottom
     labbrev(1:30) = " Sea Bottom"
  elseif (ipdtmpl(ipos) .eq. 10) then        ! Entire Atmosphere (EATM)
     labbrev(1:30) = " Entire Atmosphere"
  elseif (ipdtmpl(ipos) .eq. 11) then        ! Cumulonimbus Base
     labbrev(1:30) = " Cumulonimbus Base"
  elseif (ipdtmpl(ipos) .eq. 12) then        ! Cumulonimbus Top
     labbrev(1:30) = " Cumulonimbus Top"
  elseif (ipdtmpl(ipos) .eq. 20) then        ! Isothermal level
     labbrev(1:30) = " Isothermal level"
  elseif (ipdtmpl(ipos) .eq. 200) then       ! Entire Atmosphere (EATM)
     labbrev(1:30) = " Entire Atmosphere"
  elseif (ipdtmpl(ipos) .eq. 201) then       ! Entire ocean (EOCN)
     labbrev(1:30) = " Entire ocean"
  elseif (ipdtmpl(ipos) .eq. 204) then       ! Highest tropospheric freezing level (HTFL)
     labbrev(1:30) = " Highest Frz. lvl"
  elseif (ipdtmpl(ipos) .eq. 206) then       ! Grid scale cloud bottom level (GCBL)
     labbrev(1:30) = " Grid scale cloud bl"
  elseif (ipdtmpl(ipos) .eq. 207) then       ! Grid scale cloud top level (GCTL)
     labbrev(1:30) = " Grid scale cloud tl"
  elseif (ipdtmpl(ipos) .eq. 209) then       ! Boundary layer cloud bottom level (BCBL)
     labbrev(1:30) = " Boundary layer cbl"
  elseif (ipdtmpl(ipos) .eq. 210) then       ! Boundary layer cloud top level (BCTL)
     labbrev(1:30) = " Boundary layer ctl"
  elseif (ipdtmpl(ipos) .eq. 211) then       ! Boundary layer cloud layer (BCY)
     labbrev(1:30) = " Boundary layer cl"
  elseif (ipdtmpl(ipos) .eq. 212) then       ! Low cloud bottom level (LCBL)
     labbrev(1:30) = " Low cloud bot. lvl"
  elseif (ipdtmpl(ipos) .eq. 213) then       ! Low cloud top level (LCTL)
     labbrev(1:30) = " Low cloud top lvl"
  elseif (ipdtmpl(ipos) .eq. 214) then       ! Low cloud layer (LCY)
     labbrev(1:30) = " Low cloud layer"
  elseif (ipdtmpl(ipos) .eq. 215) then       ! Cloud ceiling (CEIL)
     labbrev(1:30) = " Cloud ceiling"
  elseif (ipdtmpl(ipos) .eq. 220) then       ! Planetary Boundary Layer (PBLRI)
     labbrev(1:30) = " Planetary boundary"
  elseif (ipdtmpl(ipos) .eq. 221) then       ! Layer Between Two Hybrid Levels (HYBY)
     labbrev(1:30) = " Layer 2 Hybrid lvl "
  elseif (ipdtmpl(ipos) .eq. 222) then       ! Middle cloud bottom level (MCBL)
     labbrev(1:30) = " Mid. cloud bot. lvl"
  elseif (ipdtmpl(ipos) .eq. 223) then       ! Middle cloud top level (MCTL)
     labbrev(1:30) = " Mid. cloud top lvl"
  elseif (ipdtmpl(ipos) .eq. 224) then       ! Middle cloud layer (MCY)
     labbrev(1:30) = " Middle cloud layer"
  elseif (ipdtmpl(ipos) .eq. 232) then       ! High cloud bottom level (HCBL)
     labbrev(1:30) = " High cloud bot. lvl"
  elseif (ipdtmpl(ipos) .eq. 233) then       ! High cloud top level (HCTL)
     labbrev(1:30) = " High cloud top lvl"
  elseif (ipdtmpl(ipos) .eq. 234) then       ! High cloud layer (HCY)
     labbrev(1:30) = " High cloud layer"
  elseif (ipdtmpl(ipos) .eq. 235) then       ! Ocean isotherm level (OITL)
     labbrev(1:30) = " Ocean Isotherm lvl"
  elseif (ipdtmpl(ipos) .eq. 236) then       ! Layer between two depth below ocean sfc (OLYR)
     labbrev(1:30) = " Layer 2-depth below"
  elseif (ipdtmpl(ipos) .eq. 237) then       ! Bottom of Ocean mixed layer (OBML)
     labbrev(1:30) = " Bot. Ocean mix. lyr"
  elseif (ipdtmpl(ipos) .eq. 238) then       ! Bottom of Ocean iisothermal layer (OBIL)
     labbrev(1:30) = " Bot. Ocean iso. lyr"
  elseif (ipdtmpl(ipos) .eq. 239) then       ! Layer ocean surface and 26C ocean
     labbrev(1:30) = " layer ocean sfc 26C"    ! isothermal level (S26CY)
  elseif (ipdtmpl(ipos) .eq. 240) then       ! Ocean Mixed Layer
     labbrev(1:30) = " Ocean Mixed Layer"
  elseif (ipdtmpl(ipos) .eq. 241) then       ! Ordered Sequence of Data
     labbrev(1:30) = " Order Seq. Of Data"
  elseif (ipdtmpl(ipos) .eq. 242) then       ! Convective cloud bottom level (CCBL)
     labbrev(1:30) = " Con. cloud bot. lvl"
  elseif (ipdtmpl(ipos) .eq. 243) then       ! Convective cloud top level (CCTL)
     labbrev(1:30) = " Con. cloud top lvl"
  elseif (ipdtmpl(ipos) .eq. 244) then       ! Convective cloud layer (CCY)
     labbrev(1:30) = " Conv. cloud layer"
  elseif (ipdtmpl(ipos) .eq. 245) then       ! Lowest level of the wet bulb zero (LLTW)
     labbrev(1:30) = " Lowest lvl wet bulb"
  elseif (ipdtmpl(ipos) .eq. 246) then       ! Maximum equiv. potential temp. level (MTHE)
     labbrev(1:30) = " Max. equi. potential"
  elseif (ipdtmpl(ipos) .eq. 247) then       ! Equilibrium level (EHLT)
     labbrev(1:30) = " Equilibrium level"
  elseif (ipdtmpl(ipos) .eq. 248) then       ! Shallow convective cloud bottom level (SCBL)
     labbrev(1:30) = " Shallow con. cld bl"
  elseif (ipdtmpl(ipos) .eq. 249) then       ! Shallow convective cloud top level (SCTL)
     labbrev(1:30) = " Shallow con. cld tl"
  elseif (ipdtmpl(ipos) .eq. 251) then       ! Deep convective cloud bottom level (DCBL)
     labbrev(1:30) = " Deep conv. cld bl"
  elseif (ipdtmpl(ipos) .eq. 252) then       ! Deep convective cloud top level (DCTL)
     labbrev(1:30) = " Deep conv. cld tl"
  elseif (ipdtmpl(ipos) .eq. 253) then       ! Lowest bottom level of supercooled
     labbrev(1:30) = " Lowest bot. lvl sup"    ! liquid water layer (LBLSW)
  elseif (ipdtmpl(ipos) .eq. 254) then       ! Highest top level of supercooled
     labbrev(1:30) = " highest top lvl sup"    ! liquid water layer (HBLSW)
  else
     write(labbrev, fmt = '(1x,I4," (Unknown Lvl)")') ipdtmpl(ipos)
  endif

end subroutine prlevel

!> Format the level description.
!>
!> @param[in] cval char array that gets the level description.
!> @param[in] ival value to be used in level description.
!> @param[out] iscal scaling factor (if any) for ival.
!>
!> @author Vuong @date 2010-09-08
subroutine frmt(cval, ival, iscal)
  implicit none

  character(len = 10), intent(out) :: cval
  integer, intent(in) :: ival, iscal

  real :: rval
  integer :: newscal
  character(len = 7) :: cformat

  if  (iscal .eq. 0) then
     write(cval, '(I0)') ival
  else
     newscal = -1 * iscal
     rval = real(ival) * (10.0**newscal)
     if (rval .eq. real(nint(rval))) then
        write(cval, '(1X,I0)') nint(rval)
     else
        write(cformat, fmt = '("(f0.",I1,")")') iabs(iscal)
        write(cval, fmt = cformat) rval
     endif
  endif

  return
end subroutine frmt

!> Convert date and time from GRIB2 info to string output.
!>
!> @param[in] ipdtn Product Definition Template Number ([Code Table
!> 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[in] ipdtmpl Array of data values for the Product Definition
!> Template specified by ipdtn.
!> @param[in] listsec1 Contains information read from GRIB
!> Identification Section 1. Must be dimensioned >= 13.
!> @param[out] tabbrev Character array that will get the date and time
!> string. Must be of length 100.
!>
!> @author Stephen Gilbert @date 2010-09-08
subroutine prvtime(ipdtn, ipdtmpl, listsec1, tabbrev)
	implicit none
 
	integer, intent(in) :: ipdtn
	integer, intent(in) :: ipdtmpl(*), listsec1(*)
	character(len = 110), intent(out) :: tabbrev
 
	character(len = 16) :: reftime, endtime
	character(len = 12) :: tmpval2
	character(len = 12) :: tmpval
	character(len = 10) :: tunit
	integer, dimension(200) :: ipos, ipos2
	integer :: is, itemp, itemp2, iunit, iuni2t2, iunit2, iutpos, iutpos2, j
	
	data ipos  /7*0, 16, 23, 17, 19, 18, 32, 31, 27*0, 17, 20, 0, 0, 22,  &
		  25, 43*0, 23, 109*0/
 
	data ipos2 /7*0, 26, 33, 27, 29, 28, 42, 41, 27*0, 22, 30, 0, 0, 32,  &
		  35, 43*0, 33, 109*0/
 
	tabbrev(1:100) = " "
 
	! Determine unit of time range.
	if ((ipdtn .ge. 0 .and. ipdtn .le. 15) .or. ipdtn .eq. 32 &
		  .or. ipdtn .eq. 50 .or. ipdtn .eq. 51    &
		  .or. ipdtn .eq. 91) then
		iutpos = 8
	elseif (ipdtn .ge. 40 .and. ipdtn .le. 43) then
		iutpos = 9
	elseif (ipdtn .ge. 44 .and. ipdtn .le. 47) then
		iutpos = 14
	elseif (ipdtn .eq. 48) then
		iutpos = 19
	elseif (ipdtn .eq. 52) then
		iutpos = 11
	else
		iutpos = 8
	endif
 
	! Determine first unit of time range.
	selectcase(ipdtmpl(iutpos))
	case (0)
		tunit = "minute"
		iunit = 1
	case (1)
		tunit = "hour"
		iunit = 1
	case (2)
		tunit = "day"
		iunit = 1
	case (3)
		tunit = "month"
		iunit = 1
	case (4)
		tunit = "year"
		iunit = 1
	case (10)
		tunit = "hour"
		iunit = 3
	case (11)
		tunit = "hour"
		iunit = 6
	case default
		tunit = "hour"
		iunit = 1
	end select
 
	! Determine second unit of time range.
	if (ipdtn .eq. 0) then
		iunit2 = 1
		iutpos2 = 0
	else
		iutpos2 = ipos2(ipdtn)
		if (iutpos2 .gt. 0) then
			selectcase(ipdtmpl(iutpos2))
			case (0)
				iunit2 = 1
			case (1)
				iunit2 = 1
			case (2)
				iunit2 = 1
			case (3)
				iuni2t2 = 1
			case (4)
				iunit2 = 1
			case (10)
				iunit2 = 3
			case (11)
				iunit2 = 6
			case default
				iunit2 = 1
			end select
		endif
	endif
 
	write(reftime, fmt = '(i4,3i2.2,":",i2.2,":",i2.2)') (listsec1(j), j = 6, 11)
	itemp = abs(ipdtmpl(iutpos + 1)) * iunit
	write(tmpval, '(I0)') itemp
	write(tabbrev, fmt = '("valid at  ", i4)') ipdtmpl(iutpos + 1)
 
	! Determine Reference Time: Year, Month, Day, Hour, Minute, Second.
	if ((ipdtn .ge. 0 .and. ipdtn .le. 7) .or. ipdtn .eq. 15 &
		  .or. ipdtn .eq. 20 .or. (ipdtn .ge. 30 .and. ipdtn .le. 32) &
		  .or. ipdtn .eq. 40 .or. ipdtn .eq. 41 .or. ipdtn .eq. 44 &
		  .or. ipdtn .eq. 45 .or. ipdtn .eq. 48 .or.  &
		  (ipdtn .ge. 50 .and. ipdtn .le. 52)) then ! Point in time
		tabbrev = "valid  " // trim(tmpval) // " " // trim(tunit) // " after " // reftime
	else
		is = ipos(ipdtn) ! Continuous time interval
		write(endtime, fmt = '(i4,3i2.2,":",i2.2,":",i2.2)') (ipdtmpl(j), j = is, is + 5)
		itemp2 = abs(ipdtmpl(iutpos2 + 1)) * iunit2
		itemp2 = itemp + itemp2
		write(tmpval2, '(I0)') itemp2
		if (ipdtn .eq. 8 .and. ipdtmpl(9) .lt. 0) then
			tabbrev = "(" // trim(tmpval) // " -" &
				  // trim(tmpval2) // ") valid  " // trim(tmpval) // &
				  " " // trim(tunit) // " before " &
				  // reftime // " to " //endtime
		elseif ((ipdtn .ge. 8 .and. ipdtn .le. 14) .or. &
			  (ipdtn .ge. 42 .and. ipdtn .le. 47) .or. &
			  ipdtn .eq. 91) then ! Continuous time interval
			tabbrev = "(" // trim(tmpval) // " -" &
				  // trim(tmpval2) // " hr) valid  " // trim(tmpval) // &
				  " " // trim(tunit) // " after " &
				  // reftime // " to " // endtime
		endif
	endif
 
	return
end subroutine prvtime
 