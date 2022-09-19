! This program tests the params module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 10/1/21
program test_params
  use params
  implicit none

  integer :: g2disc, g2cat, g2num, g1val, g1ver
  character(len=8) :: abbrev
  integer :: LU = 10;
  integer :: g1_table_version, g1_val, g2_discipline, g2_category, g2_param_num
  character(len = 8) :: g2_abbrev
  integer :: ios, i

  print *, 'Testing the params module.'

  print *, 'Testing param_g1_to_g2...'
  call param_g1_to_g2(1, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 2
  call param_g1_to_g2(47, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 3
  
  print *, 'Testing param_get_abbrev...'
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 4
  abbrev = param_get_abbrev(0, 3, 8)
  if (abbrev .ne. 'PRESA') stop 4
  
  print *, 'Testing param_g2_to_g1...'
  call param_g2_to_g1(0, 3, 1, g1val, g1ver)
  if (g1val .ne. 2 .or. g1ver .ne. 2) stop 6
  call param_g2_to_g1(0, 2, 0, g1val, g1ver)
  if (g1val .ne. 31 .or. g1ver .ne. 2) stop 7

  print *, 'Testing param_all with out of range indexes...'
  call param_all(0, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 255 .or. g1_val .ne. 255 .or. g2_discipline .ne. 255 .or. &
       g2_category .ne. 255 .or. g2_param_num .ne. 255 .or. g2_abbrev .ne. 'UNKNOWN ') stop 10
  call param_all(2001, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 255 .or. g1_val .ne. 255 .or. g2_discipline .ne. 255 .or. &
       g2_category .ne. 255 .or. g2_param_num .ne. 255 .or. g2_abbrev .ne. 'UNKNOWN ') stop 10
  
  print *, 'Testing param_all with some real indexes...'
!  Index, GRIB1_version, GRIB1_value, GRIB2_discipline, GRIB2_category, GRIB2_parameter
  call param_all(1, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 2 .or. g1_val .ne. 1 .or. g2_discipline .ne. 0 .or. &
       g2_category .ne. 3 .or. g2_param_num .ne. 0 .or. g2_abbrev .ne. 'PRES') stop 10


  ! Check all abb
  ! Check all abb
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 4
  abbrev = param_get_abbrev(0, 3, 0)
  if (abbrev .ne. 'PRES') stop 5
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 5
  abbrev = param_get_abbrev(0, 3, 2)
  if (abbrev .ne. 'PTEND') stop 5
  abbrev = param_get_abbrev(0, 2, 14)
  if (abbrev .ne. 'PVORT') stop 5
  abbrev = param_get_abbrev(0, 3, 3)
  if (abbrev .ne. 'ICAHT') stop 5
  abbrev = param_get_abbrev(0, 3, 4)
  if (abbrev .ne. 'GP') stop 5
  abbrev = param_get_abbrev(0, 3, 5)
  if (abbrev .ne. 'HGT') stop 5
  abbrev = param_get_abbrev(0, 3, 6)
  if (abbrev .ne. 'DIST') stop 5
  abbrev = param_get_abbrev(0, 3, 7)
  if (abbrev .ne. 'HSTDV') stop 5
  abbrev = param_get_abbrev(0, 14, 0)
  if (abbrev .ne. 'TOZNE') stop 5
  abbrev = param_get_abbrev(0, 0, 0)
  if (abbrev .ne. 'TMP') stop 5
  abbrev = param_get_abbrev(0, 0, 1)
  if (abbrev .ne. 'VTMP') stop 5
  abbrev = param_get_abbrev(0, 0, 2)
  if (abbrev .ne. 'POT') stop 5
  abbrev = param_get_abbrev(0, 0, 3)
  if (abbrev .ne. 'EPOT') stop 5
  abbrev = param_get_abbrev(0, 0, 4)
  if (abbrev .ne. 'TMAX') stop 5
  abbrev = param_get_abbrev(0, 0, 5)
  if (abbrev .ne. 'TMIN') stop 5
  abbrev = param_get_abbrev(0, 0, 6)
  if (abbrev .ne. 'DPT') stop 5
  abbrev = param_get_abbrev(0, 0, 7)
  if (abbrev .ne. 'DEPR') stop 5
  abbrev = param_get_abbrev(0, 0, 8)
  if (abbrev .ne. 'LAPR') stop 5
  abbrev = param_get_abbrev(0, 19, 0)
  if (abbrev .ne. 'VIS') stop 5
  abbrev = param_get_abbrev(0, 15, 6)
  if (abbrev .ne. 'RDSP1') stop 5
  abbrev = param_get_abbrev(0, 15, 7)
  if (abbrev .ne. 'RDSP2') stop 5
  abbrev = param_get_abbrev(0, 15, 8)
  if (abbrev .ne. 'RDSP3') stop 5
  abbrev = param_get_abbrev(0, 7, 0)
  if (abbrev .ne. 'PLI') stop 5
  abbrev = param_get_abbrev(0, 0, 9)
  if (abbrev .ne. 'TMPA') stop 5
  abbrev = param_get_abbrev(0, 3, 8)
  if (abbrev .ne. 'PRESA') stop 5
  abbrev = param_get_abbrev(0, 3, 9)
  if (abbrev .ne. 'GPA') stop 5
  abbrev = param_get_abbrev(10, 0, 0)
  if (abbrev .ne. 'WVSP1') stop 5
  abbrev = param_get_abbrev(10, 0, 1)
  if (abbrev .ne. 'WVSP2') stop 5
  abbrev = param_get_abbrev(10, 0, 2)
  if (abbrev .ne. 'WVSP3') stop 5
  abbrev = param_get_abbrev(0, 2, 0)
  if (abbrev .ne. 'WDIR') stop 5
  abbrev = param_get_abbrev(0, 2, 1)
  if (abbrev .ne. 'WIND') stop 5
  abbrev = param_get_abbrev(0, 2, 2)
  if (abbrev .ne. 'UGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 3)
  if (abbrev .ne. 'VGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 4)
  if (abbrev .ne. 'STRM') stop 5
  abbrev = param_get_abbrev(0, 2, 5)
  if (abbrev .ne. 'VPOT') stop 5
  abbrev = param_get_abbrev(0, 2, 6)
  if (abbrev .ne. 'MNTSF') stop 5
  abbrev = param_get_abbrev(0, 2, 7)
  if (abbrev .ne. 'SGCVV') stop 5
  abbrev = param_get_abbrev(0, 2, 8)
  if (abbrev .ne. 'VVEL') stop 5
  abbrev = param_get_abbrev(0, 2, 9)
  if (abbrev .ne. 'DZDT') stop 5
  abbrev = param_get_abbrev(0, 2, 10)
  if (abbrev .ne. 'ABSV') stop 5
  abbrev = param_get_abbrev(0, 2, 11)
  if (abbrev .ne. 'ABSD') stop 5
  abbrev = param_get_abbrev(0, 2, 12)
  if (abbrev .ne. 'RELV') stop 5
  abbrev = param_get_abbrev(0, 2, 13)
  if (abbrev .ne. 'RELD') stop 5
  abbrev = param_get_abbrev(0, 2, 15)
  if (abbrev .ne. 'VUCSH') stop 5
  abbrev = param_get_abbrev(0, 2, 16)
  if (abbrev .ne. 'VVCSH') stop 5
  abbrev = param_get_abbrev(10, 1, 0)
  if (abbrev .ne. 'DIRC') stop 5
  abbrev = param_get_abbrev(10, 1, 1)
  if (abbrev .ne. 'SPC') stop 5
  abbrev = param_get_abbrev(10, 1, 2)
  if (abbrev .ne. 'UOGRD') stop 5
  abbrev = param_get_abbrev(10, 1, 3)
  if (abbrev .ne. 'VOGRD') stop 5
  abbrev = param_get_abbrev(0, 1, 0)
  if (abbrev .ne. 'SPFH') stop 5
  abbrev = param_get_abbrev(0, 1, 1)
  if (abbrev .ne. 'RH') stop 5
  abbrev = param_get_abbrev(0, 1, 2)
  if (abbrev .ne. 'MIXR') stop 5
  abbrev = param_get_abbrev(0, 1, 3)
  if (abbrev .ne. 'PWAT') stop 5
  abbrev = param_get_abbrev(0, 1, 4)
  if (abbrev .ne. 'VAPP') stop 5
  abbrev = param_get_abbrev(0, 1, 5)
  if (abbrev .ne. 'SATD') stop 5
  abbrev = param_get_abbrev(0, 1, 6)
  if (abbrev .ne. 'EVP') stop 5
  abbrev = param_get_abbrev(0, 6, 0)
  if (abbrev .ne. 'CICE') stop 5
  abbrev = param_get_abbrev(0, 1, 7)
  if (abbrev .ne. 'PRATE') stop 5
  abbrev = param_get_abbrev(0, 19, 2)
  if (abbrev .ne. 'TSTM') stop 5
  abbrev = param_get_abbrev(0, 1, 8)
  if (abbrev .ne. 'APCP') stop 5
  abbrev = param_get_abbrev(0, 1, 9)
  if (abbrev .ne. 'NCPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 10)
  if (abbrev .ne. 'ACPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 12)
  if (abbrev .ne. 'SRWEQ') stop 5
  abbrev = param_get_abbrev(0, 1, 13)
  if (abbrev .ne. 'WEASD') stop 5
  abbrev = param_get_abbrev(0, 1, 11)
  if (abbrev .ne. 'SNOD') stop 5
  abbrev = param_get_abbrev(0, 19, 3)
  if (abbrev .ne. 'MIXHT') stop 5
  abbrev = param_get_abbrev(10, 4, 2)
  if (abbrev .ne. 'TTHDP') stop 5
  abbrev = param_get_abbrev(10, 4, 0)
  if (abbrev .ne. 'MTHD') stop 5
  abbrev = param_get_abbrev(10, 4, 1)
  if (abbrev .ne. 'MTHA') stop 5
  abbrev = param_get_abbrev(0, 6, 1)
  if (abbrev .ne. 'TCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 2)
  if (abbrev .ne. 'CDCON') stop 5
  abbrev = param_get_abbrev(0, 6, 3)
  if (abbrev .ne. 'LCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 4)
  if (abbrev .ne. 'MCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 5)
  if (abbrev .ne. 'HCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 6)
  if (abbrev .ne. 'CWAT') stop 5
  abbrev = param_get_abbrev(0, 7, 1)
  if (abbrev .ne. 'BLI') stop 5
  abbrev = param_get_abbrev(0, 1, 14)
  if (abbrev .ne. 'SNOC') stop 5
  abbrev = param_get_abbrev(0, 1, 15)
  if (abbrev .ne. 'SNOL') stop 5
  abbrev = param_get_abbrev(10, 3, 0)
  if (abbrev .ne. 'WTMP') stop 5
  abbrev = param_get_abbrev(2, 0, 0)
  if (abbrev .ne. 'LAND') stop 5
  abbrev = param_get_abbrev(10, 3, 1)
  if (abbrev .ne. 'DSLM') stop 5
  abbrev = param_get_abbrev(2, 0, 1)
  if (abbrev .ne. 'SFCR') stop 5
  abbrev = param_get_abbrev(0, 19, 1)
  if (abbrev .ne. 'ALBDO') stop 5
  abbrev = param_get_abbrev(2, 0, 2)
  if (abbrev .ne. 'TSOIL') stop 5
  abbrev = param_get_abbrev(2, 0, 3)
  if (abbrev .ne. 'SOILM') stop 5
  abbrev = param_get_abbrev(2, 0, 4)
  if (abbrev .ne. 'VEG') stop 5
  abbrev = param_get_abbrev(10, 4, 3)
  if (abbrev .ne. 'SALTY') stop 5
  abbrev = param_get_abbrev(0, 3, 10)
  if (abbrev .ne. 'DEN') stop 5
  abbrev = param_get_abbrev(2, 0, 5)
  if (abbrev .ne. 'WATR') stop 5
  abbrev = param_get_abbrev(10, 2, 0)
  if (abbrev .ne. 'ICEC') stop 5
  abbrev = param_get_abbrev(10, 2, 1)
  if (abbrev .ne. 'ICETK') stop 5
  abbrev = param_get_abbrev(10, 2, 2)
  if (abbrev .ne. 'DICED') stop 5
  abbrev = param_get_abbrev(10, 2, 3)
  if (abbrev .ne. 'SICED') stop 5
  abbrev = param_get_abbrev(10, 2, 4)
  if (abbrev .ne. 'UICE') stop 5
  abbrev = param_get_abbrev(10, 2, 5)
  if (abbrev .ne. 'VICE') stop 5
  abbrev = param_get_abbrev(10, 2, 6)
  if (abbrev .ne. 'ICEG') stop 5
  abbrev = param_get_abbrev(10, 2, 7)
  if (abbrev .ne. 'ICED') stop 5
  abbrev = param_get_abbrev(0, 1, 16)
  if (abbrev .ne. 'SNOM') stop 5
  abbrev = param_get_abbrev(10, 0, 3)
  if (abbrev .ne. 'HTSGW') stop 5
  abbrev = param_get_abbrev(10, 0, 4)
  if (abbrev .ne. 'WVDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 5)
  if (abbrev .ne. 'WVHGT') stop 5
  abbrev = param_get_abbrev(10, 0, 6)
  if (abbrev .ne. 'WVPER') stop 5
  abbrev = param_get_abbrev(10, 0, 7)
  if (abbrev .ne. 'SWDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 8)
  if (abbrev .ne. 'SWELL') stop 5
  abbrev = param_get_abbrev(10, 0, 9)
  if (abbrev .ne. 'SWPER') stop 5
  abbrev = param_get_abbrev(10, 0, 10)
  if (abbrev .ne. 'DIRPW') stop 5
  abbrev = param_get_abbrev(10, 0, 11)
  if (abbrev .ne. 'PERPW') stop 5
  abbrev = param_get_abbrev(10, 0, 12)
  if (abbrev .ne. 'DIRSW') stop 5
  abbrev = param_get_abbrev(10, 0, 13)
  if (abbrev .ne. 'PERSW') stop 5
  abbrev = param_get_abbrev(0, 4, 0)
  if (abbrev .ne. 'NSWRS') stop 5
  abbrev = param_get_abbrev(0, 5, 0)
  if (abbrev .ne. 'NLWRS') stop 5
  abbrev = param_get_abbrev(0, 4, 1)
  if (abbrev .ne. 'NSWRT') stop 5
  abbrev = param_get_abbrev(0, 5, 1)
  if (abbrev .ne. 'NLWRT') stop 5
  abbrev = param_get_abbrev(0, 5, 2)
  if (abbrev .ne. 'LWAVR') stop 5
  abbrev = param_get_abbrev(0, 4, 2)
  if (abbrev .ne. 'SWAVR') stop 5
  abbrev = param_get_abbrev(0, 4, 3)
  if (abbrev .ne. 'GRAD') stop 5
  abbrev = param_get_abbrev(0, 4, 4)
  if (abbrev .ne. 'BRTMP') stop 5
  abbrev = param_get_abbrev(0, 4, 5)
  if (abbrev .ne. 'LWRAD') stop 5
  abbrev = param_get_abbrev(0, 4, 6)
  if (abbrev .ne. 'SWRAD') stop 5
  abbrev = param_get_abbrev(0, 0, 10)
  if (abbrev .ne. 'LHTFL') stop 5
  abbrev = param_get_abbrev(0, 0, 11)
  if (abbrev .ne. 'SHTFL') stop 5
  abbrev = param_get_abbrev(0, 2, 20)
  if (abbrev .ne. 'BLYDP') stop 5
  abbrev = param_get_abbrev(0, 2, 17)
  if (abbrev .ne. 'UFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 18)
  if (abbrev .ne. 'VFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 19)
  if (abbrev .ne. 'WMIXE') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(0, 0, 192)
  if (abbrev .ne. 'SNOHF') stop 5
  abbrev = param_get_abbrev(0, 1, 22)
  if (abbrev .ne. 'CLWMR') stop 5
  abbrev = param_get_abbrev(0, 1, 192)
  if (abbrev .ne. 'CRAIN') stop 5
  abbrev = param_get_abbrev(0, 1, 193)
  if (abbrev .ne. 'CFRZR') stop 5
  abbrev = param_get_abbrev(0, 1, 194)
  if (abbrev .ne. 'CICEP') stop 5
  abbrev = param_get_abbrev(0, 1, 195)
  if (abbrev .ne. 'CSNOW') stop 5
  abbrev = param_get_abbrev(0, 1, 196)
  if (abbrev .ne. 'CPRAT') stop 5
  abbrev = param_get_abbrev(0, 1, 197)
  if (abbrev .ne. 'MCONV') stop 5
  abbrev = param_get_abbrev(1, 1, 193)
  if (abbrev .ne. 'CPOFP') stop 5
  abbrev = param_get_abbrev(0, 1, 199)
  if (abbrev .ne. 'PEVAP') stop 5
  abbrev = param_get_abbrev(0, 2, 192)
  if (abbrev .ne. 'VWSH') stop 5
  abbrev = param_get_abbrev(0, 2, 193)
  if (abbrev .ne. 'MFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 194)
  if (abbrev .ne. 'USTM') stop 5
  abbrev = param_get_abbrev(0, 2, 195)
  if (abbrev .ne. 'VSTM') stop 5
  abbrev = param_get_abbrev(0, 2, 196)
  if (abbrev .ne. 'CD') stop 5
  abbrev = param_get_abbrev(0, 2, 197)
  if (abbrev .ne. 'FRICV') stop 5
  abbrev = param_get_abbrev(0, 3, 192)
  if (abbrev .ne. 'MSLET') stop 5
  abbrev = param_get_abbrev(0, 4, 192)
  if (abbrev .ne. 'DSWRF') stop 5
  abbrev = param_get_abbrev(0, 4, 193)
  if (abbrev .ne. 'USWRF') stop 5
  abbrev = param_get_abbrev(0, 5, 192)
  if (abbrev .ne. 'DLWRF') stop 5
  abbrev = param_get_abbrev(0, 5, 193)
  if (abbrev .ne. 'ULWRF') stop 5
  abbrev = param_get_abbrev(0, 6, 192)
  if (abbrev .ne. 'CDLYR') stop 5
  abbrev = param_get_abbrev(0, 7, 193)
  if (abbrev .ne. '4LFTX') stop 5
  abbrev = param_get_abbrev(0, 7, 6)
  if (abbrev .ne. 'CAPE') stop 5
  abbrev = param_get_abbrev(0, 7, 7)
  if (abbrev .ne. 'CIN') stop 5
  abbrev = param_get_abbrev(0, 7, 8)
  if (abbrev .ne. 'HLCY') stop 5
  abbrev = param_get_abbrev(0, 7, 192)
  if (abbrev .ne. 'LFTX') stop 5
  abbrev = param_get_abbrev(0, 19, 11)
  if (abbrev .ne. 'TKE') stop 5
  abbrev = param_get_abbrev(0, 191, 192)
  if (abbrev .ne. 'NLAT') stop 5
  abbrev = param_get_abbrev(0, 191, 193)
  if (abbrev .ne. 'ELON') stop 5
  abbrev = param_get_abbrev(1, 0, 192)
  if (abbrev .ne. 'BGRUN') stop 5
  abbrev = param_get_abbrev(1, 0, 193)
  if (abbrev .ne. 'SSRUN') stop 5
  abbrev = param_get_abbrev(2, 0, 192)
  if (abbrev .ne. 'SOILW') stop 5
  abbrev = param_get_abbrev(2, 0, 193)
  if (abbrev .ne. 'GFLUX') stop 5
  abbrev = param_get_abbrev(2, 0, 194)
  if (abbrev .ne. 'MSTAV') stop 5
  abbrev = param_get_abbrev(2, 0, 195)
  if (abbrev .ne. 'SFEXC') stop 5
  abbrev = param_get_abbrev(2, 0, 196)
  if (abbrev .ne. 'CNWAT') stop 5
  abbrev = param_get_abbrev(2, 0, 197)
  if (abbrev .ne. 'BMIXL') stop 5
  abbrev = param_get_abbrev(0, 14, 192)
  if (abbrev .ne. 'O3MR') stop 5
  abbrev = param_get_abbrev(0, 3, 193)
  if (abbrev .ne. '5WAVH') stop 5
  abbrev = param_get_abbrev(0, 1, 200)
  if (abbrev .ne. 'PEVPR') stop 5
  abbrev = param_get_abbrev(0, 6, 193)
  if (abbrev .ne. 'CWORK') stop 5
  abbrev = param_get_abbrev(0, 3, 194)
  if (abbrev .ne. 'U-GWD') stop 5
  abbrev = param_get_abbrev(0, 3, 195)
  if (abbrev .ne. 'V-GWD') stop 5
  abbrev = param_get_abbrev(0, 3, 196)
  if (abbrev .ne. 'HPBL') stop 5
  abbrev = param_get_abbrev(0, 3, 197)
  if (abbrev .ne. '5WAVA') stop 5
  abbrev = param_get_abbrev(2, 3, 192)
  if (abbrev .ne. 'SOILL') stop 5
  abbrev = param_get_abbrev(2, 3, 193)
  if (abbrev .ne. 'RLYRS') stop 5
  abbrev = param_get_abbrev(2, 0, 201)
  if (abbrev .ne. 'WILT') stop 5
  abbrev = param_get_abbrev(2, 3, 194)
  if (abbrev .ne. 'SLTYP') stop 5
  abbrev = param_get_abbrev(2, 3, 0)
  if (abbrev .ne. 'SOTYP') stop 5
  abbrev = param_get_abbrev(2, 0, 198)
  if (abbrev .ne. 'VGTYP') stop 5
  abbrev = param_get_abbrev(2, 3, 195)
  if (abbrev .ne. 'SMREF') stop 5
  abbrev = param_get_abbrev(2, 3, 196)
  if (abbrev .ne. 'SMDRY') stop 5
  abbrev = param_get_abbrev(0, 1, 201)
  if (abbrev .ne. 'SNOWC') stop 5
  abbrev = param_get_abbrev(2, 3, 197)
  if (abbrev .ne. 'POROS') stop 5
  abbrev = param_get_abbrev(0, 1, 202)
  if (abbrev .ne. 'FRAIN') stop 5
  abbrev = param_get_abbrev(0, 6, 199)
  if (abbrev .ne. 'FICE') stop 5
  abbrev = param_get_abbrev(0, 1, 203)
  if (abbrev .ne. 'RIME') stop 5
  abbrev = param_get_abbrev(0, 6, 194)
  if (abbrev .ne. 'CUEFI') stop 5
  abbrev = param_get_abbrev(0, 6, 195)
  if (abbrev .ne. 'TCOND') stop 5
  abbrev = param_get_abbrev(0, 6, 196)
  if (abbrev .ne. 'TCOLW') stop 5
  abbrev = param_get_abbrev(0, 6, 197)
  if (abbrev .ne. 'TCOLI') stop 5
  abbrev = param_get_abbrev(0, 1, 204)
  if (abbrev .ne. 'TCOLR') stop 5
  abbrev = param_get_abbrev(0, 1, 205)
  if (abbrev .ne. 'TCOLS') stop 5
  abbrev = param_get_abbrev(0, 6, 198)
  if (abbrev .ne. 'TCOLC') stop 5
  abbrev = param_get_abbrev(0, 19, 192)
  if (abbrev .ne. 'MXSALB') stop 5
  abbrev = param_get_abbrev(0, 19, 193)
  if (abbrev .ne. 'SNFALB') stop 5
  abbrev = param_get_abbrev(0, 1, 24)
  if (abbrev .ne. 'RWMR') stop 5
  abbrev = param_get_abbrev(0, 1, 25)
  if (abbrev .ne. 'SNMR') stop 5
  abbrev = param_get_abbrev(2, 0, 199)
  if (abbrev .ne. 'CCOND') stop 5
  abbrev = param_get_abbrev(2, 0, 200)
  if (abbrev .ne. 'RSMIN') stop 5
  abbrev = param_get_abbrev(2, 0, 202)
  if (abbrev .ne. 'RCS') stop 5
  abbrev = param_get_abbrev(2, 0, 203)
  if (abbrev .ne. 'RCT') stop 5
  abbrev = param_get_abbrev(2, 0, 204)
  if (abbrev .ne. 'RCQ') stop 5
  abbrev = param_get_abbrev(2, 0, 205)
  if (abbrev .ne. 'RCSOL') stop 5
  abbrev = param_get_abbrev(0, 7, 194)
  if (abbrev .ne. 'RI') stop 5
  abbrev = param_get_abbrev(3, 1, 192)
  if (abbrev .ne. 'USCT') stop 5
  abbrev = param_get_abbrev(3, 1, 193)
  if (abbrev .ne. 'VSCT') stop 5
  abbrev = param_get_abbrev(0, 191, 194)
  if (abbrev .ne. 'TSEC') stop 5
  abbrev = param_get_abbrev(0, 14, 193)
  if (abbrev .ne. 'OZCON') stop 5
  abbrev = param_get_abbrev(0, 14, 194)
  if (abbrev .ne. 'OZCAT') stop 5
  abbrev = param_get_abbrev(1, 1, 2)
  if (abbrev .ne. 'POP') stop 5
  abbrev = param_get_abbrev(1, 1, 192)
  if (abbrev .ne. 'CPOZP') stop 5
  abbrev = param_get_abbrev(0, 2, 22)
  if (abbrev .ne. 'GUST') stop 5
  abbrev = param_get_abbrev(0, 2, 0)
  if (abbrev .ne. 'WDIR') stop 5
  abbrev = param_get_abbrev(0, 2, 1)
  if (abbrev .ne. 'WIND') stop 5
  abbrev = param_get_abbrev(0, 2, 2)
  if (abbrev .ne. 'UGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 3)
  if (abbrev .ne. 'VGRD') stop 5
  abbrev = param_get_abbrev(10, 0, 3)
  if (abbrev .ne. 'HTSGW') stop 5
  abbrev = param_get_abbrev(10, 0, 4)
  if (abbrev .ne. 'WVDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 6)
  if (abbrev .ne. 'WVPER') stop 5
  abbrev = param_get_abbrev(10, 0, 10)
  if (abbrev .ne. 'DIRPW') stop 5
  abbrev = param_get_abbrev(10, 0, 11)
  if (abbrev .ne. 'PERPW') stop 5
  abbrev = param_get_abbrev(10, 0, 12)
  if (abbrev .ne. 'DIRSW') stop 5
  abbrev = param_get_abbrev(10, 0, 13)
  if (abbrev .ne. 'PERSW') stop 5
  abbrev = param_get_abbrev(0, 13, 192)
  if (abbrev .ne. 'PMTC') stop 5
  abbrev = param_get_abbrev(0, 13, 193)
  if (abbrev .ne. 'PMTF') stop 5
  abbrev = param_get_abbrev(0, 0, 0)
  if (abbrev .ne. 'TMP') stop 5
  abbrev = param_get_abbrev(0, 3, 198)
  if (abbrev .ne. 'MSLMA') stop 5
  abbrev = param_get_abbrev(0, 13, 194)
  if (abbrev .ne. 'LPMTF') stop 5
  abbrev = param_get_abbrev(0, 13, 195)
  if (abbrev .ne. 'LIPMF') stop 5
  abbrev = param_get_abbrev(0, 1, 23)
  if (abbrev .ne. 'ICMR') stop 5
  abbrev = param_get_abbrev(0, 1, 32)
  if (abbrev .ne. 'GRMR') stop 5
  abbrev = param_get_abbrev(0, 1, 206)
  if (abbrev .ne. 'TIPD') stop 5
  abbrev = param_get_abbrev(0, 17, 192)
  if (abbrev .ne. 'LTNG') stop 5
  abbrev = param_get_abbrev(2, 0, 206)
  if (abbrev .ne. 'RDRIP') stop 5
  abbrev = param_get_abbrev(0, 0, 15)
  if (abbrev .ne. 'VPTMP') stop 5
  abbrev = param_get_abbrev(0, 1, 207)
  if (abbrev .ne. 'NCIP') stop 5
  abbrev = param_get_abbrev(0, 1, 208)
  if (abbrev .ne. 'SNOT') stop 5
!  abbrev = param_get_abbrev(0, 3, 1)
!  if (abbrev .ne. 'MSLSA') stop 5
  abbrev = param_get_abbrev(0, 3, 199)
  if (abbrev .ne. 'TSLSA') stop 5
  abbrev = param_get_abbrev(0, 3, 200)
  if (abbrev .ne. 'PLPL') stop 5
  abbrev = param_get_abbrev(0, 4, 194)
  if (abbrev .ne. 'DUVB') stop 5
  abbrev = param_get_abbrev(0, 4, 195)
  if (abbrev .ne. 'CDUVB') stop 5
  abbrev = param_get_abbrev(2, 0, 207)
  if (abbrev .ne. 'ICWAT') stop 5
  abbrev = param_get_abbrev(0, 19, 204)
  if (abbrev .ne. 'MIXLY') stop 5
  abbrev = param_get_abbrev(0, 0, 193)
  if (abbrev .ne. 'TTRAD') stop 5
  abbrev = param_get_abbrev(0, 16, 195)
  if (abbrev .ne. 'REFD') stop 5
  abbrev = param_get_abbrev(0, 16, 196)
  if (abbrev .ne. 'REFC') stop 5
  abbrev = param_get_abbrev(0, 4, 196)
  if (abbrev .ne. 'CSDSF') stop 5
  abbrev = param_get_abbrev(0, 1, 209)
  if (abbrev .ne. 'TCLSW') stop 5
  abbrev = param_get_abbrev(0, 1, 210)
  if (abbrev .ne. 'TCOLM') stop 5
  abbrev = param_get_abbrev(0, 3, 201)
  if (abbrev .ne. 'LPSX') stop 5
  abbrev = param_get_abbrev(0, 3, 202)
  if (abbrev .ne. 'LPSY') stop 5
  abbrev = param_get_abbrev(0, 3, 203)
  if (abbrev .ne. 'HGTX') stop 5
  abbrev = param_get_abbrev(0, 3, 204)
  if (abbrev .ne. 'HGTY') stop 5
  abbrev = param_get_abbrev(0, 0, 194)
  if (abbrev .ne. 'REV') stop 5
  abbrev = param_get_abbrev(10, 2, 0)
  if (abbrev .ne. 'ICEC') stop 5
  abbrev = param_get_abbrev(10, 1, 2)
  if (abbrev .ne. 'UOGRD') stop 5
  abbrev = param_get_abbrev(10, 1, 3)
  if (abbrev .ne. 'VOGRD') stop 5
  abbrev = param_get_abbrev(10, 3, 0)
  if (abbrev .ne. 'WTMP') stop 5
  abbrev = param_get_abbrev(10, 3, 1)
  if (abbrev .ne. 'DSLM') stop 5
  abbrev = param_get_abbrev(10, 4, 3)
  if (abbrev .ne. 'SALTY') stop 5
  abbrev = param_get_abbrev(10, 1, 2)
  if (abbrev .ne. 'UOGRD') stop 5
  abbrev = param_get_abbrev(10, 1, 3)
  if (abbrev .ne. 'VOGRD') stop 5
  abbrev = param_get_abbrev(10, 3, 0)
  if (abbrev .ne. 'WTMP') stop 5
  abbrev = param_get_abbrev(10, 4, 3)
  if (abbrev .ne. 'SALTY') stop 5
  abbrev = param_get_abbrev(0, 2, 9)
  if (abbrev .ne. 'DZDT') stop 5
  abbrev = param_get_abbrev(0, 19, 3)
  if (abbrev .ne. 'MIXHT') stop 5
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 5
  abbrev = param_get_abbrev(0, 3, 5)
  if (abbrev .ne. 'HGT') stop 5
  abbrev = param_get_abbrev(10, 3, 194)
  if (abbrev .ne. 'ELEV') stop 5
  abbrev = param_get_abbrev(0, 1, 198)
  if (abbrev .ne. 'MINRH') stop 5
  abbrev = param_get_abbrev(0, 1, 27)
  if (abbrev .ne. 'MAXRH') stop 5
  abbrev = param_get_abbrev(0, 1, 29)
  if (abbrev .ne. 'ASNOW') stop 5
  abbrev = param_get_abbrev(0, 16, 192)
  if (abbrev .ne. 'REFZR') stop 5
  abbrev = param_get_abbrev(0, 16, 193)
  if (abbrev .ne. 'REFZI') stop 5
  abbrev = param_get_abbrev(0, 16, 194)
  if (abbrev .ne. 'REFZC') stop 5
  abbrev = param_get_abbrev(0, 2, 198)
  if (abbrev .ne. 'LAUV') stop 5
  abbrev = param_get_abbrev(0, 2, 199)
  if (abbrev .ne. 'LOUV') stop 5
  abbrev = param_get_abbrev(0, 2, 200)
  if (abbrev .ne. 'LAVV') stop 5
  abbrev = param_get_abbrev(0, 2, 201)
  if (abbrev .ne. 'LOVV') stop 5
  abbrev = param_get_abbrev(0, 2, 202)
  if (abbrev .ne. 'LAPP') stop 5
  abbrev = param_get_abbrev(0, 2, 203)
  if (abbrev .ne. 'LOPP') stop 5
  abbrev = param_get_abbrev(10, 3, 195)
  if (abbrev .ne. 'SSHG') stop 5
  abbrev = param_get_abbrev(0, 2, 2)
  if (abbrev .ne. 'UGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 3)
  if (abbrev .ne. 'VGRD') stop 5
  abbrev = param_get_abbrev(0, 3, 1)
  if (abbrev .ne. 'PRMSL') stop 5
  abbrev = param_get_abbrev(0, 3, 5)
  if (abbrev .ne. 'HGT') stop 5
  abbrev = param_get_abbrev(10, 4, 192)
  if (abbrev .ne. 'WTMPC') stop 5
  abbrev = param_get_abbrev(10, 4, 193)
  if (abbrev .ne. 'SALIN') stop 5
  abbrev = param_get_abbrev(10, 3, 196)
  if (abbrev .ne. 'P2OMLT') stop 5
  abbrev = param_get_abbrev(10, 1, 192)
  if (abbrev .ne. 'OMLU') stop 5
  abbrev = param_get_abbrev(10, 1, 193)
  if (abbrev .ne. 'OMLV') stop 5
  abbrev = param_get_abbrev(10, 1, 194)
  if (abbrev .ne. 'UBARO') stop 5
  abbrev = param_get_abbrev(10, 1, 195)
  if (abbrev .ne. 'VBARO') stop 5
  abbrev = param_get_abbrev(0, 19, 205)
  if (abbrev .ne. 'FLGHT') stop 5
  abbrev = param_get_abbrev(0, 19, 206)
  if (abbrev .ne. 'CICEL') stop 5
  abbrev = param_get_abbrev(0, 19, 207)
  if (abbrev .ne. 'CIVIS') stop 5
  abbrev = param_get_abbrev(0, 19, 208)
  if (abbrev .ne. 'CIFLT') stop 5
  abbrev = param_get_abbrev(0, 19, 209)
  if (abbrev .ne. 'LAVNI') stop 5
  abbrev = param_get_abbrev(0, 19, 210)
  if (abbrev .ne. 'HAVNI') stop 5
  abbrev = param_get_abbrev(0, 19, 211)
  if (abbrev .ne. 'SBSALB') stop 5
  abbrev = param_get_abbrev(0, 19, 212)
  if (abbrev .ne. 'SWSALB') stop 5
  abbrev = param_get_abbrev(0, 19, 213)
  if (abbrev .ne. 'NBSALB') stop 5
  abbrev = param_get_abbrev(0, 19, 214)
  if (abbrev .ne. 'NWSALB') stop 5
  abbrev = param_get_abbrev(10, 0, 192)
  if (abbrev .ne. 'WSTP') stop 5
  abbrev = param_get_abbrev(0, 1, 211)
  if (abbrev .ne. 'EMNP') stop 5
  abbrev = param_get_abbrev(0, 3, 205)
  if (abbrev .ne. 'LAYTH') stop 5
  abbrev = param_get_abbrev(0, 6, 13)
  if (abbrev .ne. 'CEIL') stop 5
  abbrev = param_get_abbrev(0, 19, 12)
  if (abbrev .ne. 'PBLREG') stop 5
  abbrev = param_get_abbrev(2, 0, 228)
  if (abbrev .ne. 'ACOND') stop 5
  abbrev = param_get_abbrev(0, 1, 212)
  if (abbrev .ne. 'SBSNO') stop 5
  abbrev = param_get_abbrev(2, 3, 198)
  if (abbrev .ne. 'EVBS') stop 5
  abbrev = param_get_abbrev(2, 0, 229)
  if (abbrev .ne. 'EVCW') stop 5
  abbrev = param_get_abbrev(2, 0, 230)
  if (abbrev .ne. 'TRANS') stop 5
  abbrev = param_get_abbrev(0, 2, 204)
  if (abbrev .ne. 'VEDH') stop 5
  abbrev = param_get_abbrev(0, 0, 195)
  if (abbrev .ne. 'LRGHR') stop 5
  abbrev = param_get_abbrev(0, 0, 196)
  if (abbrev .ne. 'CNVHR') stop 5
  abbrev = param_get_abbrev(0, 19, 20)
  if (abbrev .ne. 'ICIP') stop 5
  abbrev = param_get_abbrev(0, 19, 20)
  if (abbrev .ne. 'ICIP') stop 5
  abbrev = param_get_abbrev(0, 19, 21)
  if (abbrev .ne. 'CTP') stop 5
  abbrev = param_get_abbrev(0, 19, 21)
  if (abbrev .ne. 'CTP') stop 5
  abbrev = param_get_abbrev(0, 19, 22)
  if (abbrev .ne. 'CAT') stop 5
  abbrev = param_get_abbrev(0, 19, 22)
  if (abbrev .ne. 'CAT') stop 5
  abbrev = param_get_abbrev(0, 6, 25)
  if (abbrev .ne. 'CBHE') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(0, 3, 3)
  if (abbrev .ne. 'ICAHT') stop 5
  abbrev = param_get_abbrev(0, 3, 3)
  if (abbrev .ne. 'ICAHT') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  abbrev = param_get_abbrev(0, 6, 6)
  if (abbrev .ne. 'CWAT') stop 5
  abbrev = param_get_abbrev(10, 0, 7)
  if (abbrev .ne. 'SWDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 8)
  if (abbrev .ne. 'SWELL') stop 5
  abbrev = param_get_abbrev(10, 0, 9)
  if (abbrev .ne. 'SWPER') stop 5
  abbrev = param_get_abbrev(10, 0, 5)
  if (abbrev .ne. 'WVHGT') stop 5
  abbrev = param_get_abbrev(3, 192, 0)
  if (abbrev .ne. 'SBT122') stop 5
  abbrev = param_get_abbrev(3, 192, 1)
  if (abbrev .ne. 'SBT123') stop 5
  abbrev = param_get_abbrev(3, 192, 2)
  if (abbrev .ne. 'SBT124') stop 5
  abbrev = param_get_abbrev(3, 192, 3)
  if (abbrev .ne. 'SBT126') stop 5
  abbrev = param_get_abbrev(3, 192, 4)
  if (abbrev .ne. 'SBC123') stop 5
  abbrev = param_get_abbrev(3, 192, 5)
  if (abbrev .ne. 'SBC124') stop 5
  abbrev = param_get_abbrev(10, 3, 192)
  if (abbrev .ne. 'SURGE') stop 5
  abbrev = param_get_abbrev(10, 3, 193)
  if (abbrev .ne. 'ETSRG') stop 5
  abbrev = param_get_abbrev(0, 2, 14)
  if (abbrev .ne. 'PVORT') stop 5
  abbrev = param_get_abbrev(0, 192, 1)
  if (abbrev .ne. 'COVMZ') stop 5
  abbrev = param_get_abbrev(0, 192, 2)
  if (abbrev .ne. 'COVTZ') stop 5
  abbrev = param_get_abbrev(0, 192, 3)
  if (abbrev .ne. 'COVTM') stop 5
  abbrev = param_get_abbrev(0, 0, 197)
  if (abbrev .ne. 'THFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 2)
  if (abbrev .ne. 'UGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 3)
  if (abbrev .ne. 'VGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 9)
  if (abbrev .ne. 'DZDT') stop 5
  abbrev = param_get_abbrev(0, 2, 17)
  if (abbrev .ne. 'UFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 18)
  if (abbrev .ne. 'VFLX') stop 5
  abbrev = param_get_abbrev(0, 3, 6)
  if (abbrev .ne. 'DIST') stop 5
  abbrev = param_get_abbrev(0, 0, 2)
  if (abbrev .ne. 'POT') stop 5
  abbrev = param_get_abbrev(10, 4, 3)
  if (abbrev .ne. 'SALTY') stop 5
  abbrev = param_get_abbrev(10, 1, 2)
  if (abbrev .ne. 'UOGRD') stop 5
  abbrev = param_get_abbrev(10, 1, 3)
  if (abbrev .ne. 'VOGRD') stop 5
  abbrev = param_get_abbrev(0, 0, 198)
  if (abbrev .ne. 'TTDIA') stop 5
  abbrev = param_get_abbrev(0, 0, 199)
  if (abbrev .ne. 'TTPHY') stop 5
  abbrev = param_get_abbrev(2, 3, 199)
  if (abbrev .ne. 'LSPA') stop 5
  abbrev = param_get_abbrev(0, 4, 197)
  if (abbrev .ne. 'SWHR') stop 5
  abbrev = param_get_abbrev(0, 5, 194)
  if (abbrev .ne. 'LWHR') stop 5
  abbrev = param_get_abbrev(0, 4, 198)
  if (abbrev .ne. 'CSUSF') stop 5
  abbrev = param_get_abbrev(0, 5, 195)
  if (abbrev .ne. 'CSULF') stop 5
  abbrev = param_get_abbrev(0, 5, 196)
  if (abbrev .ne. 'CSDLF') stop 5
  abbrev = param_get_abbrev(0, 4, 199)
  if (abbrev .ne. 'CFNSF') stop 5
  abbrev = param_get_abbrev(0, 5, 197)
  if (abbrev .ne. 'CFNLF') stop 5
  abbrev = param_get_abbrev(0, 4, 200)
  if (abbrev .ne. 'VBDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 201)
  if (abbrev .ne. 'VDDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 202)
  if (abbrev .ne. 'NBDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 203)
  if (abbrev .ne. 'NDDSF') stop 5
  abbrev = param_get_abbrev(0, 7, 196)
  if (abbrev .ne. 'UVI') stop 5
  abbrev = param_get_abbrev(0, 0, 200)
  if (abbrev .ne. 'TSD1D') stop 5
  abbrev = param_get_abbrev(0, 3, 206)
  if (abbrev .ne. 'NLGSP') stop 5
  abbrev = param_get_abbrev(0, 0, 201)
  if (abbrev .ne. 'SHAHR') stop 5
  abbrev = param_get_abbrev(0, 0, 202)
  if (abbrev .ne. 'VDFHR') stop 5
  abbrev = param_get_abbrev(0, 1, 213)
  if (abbrev .ne. 'CNVMR') stop 5
  abbrev = param_get_abbrev(0, 1, 214)
  if (abbrev .ne. 'SHAMR') stop 5
  abbrev = param_get_abbrev(0, 1, 215)
  if (abbrev .ne. 'VDFMR') stop 5
  abbrev = param_get_abbrev(0, 2, 208)
  if (abbrev .ne. 'VDFUA') stop 5
  abbrev = param_get_abbrev(0, 2, 209)
  if (abbrev .ne. 'VDFVA') stop 5
  abbrev = param_get_abbrev(0, 7, 195)
  if (abbrev .ne. 'CWDI') stop 5
  abbrev = param_get_abbrev(0, 4, 204)
  if (abbrev .ne. 'DTRF') stop 5
  abbrev = param_get_abbrev(0, 4, 205)
  if (abbrev .ne. 'UTRF') stop 5
  abbrev = param_get_abbrev(0, 6, 200)
  if (abbrev .ne. 'MFLUX') stop 5
  abbrev = param_get_abbrev(0, 7, 195)
  if (abbrev .ne. 'CWDI') stop 5
  abbrev = param_get_abbrev(0, 19, 232)
  if (abbrev .ne. 'VAFTD') stop 5
  abbrev = param_get_abbrev(0, 1, 201)
  if (abbrev .ne. 'SNOWC') stop 5
  abbrev = param_get_abbrev(0, 1, 11)
  if (abbrev .ne. 'SNOD') stop 5
  abbrev = param_get_abbrev(0, 7, 2)
  if (abbrev .ne. 'KX') stop 5
  abbrev = param_get_abbrev(0, 7, 5)
  if (abbrev .ne. 'SX') stop 5
  abbrev = param_get_abbrev(10, 4, 194)
  if (abbrev .ne. 'BKENG') stop 5
  abbrev = param_get_abbrev(10, 4, 195)
  if (abbrev .ne. 'DBSS') stop 5
  abbrev = param_get_abbrev(10, 3, 197)
  if (abbrev .ne. 'AOHFLX') stop 5
  abbrev = param_get_abbrev(10, 3, 198)
  if (abbrev .ne. 'ASHFL') stop 5
  abbrev = param_get_abbrev(10, 3, 199)
  if (abbrev .ne. 'SSTT') stop 5
  abbrev = param_get_abbrev(10, 3, 200)
  if (abbrev .ne. 'SSST') stop 5
  abbrev = param_get_abbrev(10, 3, 201)
  if (abbrev .ne. 'KENG') stop 5
  abbrev = param_get_abbrev(10, 4, 196)
  if (abbrev .ne. 'INTFD') stop 5
  abbrev = param_get_abbrev(10, 3, 202)
  if (abbrev .ne. 'SLTFL') stop 5
  abbrev = param_get_abbrev(10, 4, 197)
  if (abbrev .ne. 'OHC') stop 5
  abbrev = param_get_abbrev(0, 1, 216)
  if (abbrev .ne. 'CONP') stop 5
  abbrev = param_get_abbrev(0, 191, 195)
  if (abbrev .ne. 'MLYNO') stop 5
  abbrev = param_get_abbrev(0, 1, 65)
  if (abbrev .ne. 'RPRATE') stop 5
  abbrev = param_get_abbrev(0, 1, 66)
  if (abbrev .ne. 'SPRATE') stop 5
  abbrev = param_get_abbrev(0, 1, 67)
  if (abbrev .ne. 'FPRATE') stop 5
  abbrev = param_get_abbrev(0, 1, 68)
  if (abbrev .ne. 'IPRATE') stop 5
  abbrev = param_get_abbrev(0, 7, 197)
  if (abbrev .ne. 'UPHL') stop 5
  abbrev = param_get_abbrev(2, 0, 4)
  if (abbrev .ne. 'VEG') stop 5
  abbrev = param_get_abbrev(1, 1, 195)
  if (abbrev .ne. 'CWR') stop 5
  abbrev = param_get_abbrev(0, 192, 4)
  if (abbrev .ne. 'COVTW') stop 5
  abbrev = param_get_abbrev(0, 192, 5)
  if (abbrev .ne. 'COVZZ') stop 5
  abbrev = param_get_abbrev(0, 192, 6)
  if (abbrev .ne. 'COVMM') stop 5
  abbrev = param_get_abbrev(0, 192, 7)
  if (abbrev .ne. 'COVQZ') stop 5
  abbrev = param_get_abbrev(0, 192, 8)
  if (abbrev .ne. 'COVQM') stop 5
  abbrev = param_get_abbrev(0, 192, 9)
  if (abbrev .ne. 'COVTVV') stop 5
  abbrev = param_get_abbrev(0, 192, 10)
  if (abbrev .ne. 'COVQVV') stop 5
  abbrev = param_get_abbrev(0, 192, 11)
  if (abbrev .ne. 'COVPSPS') stop 5
  abbrev = param_get_abbrev(0, 192, 12)
  if (abbrev .ne. 'COVQQ') stop 5
  abbrev = param_get_abbrev(0, 192, 13)
  if (abbrev .ne. 'COVVVVV') stop 5
  abbrev = param_get_abbrev(0, 192, 14)
  if (abbrev .ne. 'COVTT') stop 5
  abbrev = param_get_abbrev(0, 0, 203)
  if (abbrev .ne. 'THZ0') stop 5
  abbrev = param_get_abbrev(0, 1, 218)
  if (abbrev .ne. 'QZ0') stop 5
  abbrev = param_get_abbrev(0, 1, 219)
  if (abbrev .ne. 'QMAX') stop 5
  abbrev = param_get_abbrev(0, 1, 220)
  if (abbrev .ne. 'QMIN') stop 5
  abbrev = param_get_abbrev(0, 2, 210)
  if (abbrev .ne. 'GWDU') stop 5
  abbrev = param_get_abbrev(0, 2, 211)
  if (abbrev .ne. 'GWDV') stop 5
  abbrev = param_get_abbrev(0, 2, 212)
  if (abbrev .ne. 'CNVU') stop 5
  abbrev = param_get_abbrev(0, 2, 213)
  if (abbrev .ne. 'CNVV') stop 5
  abbrev = param_get_abbrev(0, 2, 214)
  if (abbrev .ne. 'WTEND') stop 5
  abbrev = param_get_abbrev(0, 2, 215)
  if (abbrev .ne. 'OMGALF') stop 5
  abbrev = param_get_abbrev(0, 2, 216)
  if (abbrev .ne. 'CNGWDU') stop 5
  abbrev = param_get_abbrev(0, 2, 217)
  if (abbrev .ne. 'CNGWDV') stop 5
  abbrev = param_get_abbrev(0, 3, 207)
  if (abbrev .ne. 'CNVUMF') stop 5
  abbrev = param_get_abbrev(0, 3, 208)
  if (abbrev .ne. 'CNVDMF') stop 5
  abbrev = param_get_abbrev(0, 3, 209)
  if (abbrev .ne. 'CNVDEMF') stop 5
  abbrev = param_get_abbrev(0, 1, 217)
  if (abbrev .ne. 'LRGMR') stop 5
  abbrev = param_get_abbrev(0, 14, 195)
  if (abbrev .ne. 'VDFOZ') stop 5
  abbrev = param_get_abbrev(0, 14, 196)
  if (abbrev .ne. 'POZ') stop 5
  abbrev = param_get_abbrev(0, 14, 197)
  if (abbrev .ne. 'TOZ') stop 5
  abbrev = param_get_abbrev(0, 14, 198)
  if (abbrev .ne. 'POZT') stop 5
  abbrev = param_get_abbrev(0, 14, 199)
  if (abbrev .ne. 'POZO') stop 5
  abbrev = param_get_abbrev(2, 0, 208)
  if (abbrev .ne. 'AKHS') stop 5
  abbrev = param_get_abbrev(2, 0, 209)
  if (abbrev .ne. 'AKMS') stop 5
  abbrev = param_get_abbrev(0, 19, 218)
  if (abbrev .ne. 'EPSR') stop 5
  abbrev = param_get_abbrev(0, 0, 192)
  if (abbrev .ne. 'SNOHF') stop 5
  abbrev = param_get_abbrev(0, 0, 204)
  if (abbrev .ne. 'TCHP') stop 5
  abbrev = param_get_abbrev(0, 19, 219)
  if (abbrev .ne. 'TPFI') stop 5
  abbrev = param_get_abbrev(0, 7, 198)
  if (abbrev .ne. 'LAI') stop 5
  abbrev = param_get_abbrev(0, 3, 210)
  if (abbrev .ne. 'LMH') stop 5
  abbrev = param_get_abbrev(0, 2, 218)
  if (abbrev .ne. 'LMV') stop 5
  abbrev = param_get_abbrev(0, 3, 0)
  if (abbrev .ne. 'PRES') stop 5
  ! abbrev = param_get_abbrev(0, 3, 1)
  ! if (abbrev .ne. 'PRMSL') stop 5
  abbrev = param_get_abbrev(0, 3, 2)
  if (abbrev .ne. 'PTEND') stop 5
  abbrev = param_get_abbrev(0, 2, 14)
  if (abbrev .ne. 'PVORT') stop 5
  abbrev = param_get_abbrev(0, 3, 3)
  if (abbrev .ne. 'ICAHT') stop 5
  abbrev = param_get_abbrev(0, 3, 4)
  if (abbrev .ne. 'GP') stop 5
  abbrev = param_get_abbrev(0, 3, 5)
  if (abbrev .ne. 'HGT') stop 5
  abbrev = param_get_abbrev(0, 3, 6)
  if (abbrev .ne. 'DIST') stop 5
  abbrev = param_get_abbrev(0, 3, 7)
  if (abbrev .ne. 'HSTDV') stop 5
  abbrev = param_get_abbrev(0, 14, 0)
  if (abbrev .ne. 'TOZNE') stop 5
  abbrev = param_get_abbrev(0, 0, 0)
  if (abbrev .ne. 'TMP') stop 5
  abbrev = param_get_abbrev(0, 0, 1)
  if (abbrev .ne. 'VTMP') stop 5
  abbrev = param_get_abbrev(0, 0, 2)
  if (abbrev .ne. 'POT') stop 5
  abbrev = param_get_abbrev(0, 0, 3)
  if (abbrev .ne. 'EPOT') stop 5
  abbrev = param_get_abbrev(0, 0, 4)
  if (abbrev .ne. 'TMAX') stop 5
  abbrev = param_get_abbrev(0, 0, 5)
  if (abbrev .ne. 'TMIN') stop 5
  abbrev = param_get_abbrev(0, 0, 6)
  if (abbrev .ne. 'DPT') stop 5
  abbrev = param_get_abbrev(0, 0, 7)
  if (abbrev .ne. 'DEPR') stop 5
  abbrev = param_get_abbrev(0, 0, 8)
  if (abbrev .ne. 'LAPR') stop 5
  abbrev = param_get_abbrev(0, 19, 0)
  if (abbrev .ne. 'VIS') stop 5
  abbrev = param_get_abbrev(0, 15, 6)
  if (abbrev .ne. 'RDSP1') stop 5
  abbrev = param_get_abbrev(0, 15, 7)
  if (abbrev .ne. 'RDSP2') stop 5
  abbrev = param_get_abbrev(0, 15, 8)
  if (abbrev .ne. 'RDSP3') stop 5
  abbrev = param_get_abbrev(0, 7, 0)
  if (abbrev .ne. 'PLI') stop 5
  abbrev = param_get_abbrev(0, 0, 9)
  if (abbrev .ne. 'TMPA') stop 5
  abbrev = param_get_abbrev(0, 3, 8)
  if (abbrev .ne. 'PRESA') stop 5
  abbrev = param_get_abbrev(0, 3, 9)
  if (abbrev .ne. 'GPA') stop 5
  abbrev = param_get_abbrev(10, 0, 0)
  if (abbrev .ne. 'WVSP1') stop 5
  abbrev = param_get_abbrev(10, 0, 1)
  if (abbrev .ne. 'WVSP2') stop 5
  abbrev = param_get_abbrev(10, 0, 2)
  if (abbrev .ne. 'WVSP3') stop 5
  abbrev = param_get_abbrev(0, 2, 0)
  if (abbrev .ne. 'WDIR') stop 5
  abbrev = param_get_abbrev(0, 2, 1)
  if (abbrev .ne. 'WIND') stop 5
  abbrev = param_get_abbrev(0, 2, 2)
  if (abbrev .ne. 'UGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 3)
  if (abbrev .ne. 'VGRD') stop 5
  abbrev = param_get_abbrev(0, 2, 4)
  if (abbrev .ne. 'STRM') stop 5
  abbrev = param_get_abbrev(0, 2, 5)
  if (abbrev .ne. 'VPOT') stop 5
  abbrev = param_get_abbrev(0, 2, 6)
  if (abbrev .ne. 'MNTSF') stop 5
  abbrev = param_get_abbrev(0, 2, 7)
  if (abbrev .ne. 'SGCVV') stop 5
  abbrev = param_get_abbrev(0, 2, 8)
  if (abbrev .ne. 'VVEL') stop 5
  abbrev = param_get_abbrev(0, 2, 9)
  if (abbrev .ne. 'DZDT') stop 5
  abbrev = param_get_abbrev(0, 2, 10)
  if (abbrev .ne. 'ABSV') stop 5
  abbrev = param_get_abbrev(0, 2, 11)
  if (abbrev .ne. 'ABSD') stop 5
  abbrev = param_get_abbrev(0, 2, 12)
  if (abbrev .ne. 'RELV') stop 5
  abbrev = param_get_abbrev(0, 2, 13)
  if (abbrev .ne. 'RELD') stop 5
  abbrev = param_get_abbrev(0, 2, 15)
  if (abbrev .ne. 'VUCSH') stop 5
  abbrev = param_get_abbrev(0, 2, 16)
  if (abbrev .ne. 'VVCSH') stop 5
  abbrev = param_get_abbrev(10, 1, 0)
  if (abbrev .ne. 'DIRC') stop 5
  abbrev = param_get_abbrev(10, 1, 1)
  if (abbrev .ne. 'SPC') stop 5
  abbrev = param_get_abbrev(10, 1, 2)
  if (abbrev .ne. 'UOGRD') stop 5
  abbrev = param_get_abbrev(10, 1, 3)
  if (abbrev .ne. 'VOGRD') stop 5
  abbrev = param_get_abbrev(0, 1, 0)
  if (abbrev .ne. 'SPFH') stop 5
  abbrev = param_get_abbrev(0, 1, 1)
  if (abbrev .ne. 'RH') stop 5
  abbrev = param_get_abbrev(0, 1, 2)
  if (abbrev .ne. 'MIXR') stop 5
  abbrev = param_get_abbrev(0, 1, 3)
  if (abbrev .ne. 'PWAT') stop 5
  abbrev = param_get_abbrev(0, 1, 4)
  if (abbrev .ne. 'VAPP') stop 5
  abbrev = param_get_abbrev(0, 1, 5)
  if (abbrev .ne. 'SATD') stop 5
  abbrev = param_get_abbrev(0, 1, 6)
  if (abbrev .ne. 'EVP') stop 5
  abbrev = param_get_abbrev(0, 6, 0)
  if (abbrev .ne. 'CICE') stop 5
  abbrev = param_get_abbrev(0, 1, 7)
  if (abbrev .ne. 'PRATE') stop 5
  abbrev = param_get_abbrev(0, 19, 2)
  if (abbrev .ne. 'TSTM') stop 5
  abbrev = param_get_abbrev(0, 1, 8)
  if (abbrev .ne. 'APCP') stop 5
  abbrev = param_get_abbrev(0, 1, 9)
  if (abbrev .ne. 'NCPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 10)
  if (abbrev .ne. 'ACPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 12)
  if (abbrev .ne. 'SRWEQ') stop 5
  abbrev = param_get_abbrev(0, 1, 13)
  if (abbrev .ne. 'WEASD') stop 5
  abbrev = param_get_abbrev(0, 1, 11)
  if (abbrev .ne. 'SNOD') stop 5
  abbrev = param_get_abbrev(0, 19, 3)
  if (abbrev .ne. 'MIXHT') stop 5
  abbrev = param_get_abbrev(10, 4, 2)
  if (abbrev .ne. 'TTHDP') stop 5
  abbrev = param_get_abbrev(10, 4, 0)
  if (abbrev .ne. 'MTHD') stop 5
  abbrev = param_get_abbrev(10, 4, 1)
  if (abbrev .ne. 'MTHA') stop 5
  abbrev = param_get_abbrev(0, 6, 1)
  if (abbrev .ne. 'TCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 2)
  if (abbrev .ne. 'CDCON') stop 5
  abbrev = param_get_abbrev(0, 6, 3)
  if (abbrev .ne. 'LCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 4)
  if (abbrev .ne. 'MCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 5)
  if (abbrev .ne. 'HCDC') stop 5
  abbrev = param_get_abbrev(0, 6, 6)
  if (abbrev .ne. 'CWAT') stop 5
  abbrev = param_get_abbrev(0, 7, 1)
  if (abbrev .ne. 'BLI') stop 5
  abbrev = param_get_abbrev(0, 1, 14)
  if (abbrev .ne. 'SNOC') stop 5
  abbrev = param_get_abbrev(0, 1, 15)
  if (abbrev .ne. 'SNOL') stop 5
  abbrev = param_get_abbrev(10, 3, 0)
  if (abbrev .ne. 'WTMP') stop 5
  abbrev = param_get_abbrev(2, 0, 0)
  if (abbrev .ne. 'LAND') stop 5
  abbrev = param_get_abbrev(10, 3, 1)
  if (abbrev .ne. 'DSLM') stop 5
  abbrev = param_get_abbrev(2, 0, 1)
  if (abbrev .ne. 'SFCR') stop 5
  abbrev = param_get_abbrev(0, 19, 1)
  if (abbrev .ne. 'ALBDO') stop 5
  abbrev = param_get_abbrev(2, 0, 2)
  if (abbrev .ne. 'TSOIL') stop 5
  abbrev = param_get_abbrev(2, 0, 3)
  if (abbrev .ne. 'SOILM') stop 5
  abbrev = param_get_abbrev(2, 0, 4)
  if (abbrev .ne. 'VEG') stop 5
  abbrev = param_get_abbrev(10, 4, 3)
  if (abbrev .ne. 'SALTY') stop 5
  abbrev = param_get_abbrev(0, 3, 10)
  if (abbrev .ne. 'DEN') stop 5
  abbrev = param_get_abbrev(2, 0, 5)
  if (abbrev .ne. 'WATR') stop 5
  abbrev = param_get_abbrev(10, 2, 0)
  if (abbrev .ne. 'ICEC') stop 5
  abbrev = param_get_abbrev(10, 2, 1)
  if (abbrev .ne. 'ICETK') stop 5
  abbrev = param_get_abbrev(10, 2, 2)
  if (abbrev .ne. 'DICED') stop 5
  abbrev = param_get_abbrev(10, 2, 3)
  if (abbrev .ne. 'SICED') stop 5
  abbrev = param_get_abbrev(10, 2, 4)
  if (abbrev .ne. 'UICE') stop 5
  abbrev = param_get_abbrev(10, 2, 5)
  if (abbrev .ne. 'VICE') stop 5
  abbrev = param_get_abbrev(10, 2, 6)
  if (abbrev .ne. 'ICEG') stop 5
  abbrev = param_get_abbrev(10, 2, 7)
  if (abbrev .ne. 'ICED') stop 5
  abbrev = param_get_abbrev(0, 1, 16)
  if (abbrev .ne. 'SNOM') stop 5
  abbrev = param_get_abbrev(10, 0, 3)
  if (abbrev .ne. 'HTSGW') stop 5
  abbrev = param_get_abbrev(10, 0, 4)
  if (abbrev .ne. 'WVDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 5)
  if (abbrev .ne. 'WVHGT') stop 5
  abbrev = param_get_abbrev(10, 0, 6)
  if (abbrev .ne. 'WVPER') stop 5
  abbrev = param_get_abbrev(10, 0, 7)
  if (abbrev .ne. 'SWDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 8)
  if (abbrev .ne. 'SWELL') stop 5
  abbrev = param_get_abbrev(10, 0, 9)
  if (abbrev .ne. 'SWPER') stop 5
  abbrev = param_get_abbrev(10, 0, 10)
  if (abbrev .ne. 'DIRPW') stop 5
  abbrev = param_get_abbrev(10, 0, 11)
  if (abbrev .ne. 'PERPW') stop 5
  abbrev = param_get_abbrev(10, 0, 12)
  if (abbrev .ne. 'DIRSW') stop 5
  abbrev = param_get_abbrev(10, 0, 13)
  if (abbrev .ne. 'PERSW') stop 5
  abbrev = param_get_abbrev(0, 4, 0)
  if (abbrev .ne. 'NSWRS') stop 5
  abbrev = param_get_abbrev(0, 5, 0)
  if (abbrev .ne. 'NLWRS') stop 5
  abbrev = param_get_abbrev(0, 4, 1)
  if (abbrev .ne. 'NSWRT') stop 5
  abbrev = param_get_abbrev(0, 5, 1)
  if (abbrev .ne. 'NLWRT') stop 5
  abbrev = param_get_abbrev(0, 5, 2)
  if (abbrev .ne. 'LWAVR') stop 5
  abbrev = param_get_abbrev(0, 4, 2)
  if (abbrev .ne. 'SWAVR') stop 5
  abbrev = param_get_abbrev(0, 4, 3)
  if (abbrev .ne. 'GRAD') stop 5
  abbrev = param_get_abbrev(0, 4, 4)
  if (abbrev .ne. 'BRTMP') stop 5
  abbrev = param_get_abbrev(0, 4, 5)
  if (abbrev .ne. 'LWRAD') stop 5
  abbrev = param_get_abbrev(0, 4, 6)
  if (abbrev .ne. 'SWRAD') stop 5
  abbrev = param_get_abbrev(0, 0, 10)
  if (abbrev .ne. 'LHTFL') stop 5
  abbrev = param_get_abbrev(0, 0, 11)
  if (abbrev .ne. 'SHTFL') stop 5
!  abbrev = param_get_abbrev(0, 2, 20)
!  if (abbrev .ne. 'BLYDP') stop 5
  abbrev = param_get_abbrev(0, 2, 17)
  if (abbrev .ne. 'UFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 18)
  if (abbrev .ne. 'VFLX') stop 5
  abbrev = param_get_abbrev(0, 2, 19)
  if (abbrev .ne. 'WMIXE') stop 5
  abbrev = param_get_abbrev(255, 255, 255)
  if (abbrev .ne. 'IMGD') stop 5
  ! abbrev = param_get_abbrev(0, 3, 1)
  ! if (abbrev .ne. 'MSLSA') stop 5
  abbrev = param_get_abbrev(0, 3, 192)
  if (abbrev .ne. 'MSLET') stop 5
  abbrev = param_get_abbrev(0, 7, 192)
  if (abbrev .ne. 'LFTX') stop 5
  abbrev = param_get_abbrev(0, 7, 193)
  if (abbrev .ne. '4LFTX') stop 5
  abbrev = param_get_abbrev(0, 3, 212)
  if (abbrev .ne. 'PRESN') stop 5
  abbrev = param_get_abbrev(0, 1, 197)
  if (abbrev .ne. 'MCONV') stop 5
  abbrev = param_get_abbrev(0, 2, 192)
  if (abbrev .ne. 'VWSH') stop 5
  abbrev = param_get_abbrev(0, 2, 219)
  if (abbrev .ne. 'PVMWW') stop 5
  abbrev = param_get_abbrev(0, 1, 192)
  if (abbrev .ne. 'CRAIN') stop 5
  abbrev = param_get_abbrev(0, 1, 193)
  if (abbrev .ne. 'CFRZR') stop 5
  abbrev = param_get_abbrev(0, 1, 194)
  if (abbrev .ne. 'CICEP') stop 5
  abbrev = param_get_abbrev(0, 1, 195)
  if (abbrev .ne. 'CSNOW') stop 5
  abbrev = param_get_abbrev(2, 0, 192)
  if (abbrev .ne. 'SOILW') stop 5
  abbrev = param_get_abbrev(0, 1, 200)
  if (abbrev .ne. 'PEVPR') stop 5
  abbrev = param_get_abbrev(2, 0, 210)
  if (abbrev .ne. 'VEGT') stop 5
  abbrev = param_get_abbrev(2, 3, 200)
  if (abbrev .ne. 'BARET') stop 5
  abbrev = param_get_abbrev(2, 3, 201)
  if (abbrev .ne. 'AVSFT') stop 5
  abbrev = param_get_abbrev(2, 3, 202)
  if (abbrev .ne. 'RADT') stop 5
  abbrev = param_get_abbrev(2, 0, 211)
  if (abbrev .ne. 'SSTOR') stop 5
  abbrev = param_get_abbrev(2, 0, 212)
  if (abbrev .ne. 'LSOIL') stop 5
  abbrev = param_get_abbrev(2, 0, 213)
  if (abbrev .ne. 'EWATR') stop 5
  abbrev = param_get_abbrev(0, 1, 22)
  if (abbrev .ne. 'CLWMR') stop 5
  abbrev = param_get_abbrev(2, 0, 193)
  if (abbrev .ne. 'GFLUX') stop 5
  abbrev = param_get_abbrev(0, 7, 7)
  if (abbrev .ne. 'CIN') stop 5
  abbrev = param_get_abbrev(0, 7, 6)
  if (abbrev .ne. 'CAPE') stop 5
  abbrev = param_get_abbrev(0, 19, 11)
  if (abbrev .ne. 'TKE') stop 5
  abbrev = param_get_abbrev(0, 19, 192)
  if (abbrev .ne. 'MXSALB') stop 5
  abbrev = param_get_abbrev(2, 3, 192)
  if (abbrev .ne. 'SOILL') stop 5
  abbrev = param_get_abbrev(0, 1, 29)
  if (abbrev .ne. 'ASNOW') stop 5
  abbrev = param_get_abbrev(0, 1, 221)
  if (abbrev .ne. 'ARAIN') stop 5
  abbrev = param_get_abbrev(2, 0, 214)
  if (abbrev .ne. 'GWREC') stop 5
  abbrev = param_get_abbrev(2, 0, 215)
  if (abbrev .ne. 'QREC') stop 5
  abbrev = param_get_abbrev(0, 1, 222)
  if (abbrev .ne. 'SNOWT') stop 5
  abbrev = param_get_abbrev(0, 4, 200)
  if (abbrev .ne. 'VBDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 201)
  if (abbrev .ne. 'VDDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 202)
  if (abbrev .ne. 'NBDSF') stop 5
  abbrev = param_get_abbrev(0, 4, 203)
  if (abbrev .ne. 'NDDSF') stop 5
  abbrev = param_get_abbrev(0, 19, 193)
  if (abbrev .ne. 'SNFALB') stop 5
  abbrev = param_get_abbrev(2, 3, 193)
  if (abbrev .ne. 'RLYRS') stop 5
  abbrev = param_get_abbrev(0, 2, 193)
  if (abbrev .ne. 'MFLX') stop 5
  abbrev = param_get_abbrev(0, 3, 210)
  if (abbrev .ne. 'LMH') stop 5
  abbrev = param_get_abbrev(0, 2, 218)
  if (abbrev .ne. 'LMV') stop 5
  abbrev = param_get_abbrev(0, 191, 195)
  if (abbrev .ne. 'MLYNO') stop 5
  abbrev = param_get_abbrev(0, 191, 192)
  if (abbrev .ne. 'NLAT') stop 5
  abbrev = param_get_abbrev(0, 191, 193)
  if (abbrev .ne. 'ELON') stop 5
  abbrev = param_get_abbrev(0, 1, 23)
  if (abbrev .ne. 'ICMR') stop 5
  abbrev = param_get_abbrev(2, 0, 228)
  if (abbrev .ne. 'ACOND') stop 5
  abbrev = param_get_abbrev(0, 1, 17)
  if (abbrev .ne. 'SNOAG') stop 5
  abbrev = param_get_abbrev(2, 0, 199)
  if (abbrev .ne. 'CCOND') stop 5
  abbrev = param_get_abbrev(0, 7, 198)
  if (abbrev .ne. 'LAI') stop 5
  abbrev = param_get_abbrev(2, 0, 216)
  if (abbrev .ne. 'SFCRH') stop 5
  abbrev = param_get_abbrev(0, 19, 19)
  if (abbrev .ne. 'SALBD') stop 5
  abbrev = param_get_abbrev(2, 0, 217)
  if (abbrev .ne. 'NDVI') stop 5
  abbrev = param_get_abbrev(2, 0, 206)
  if (abbrev .ne. 'RDRIP') stop 5
  abbrev = param_get_abbrev(2, 0, 218)
  if (abbrev .ne. 'LANDN') stop 5
  abbrev = param_get_abbrev(0, 7, 8)
  if (abbrev .ne. 'HLCY') stop 5
  abbrev = param_get_abbrev(0, 191, 196)
  if (abbrev .ne. 'NLATN') stop 5
  abbrev = param_get_abbrev(0, 191, 197)
  if (abbrev .ne. 'ELONN') stop 5
  abbrev = param_get_abbrev(1, 1, 193)
  if (abbrev .ne. 'CPOFP') stop 5
  abbrev = param_get_abbrev(0, 2, 194)
  if (abbrev .ne. 'USTM') stop 5
  abbrev = param_get_abbrev(0, 2, 195)
  if (abbrev .ne. 'VSTM') stop 5
  abbrev = param_get_abbrev(0, 1, 212)
  if (abbrev .ne. 'SBSNO') stop 5
  abbrev = param_get_abbrev(2, 3, 198)
  if (abbrev .ne. 'EVBS') stop 5
  abbrev = param_get_abbrev(2, 0, 229)
  if (abbrev .ne. 'EVCW') stop 5
  abbrev = param_get_abbrev(0, 1, 223)
  if (abbrev .ne. 'APCPN') stop 5
  abbrev = param_get_abbrev(2, 0, 200)
  if (abbrev .ne. 'RSMIN') stop 5
  abbrev = param_get_abbrev(0, 4, 192)
  if (abbrev .ne. 'DSWRF') stop 5
  abbrev = param_get_abbrev(0, 5, 192)
  if (abbrev .ne. 'DLWRF') stop 5
  abbrev = param_get_abbrev(0, 1, 224)
  if (abbrev .ne. 'ACPCPN') stop 5
  abbrev = param_get_abbrev(2, 0, 194)
  if (abbrev .ne. 'MSTAV') stop 5
  abbrev = param_get_abbrev(2, 0, 195)
  if (abbrev .ne. 'SFEXC') stop 5
  abbrev = param_get_abbrev(2, 0, 230)
  if (abbrev .ne. 'TRANS') stop 5
  abbrev = param_get_abbrev(0, 4, 193)
  if (abbrev .ne. 'USWRF') stop 5
  abbrev = param_get_abbrev(0, 5, 193)
  if (abbrev .ne. 'ULWRF') stop 5
  abbrev = param_get_abbrev(0, 6, 192)
  if (abbrev .ne. 'CDLYR') stop 5
  abbrev = param_get_abbrev(0, 1, 196)
  if (abbrev .ne. 'CPRAT') stop 5
  abbrev = param_get_abbrev(0, 0, 193)
  if (abbrev .ne. 'TTRAD') stop 5
  abbrev = param_get_abbrev(0, 3, 211)
  if (abbrev .ne. 'HGTN') stop 5
  abbrev = param_get_abbrev(2, 0, 201)
  if (abbrev .ne. 'WILT') stop 5
  abbrev = param_get_abbrev(2, 3, 203)
  if (abbrev .ne. 'FLDCP') stop 5
  abbrev = param_get_abbrev(0, 3, 196)
  if (abbrev .ne. 'HPBL') stop 5
  abbrev = param_get_abbrev(2, 3, 194)
  if (abbrev .ne. 'SLTYP') stop 5
  abbrev = param_get_abbrev(2, 0, 196)
  if (abbrev .ne. 'CNWAT') stop 5
  abbrev = param_get_abbrev(2, 3, 0)
  if (abbrev .ne. 'SOTYP') stop 5
  abbrev = param_get_abbrev(2, 0, 198)
  if (abbrev .ne. 'VGTYP') stop 5
  abbrev = param_get_abbrev(2, 0, 197)
  if (abbrev .ne. 'BMIXL') stop 5
  abbrev = param_get_abbrev(2, 0, 219)
  if (abbrev .ne. 'AMIXL') stop 5
  abbrev = param_get_abbrev(0, 1, 199)
  if (abbrev .ne. 'PEVAP') stop 5
  abbrev = param_get_abbrev(0, 0, 192)
  if (abbrev .ne. 'SNOHF') stop 5
  abbrev = param_get_abbrev(2, 3, 195)
  if (abbrev .ne. 'SMREF') stop 5
  abbrev = param_get_abbrev(2, 3, 196)
  if (abbrev .ne. 'SMDRY') stop 5
  abbrev = param_get_abbrev(2, 0, 220)
  if (abbrev .ne. 'WVINC') stop 5
  abbrev = param_get_abbrev(2, 0, 221)
  if (abbrev .ne. 'WCINC') stop 5
  abbrev = param_get_abbrev(1, 0, 192)
  if (abbrev .ne. 'BGRUN') stop 5
  abbrev = param_get_abbrev(1, 0, 193)
  if (abbrev .ne. 'SSRUN') stop 5
  abbrev = param_get_abbrev(2, 0, 222)
  if (abbrev .ne. 'WVCONV') stop 5
  abbrev = param_get_abbrev(0, 1, 201)
  if (abbrev .ne. 'SNOWC') stop 5
  abbrev = param_get_abbrev(0, 1, 208)
  if (abbrev .ne. 'SNOT') stop 5
  abbrev = param_get_abbrev(2, 3, 197)
  if (abbrev .ne. 'POROS') stop 5
  abbrev = param_get_abbrev(2, 0, 223)
  if (abbrev .ne. 'WCCONV') stop 5
  abbrev = param_get_abbrev(2, 0, 224)
  if (abbrev .ne. 'WVUFLX') stop 5
  abbrev = param_get_abbrev(2, 0, 225)
  if (abbrev .ne. 'WVVFLX') stop 5
  abbrev = param_get_abbrev(2, 0, 226)
  if (abbrev .ne. 'WCUFLX') stop 5
  abbrev = param_get_abbrev(2, 0, 227)
  if (abbrev .ne. 'WCVFLX') stop 5
  abbrev = param_get_abbrev(2, 0, 202)
  if (abbrev .ne. 'RCS') stop 5
  abbrev = param_get_abbrev(2, 0, 203)
  if (abbrev .ne. 'RCT') stop 5
  abbrev = param_get_abbrev(2, 0, 204)
  if (abbrev .ne. 'RCQ') stop 5
  abbrev = param_get_abbrev(2, 0, 205)
  if (abbrev .ne. 'RCSOL') stop 5
  abbrev = param_get_abbrev(0, 4, 197)
  if (abbrev .ne. 'SWHR') stop 5
  abbrev = param_get_abbrev(0, 5, 194)
  if (abbrev .ne. 'LWHR') stop 5
  abbrev = param_get_abbrev(0, 2, 196)
  if (abbrev .ne. 'CD') stop 5
  abbrev = param_get_abbrev(0, 2, 197)
  if (abbrev .ne. 'FRICV') stop 5
  abbrev = param_get_abbrev(0, 7, 194)
  if (abbrev .ne. 'RI') stop 5
  abbrev = param_get_abbrev(0, 1, 9)
  if (abbrev .ne. 'NCPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 10)
  if (abbrev .ne. 'ACPCP') stop 5
  abbrev = param_get_abbrev(2, 3, 203)
  if (abbrev .ne. 'FLDCP') stop 5
  abbrev = param_get_abbrev(0, 14, 200)
  if (abbrev .ne. 'OZMAX1') stop 5
  abbrev = param_get_abbrev(0, 14, 201)
  if (abbrev .ne. 'OZMAX8') stop 5
  abbrev = param_get_abbrev(0, 16, 197)
  if (abbrev .ne. 'RETOP') stop 5
  abbrev = param_get_abbrev(0, 6, 201)
  if (abbrev .ne. 'SUNSD') stop 5
  abbrev = param_get_abbrev(0, 14, 202)
  if (abbrev .ne. 'PDMAX1') stop 5
  abbrev = param_get_abbrev(0, 14, 203)
  if (abbrev .ne. 'PDMAX24') stop 5
  abbrev = param_get_abbrev(10, 3, 242)
  if (abbrev .ne. 'TCSRG20') stop 5
  abbrev = param_get_abbrev(10, 3, 243)
  if (abbrev .ne. 'TCSRG30') stop 5
  abbrev = param_get_abbrev(10, 3, 244)
  if (abbrev .ne. 'TCSRG40') stop 5
  abbrev = param_get_abbrev(10, 3, 245)
  if (abbrev .ne. 'TCSRG50') stop 5
  abbrev = param_get_abbrev(10, 3, 246)
  if (abbrev .ne. 'TCSRG60') stop 5
  abbrev = param_get_abbrev(10, 3, 247)
  if (abbrev .ne. 'TCSRG70') stop 5
  abbrev = param_get_abbrev(10, 3, 248)
  if (abbrev .ne. 'TCSRG80') stop 5
  abbrev = param_get_abbrev(10, 3, 249)
  if (abbrev .ne. 'TCSRG90') stop 5
  abbrev = param_get_abbrev(0, 3, 0)
  if (abbrev .ne. 'PRES') stop 5
  abbrev = param_get_abbrev(0, 1, 1)
  if (abbrev .ne. 'RH') stop 5
  abbrev = param_get_abbrev(0, 1, 10)
  if (abbrev .ne. 'ACPCP') stop 5
  abbrev = param_get_abbrev(0, 1, 8)
  if (abbrev .ne. 'APCP') stop 5
  abbrev = param_get_abbrev(0, 2, 10)
  if (abbrev .ne. 'ABSV') stop 5
  abbrev = param_get_abbrev(10, 0, 3)
  if (abbrev .ne. 'HTSGW') stop 5
  abbrev = param_get_abbrev(10, 0, 4)
  if (abbrev .ne. 'WVDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 6)
  if (abbrev .ne. 'WVPER') stop 5
  abbrev = param_get_abbrev(10, 0, 7)
  if (abbrev .ne. 'SWDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 8)
  if (abbrev .ne. 'SWELL') stop 5
  abbrev = param_get_abbrev(10, 0, 10)
  if (abbrev .ne. 'DIRPW') stop 5
  abbrev = param_get_abbrev(10, 0, 11)
  if (abbrev .ne. 'PERPW') stop 5
  abbrev = param_get_abbrev(10, 0, 12)
  if (abbrev .ne. 'DIRSW') stop 5
  abbrev = param_get_abbrev(10, 0, 13)
  if (abbrev .ne. 'PERSW') stop 5
  abbrev = param_get_abbrev(10, 191, 1)
  if (abbrev .ne. 'MOSF') stop 5
  abbrev = param_get_abbrev(0, 1, 225)
  if (abbrev .ne. 'FRZR') stop 5
  abbrev = param_get_abbrev(0, 1, 227)
  if (abbrev .ne. 'FROZR') stop 5
  abbrev = param_get_abbrev(0, 1, 241)
  if (abbrev .ne. 'TSNOW') stop 5
  abbrev = param_get_abbrev(2, 0, 7)
  if (abbrev .ne. 'MTERH') stop 5
  abbrev = param_get_abbrev(10, 4, 4)
  if (abbrev .ne. 'OVHD') stop 5
  abbrev = param_get_abbrev(10, 4, 5)
  if (abbrev .ne. 'OVSD') stop 5
  abbrev = param_get_abbrev(10, 4, 6)
  if (abbrev .ne. 'OVMD') stop 5
  abbrev = param_get_abbrev(0, 1, 12)
  if (abbrev .ne. 'SRWEQ') stop 5
  abbrev = param_get_abbrev(3, 192, 6)
  if (abbrev .ne. 'SBT112') stop 5
  abbrev = param_get_abbrev(3, 192, 7)
  if (abbrev .ne. 'SBT113') stop 5
  abbrev = param_get_abbrev(3, 192, 8)
  if (abbrev .ne. 'SBT114') stop 5
  abbrev = param_get_abbrev(3, 192, 9)
  if (abbrev .ne. 'SBT115') stop 5
  abbrev = param_get_abbrev(0, 16, 198)
  if (abbrev .ne. 'MAXREF') stop 5
  abbrev = param_get_abbrev(0, 7, 199)
  if (abbrev .ne. 'MXUPHL') stop 5
  abbrev = param_get_abbrev(0, 2, 220)
  if (abbrev .ne. 'MAXUVV') stop 5
  abbrev = param_get_abbrev(0, 2, 221)
  if (abbrev .ne. 'MAXDVV') stop 5
  abbrev = param_get_abbrev(0, 2, 222)
  if (abbrev .ne. 'MAXUW') stop 5
  abbrev = param_get_abbrev(0, 2, 223)
  if (abbrev .ne. 'MAXVW') stop 5
  abbrev = param_get_abbrev(0, 2, 224)
  if (abbrev .ne. 'VRATE') stop 5
  abbrev = param_get_abbrev(2, 4, 2)
  if (abbrev .ne. 'HINDEX') stop 5
  abbrev = param_get_abbrev(0, 19, 234)
  if (abbrev .ne. 'ICSEV') stop 5
  abbrev = param_get_abbrev(0, 19, 233)
  if (abbrev .ne. 'ICPRB') stop 5
  abbrev = param_get_abbrev(0, 19, 217)
  if (abbrev .ne. 'SIPD') stop 5
  abbrev = param_get_abbrev(0, 1, 242)
  if (abbrev .ne. 'RHPW') stop 5
  abbrev = param_get_abbrev(0, 15, 3)
  if (abbrev .ne. 'VIL') stop 5
  abbrev = param_get_abbrev(0, 0, 255)
  if (abbrev .ne. 'MISSING') stop 5
  abbrev = param_get_abbrev(0, 20, 102)
  if (abbrev .ne. 'AOTK') stop 5
  abbrev = param_get_abbrev(0, 20, 103)
  if (abbrev .ne. 'SSALBK') stop 5
  abbrev = param_get_abbrev(0, 20, 104)
  if (abbrev .ne. 'ASYSFK') stop 5
  abbrev = param_get_abbrev(0, 20, 105)
  if (abbrev .ne. 'AECOEF') stop 5
  abbrev = param_get_abbrev(0, 20, 106)
  if (abbrev .ne. 'AACOEF') stop 5
  abbrev = param_get_abbrev(0, 20, 107)
  if (abbrev .ne. 'ALBSAT') stop 5
  abbrev = param_get_abbrev(0, 20, 108)
  if (abbrev .ne. 'ALBGRD') stop 5
  abbrev = param_get_abbrev(0, 20, 109)
  if (abbrev .ne. 'ALESAT') stop 5
  abbrev = param_get_abbrev(0, 20, 110)
  if (abbrev .ne. 'ALEGRD') stop 5
  abbrev = param_get_abbrev(0, 20, 9)
  if (abbrev .ne. 'WLSMFLX') stop 5
  abbrev = param_get_abbrev(0, 20, 10)
  if (abbrev .ne. 'WDCPMFLX') stop 5
  abbrev = param_get_abbrev(0, 20, 11)
  if (abbrev .ne. 'SEDMFLX') stop 5
  abbrev = param_get_abbrev(0, 20, 12)
  if (abbrev .ne. 'DDMFLX') stop 5
  abbrev = param_get_abbrev(0, 20, 13)
  if (abbrev .ne. 'TRANHH') stop 5
  abbrev = param_get_abbrev(0, 20, 14)
  if (abbrev .ne. 'TRSDS') stop 5
  abbrev = param_get_abbrev(0, 20, 59)
  if (abbrev .ne. 'ANCON') stop 5
  abbrev = param_get_abbrev(0, 0, 21)
  if (abbrev .ne. 'APTMP') stop 5
  abbrev = param_get_abbrev(0, 17, 0)
  if (abbrev .ne. 'LTNGSD') stop 5
  abbrev = param_get_abbrev(0, 1, 39)
  if (abbrev .ne. 'CPOFP') stop 5
  abbrev = param_get_abbrev(10, 3, 203)
  if (abbrev .ne. 'LCH') stop 5
  abbrev = param_get_abbrev(0, 20, 101)
  if (abbrev .ne. 'ATMTK') stop 5
  abbrev = param_get_abbrev(0, 1, 37)
  if (abbrev .ne. 'CPRAT') stop 5
  abbrev = param_get_abbrev(10, 2, 8)
  if (abbrev .ne. 'ICETMP') stop 5
  abbrev = param_get_abbrev(0, 0, 28)
  if (abbrev .ne. 'UCTMP') stop 5
  abbrev = param_get_abbrev(0, 0, 29)
  if (abbrev .ne. 'TMPADV') stop 5
  abbrev = param_get_abbrev(0, 1, 129)
  if (abbrev .ne. 'EFRCWAT') stop 5
  abbrev = param_get_abbrev(0, 1, 130)
  if (abbrev .ne. 'EFRRAIN') stop 5
  abbrev = param_get_abbrev(0, 1, 131)
  if (abbrev .ne. 'EFRCICE') stop 5
  abbrev = param_get_abbrev(0, 1, 132)
  if (abbrev .ne. 'EFRSNOW') stop 5
  abbrev = param_get_abbrev(0, 1, 133)
  if (abbrev .ne. 'EFRGRL') stop 5
  abbrev = param_get_abbrev(0, 1, 134)
  if (abbrev .ne. 'EFRHAIL') stop 5
  abbrev = param_get_abbrev(0, 1, 135)
  if (abbrev .ne. 'EFRSLC') stop 5
  abbrev = param_get_abbrev(0, 1, 136)
  if (abbrev .ne. 'EFRSICEC') stop 5
  abbrev = param_get_abbrev(0, 1, 137)
  if (abbrev .ne. 'EFARRAIN') stop 5
  abbrev = param_get_abbrev(0, 1, 138)
  if (abbrev .ne. 'EFARCICE') stop 5
  abbrev = param_get_abbrev(0, 1, 139)
  if (abbrev .ne. 'EFARSNOW') stop 5
  abbrev = param_get_abbrev(0, 1, 140)
  if (abbrev .ne. 'EFARGRL') stop 5
  abbrev = param_get_abbrev(0, 1, 141)
  if (abbrev .ne. 'EFARHAIL') stop 5
  abbrev = param_get_abbrev(0, 1, 142)
  if (abbrev .ne. 'EFARSIC') stop 5
  abbrev = param_get_abbrev(0, 1, 231)
  if (abbrev .ne. 'PPINDX') stop 5
  abbrev = param_get_abbrev(0, 1, 232)
  if (abbrev .ne. 'PROBCIP') stop 5
  abbrev = param_get_abbrev(0, 1, 233)
  if (abbrev .ne. 'SNOWLR') stop 5
  abbrev = param_get_abbrev(0, 1, 234)
  if (abbrev .ne. 'PCPDUR') stop 5
  abbrev = param_get_abbrev(0, 1, 235)
  if (abbrev .ne. 'CLLMR') stop 5
  abbrev = param_get_abbrev(0, 2, 231)
  if (abbrev .ne. 'TPWDIR') stop 5
  abbrev = param_get_abbrev(0, 2, 232)
  if (abbrev .ne. 'TPWSPD') stop 5
  abbrev = param_get_abbrev(0, 2, 36)
  if (abbrev .ne. 'AFRWE') stop 5
  abbrev = param_get_abbrev(0, 3, 20)
  if (abbrev .ne. 'SDSGSO') stop 5
  abbrev = param_get_abbrev(0, 3, 21)
  if (abbrev .ne. 'AOSGSO') stop 5
  abbrev = param_get_abbrev(0, 3, 22)
  if (abbrev .ne. 'SSGSO') stop 5
  abbrev = param_get_abbrev(0, 3, 23)
  if (abbrev .ne. 'GWD') stop 5
  abbrev = param_get_abbrev(0, 3, 24)
  if (abbrev .ne. 'ASGSO') stop 5
  abbrev = param_get_abbrev(0, 3, 25)
  if (abbrev .ne. 'NLPRES') stop 5
  abbrev = param_get_abbrev(0, 3, 26)
  if (abbrev .ne. 'EXPRES') stop 5
  abbrev = param_get_abbrev(0, 3, 27)
  if (abbrev .ne. 'UMFLX') stop 5
  abbrev = param_get_abbrev(0, 3, 28)
  if (abbrev .ne. 'DMFLX') stop 5
  abbrev = param_get_abbrev(0, 3, 29)
  if (abbrev .ne. 'UDRATE') stop 5
  abbrev = param_get_abbrev(0, 3, 30)
  if (abbrev .ne. 'DDRATE') stop 5
  abbrev = param_get_abbrev(0, 3, 31)
  if (abbrev .ne. 'UCLSPRS') stop 5
  abbrev = param_get_abbrev(0, 4, 50)
  if (abbrev .ne. 'UVIUCS') stop 5
  abbrev = param_get_abbrev(0, 4, 52)
  if (abbrev .ne. 'DSWRFCS') stop 5
  abbrev = param_get_abbrev(0, 4, 53)
  if (abbrev .ne. 'USWRFCS') stop 5
  abbrev = param_get_abbrev(0, 5, 5)
  if (abbrev .ne. 'NLWRF') stop 5
  abbrev = param_get_abbrev(0, 5, 6)
  if (abbrev .ne. 'NLWRCS') stop 5
  abbrev = param_get_abbrev(0, 5, 7)
  if (abbrev .ne. 'BRTEMP') stop 5
  abbrev = param_get_abbrev(0, 5, 8)
  if (abbrev .ne. 'DLWRFCS') stop 5
  abbrev = param_get_abbrev(0, 6, 34)
  if (abbrev .ne. 'SLWTC') stop 5
  abbrev = param_get_abbrev(0, 6, 35)
  if (abbrev .ne. 'SSWTC') stop 5
  abbrev = param_get_abbrev(0, 6, 36)
  if (abbrev .ne. 'FSTRPC') stop 5
  abbrev = param_get_abbrev(0, 6, 37)
  if (abbrev .ne. 'FCONPC') stop 5
  abbrev = param_get_abbrev(0, 6, 38)
  if (abbrev .ne. 'MASSDCD') stop 5
  abbrev = param_get_abbrev(0, 6, 39)
  if (abbrev .ne. 'MASSDCI') stop 5
  abbrev = param_get_abbrev(0, 6, 40)
  if (abbrev .ne. 'MDCCWD') stop 5
  abbrev = param_get_abbrev(0, 6, 47)
  if (abbrev .ne. 'VFRCWD') stop 5
  abbrev = param_get_abbrev(0, 6, 48)
  if (abbrev .ne. 'VFRCICE') stop 5
  abbrev = param_get_abbrev(0, 6, 49)
  if (abbrev .ne. 'VFRCIW') stop 5
  abbrev = param_get_abbrev(0, 7, 19)
  if (abbrev .ne. 'CONAPES') stop 5
  abbrev = param_get_abbrev(0, 7, 203)
  if (abbrev .ne. 'DCAPE') stop 5
  abbrev = param_get_abbrev(0, 7, 204)
  if (abbrev .ne. 'EFHL') stop 5
  abbrev = param_get_abbrev(0, 7, 205)
  if (abbrev .ne. 'ESP') stop 5
  abbrev = param_get_abbrev(0, 7, 206)
  if (abbrev .ne. 'CANGLE') stop 5
  abbrev = param_get_abbrev(0, 7, 206)
  if (abbrev .ne. 'CANGLE') stop 5
  abbrev = param_get_abbrev(0, 15, 9)
  if (abbrev .ne. 'RFCD') stop 5
  abbrev = param_get_abbrev(0, 15, 10)
  if (abbrev .ne. 'RFCI') stop 5
  abbrev = param_get_abbrev(0, 15, 11)
  if (abbrev .ne. 'RFSNOW') stop 5
  abbrev = param_get_abbrev(0, 15, 12)
  if (abbrev .ne. 'RFRAIN') stop 5
  abbrev = param_get_abbrev(0, 15, 13)
  if (abbrev .ne. 'RFGRPL') stop 5
  abbrev = param_get_abbrev(0, 15, 14)
  if (abbrev .ne. 'RFHAIL') stop 5
  abbrev = param_get_abbrev(0, 15, 15)
  if (abbrev .ne. 'HSR') stop 5
  abbrev = param_get_abbrev(0, 15, 16)
  if (abbrev .ne. 'HSRHT') stop 5
  abbrev = param_get_abbrev(0, 17, 1)
  if (abbrev .ne. 'LTPINX') stop 5
  abbrev = param_get_abbrev(0, 17, 2)
  if (abbrev .ne. 'CDGDLTFD') stop 5
  abbrev = param_get_abbrev(0, 17, 3)
  if (abbrev .ne. 'CDCDLTFD') stop 5
  abbrev = param_get_abbrev(0, 17, 4)
  if (abbrev .ne. 'TLGTFD') stop 5
  abbrev = param_get_abbrev(0, 18, 0)
  if (abbrev .ne. 'ACCES') stop 5
  abbrev = param_get_abbrev(0, 18, 1)
  if (abbrev .ne. 'ACIOD') stop 5
  abbrev = param_get_abbrev(0, 18, 2)
  if (abbrev .ne. 'ACRADP') stop 5
  abbrev = param_get_abbrev(0, 19, 28)
  if (abbrev .ne. 'MWTURB') stop 5
  abbrev = param_get_abbrev(0, 19, 29)
  if (abbrev .ne. 'CATEDR') stop 5
  abbrev = param_get_abbrev(0, 19, 30)
  if (abbrev .ne. 'EDPARM') stop 5
  abbrev = param_get_abbrev(0, 19, 31)
  if (abbrev .ne. 'MXEDPRM') stop 5
  abbrev = param_get_abbrev(0, 19, 32)
  if (abbrev .ne. 'HIFREL') stop 5
  abbrev = param_get_abbrev(0, 19, 33)
  if (abbrev .ne. 'VISLFOG') stop 5
  abbrev = param_get_abbrev(0, 19, 34)
  if (abbrev .ne. 'VISIFOG') stop 5
  abbrev = param_get_abbrev(0, 19, 35)
  if (abbrev .ne. 'VISBSN') stop 5
  abbrev = param_get_abbrev(0, 19, 36)
  if (abbrev .ne. 'PSNOWS') stop 5
  abbrev = param_get_abbrev(0, 19, 37)
  if (abbrev .ne. 'ICESEV') stop 5
  abbrev = param_get_abbrev(0, 19, 238)
  if (abbrev .ne. 'ELLINX') stop 5
  abbrev = param_get_abbrev(1, 0, 7)
  if (abbrev .ne. 'DISRS') stop 5
  abbrev = param_get_abbrev(1, 0, 8)
  if (abbrev .ne. 'GWUPS') stop 5
  abbrev = param_get_abbrev(1, 0, 9)
  if (abbrev .ne. 'GWLOWS') stop 5
  abbrev = param_get_abbrev(1, 0, 10)
  if (abbrev .ne. 'SFLORC') stop 5
  abbrev = param_get_abbrev(1, 0, 11)
  if (abbrev .ne. 'RVERSW') stop 5
  abbrev = param_get_abbrev(1, 0, 12)
  if (abbrev .ne. 'FLDPSW') stop 5
  abbrev = param_get_abbrev(1, 0, 13)
  if (abbrev .ne. 'DEPWSS') stop 5
  abbrev = param_get_abbrev(1, 0, 14)
  if (abbrev .ne. 'UPAPCP') stop 5
  abbrev = param_get_abbrev(1, 0, 15)
  if (abbrev .ne. 'UPASM') stop 5
  abbrev = param_get_abbrev(1, 0, 16)
  if (abbrev .ne. 'PERRATE') stop 5
  abbrev = param_get_abbrev(1, 2, 0)
  if (abbrev .ne. 'WDPTHIL') stop 5
  abbrev = param_get_abbrev(1, 2, 1)
  if (abbrev .ne. 'WTMPIL') stop 5
  abbrev = param_get_abbrev(1, 2, 2)
  if (abbrev .ne. 'WFRACT') stop 5
  abbrev = param_get_abbrev(1, 2, 3)
  if (abbrev .ne. 'SEDTK') stop 5
  abbrev = param_get_abbrev(1, 2, 4)
  if (abbrev .ne. 'SEDTMP') stop 5
  abbrev = param_get_abbrev(1, 2, 5)
  if (abbrev .ne. 'ICTKIL') stop 5
  abbrev = param_get_abbrev(1, 2, 6)
  if (abbrev .ne. 'ICETIL') stop 5
  abbrev = param_get_abbrev(1, 2, 7)
  if (abbrev .ne. 'ICECIL') stop 5
  abbrev = param_get_abbrev(1, 2, 8)
  if (abbrev .ne. 'LANDIL') stop 5
  abbrev = param_get_abbrev(1, 2, 9)
  if (abbrev .ne. 'SFSAL') stop 5
  abbrev = param_get_abbrev(1, 2, 10)
  if (abbrev .ne. 'SFTMP') stop 5
  abbrev = param_get_abbrev(1, 2, 11)
  if (abbrev .ne. 'ACWSR') stop 5
  abbrev = param_get_abbrev(1, 2, 12)
  if (abbrev .ne. 'SALTIL') stop 5
  abbrev = param_get_abbrev(1, 2, 13)
  if (abbrev .ne. 'CSAFC') stop 5
  abbrev = param_get_abbrev(2, 0, 35)
  if (abbrev .ne. 'TCLASS') stop 5
  abbrev = param_get_abbrev(2, 0, 36)
  if (abbrev .ne. 'TFRCT') stop 5
  abbrev = param_get_abbrev(2, 0, 37)
  if (abbrev .ne. 'TPERCT') stop 5
  abbrev = param_get_abbrev(2, 0, 38)
  if (abbrev .ne. 'SOILVIC') stop 5
  abbrev = param_get_abbrev(2, 0, 39)
  if (abbrev .ne. 'EVAPTRAT') stop 5
  abbrev = param_get_abbrev(2, 1, 192)
  if (abbrev .ne. 'CANL') stop 5
  abbrev = param_get_abbrev(2, 3, 18)
  if (abbrev .ne. 'SOILTMP') stop 5
  abbrev = param_get_abbrev(2, 3, 19)
  if (abbrev .ne. 'SOILMOI') stop 5
  abbrev = param_get_abbrev(2, 3, 20)
  if (abbrev .ne. 'CISOILM') stop 5
  abbrev = param_get_abbrev(2, 3, 21)
  if (abbrev .ne. 'SOILICE') stop 5
  abbrev = param_get_abbrev(2, 3, 22)
  if (abbrev .ne. 'CISICE') stop 5
  abbrev = param_get_abbrev(2, 3, 23)
  if (abbrev .ne. 'LWSNWP') stop 5
  ! abbrev = param_get_abbrev(2, 3, 23)
  ! if (abbrev .ne. 'FRSTINX') stop 5
  ! abbrev = param_get_abbrev(2, 3, 23)
  ! if (abbrev .ne. 'SNWDEB') stop 5
  ! abbrev = param_get_abbrev(2, 3, 23)
  ! if (abbrev .ne. 'SHFLX') stop 5
  ! abbrev = param_get_abbrev(2, 3, 23)
  ! if (abbrev .ne. 'SOILDEP') stop 5
  abbrev = param_get_abbrev(2, 4, 0)
  if (abbrev .ne. 'FIREOLK') stop 5
  abbrev = param_get_abbrev(2, 4, 1)
  if (abbrev .ne. 'FIREODT') stop 5
  abbrev = param_get_abbrev(2, 4, 3)
  if (abbrev .ne. 'FBAREA') stop 5
  abbrev = param_get_abbrev(2, 4, 4)
  if (abbrev .ne. 'FOSINDX') stop 5
  abbrev = param_get_abbrev(2, 4, 5)
  if (abbrev .ne. 'FWINX') stop 5
  abbrev = param_get_abbrev(2, 4, 6)
  if (abbrev .ne. 'FFMCODE') stop 5
  abbrev = param_get_abbrev(2, 4, 7)
  if (abbrev .ne. 'DUFMCODE') stop 5
  abbrev = param_get_abbrev(2, 4, 8)
  if (abbrev .ne. 'DRTCODE') stop 5
  abbrev = param_get_abbrev(2, 4, 9)
  if (abbrev .ne. 'INFSINX') stop 5
  abbrev = param_get_abbrev(2, 4, 10)
  if (abbrev .ne. 'FBUPINX') stop 5
  abbrev = param_get_abbrev(2, 4, 11)
  if (abbrev .ne. 'FDSRTE') stop 5
  abbrev = param_get_abbrev(2, 5, 1)
  if (abbrev .ne. 'GLACTMP') stop 5
  abbrev = param_get_abbrev(3, 0, 0)
  if (abbrev .ne. 'SRAD') stop 5
  abbrev = param_get_abbrev(3, 0, 1)
  if (abbrev .ne. 'SALBEDO') stop 5
  abbrev = param_get_abbrev(3, 0, 2)
  if (abbrev .ne. 'SBTMP') stop 5
  abbrev = param_get_abbrev(3, 0, 3)
  if (abbrev .ne. 'SPWAT') stop 5
  abbrev = param_get_abbrev(3, 0, 4)
  if (abbrev .ne. 'SLFTI') stop 5
  abbrev = param_get_abbrev(3, 0, 5)
  if (abbrev .ne. 'SCTPRES') stop 5
  abbrev = param_get_abbrev(3, 0, 6)
  if (abbrev .ne. 'SSTMP') stop 5
  abbrev = param_get_abbrev(3, 0, 7)
  if (abbrev .ne. 'CLOUDM') stop 5
  abbrev = param_get_abbrev(3, 0, 8)
  if (abbrev .ne. 'PIXST') stop 5
  abbrev = param_get_abbrev(3, 0, 9)
  if (abbrev .ne. 'FIREDI') stop 5
  abbrev = param_get_abbrev(3, 1, 194)
  if (abbrev .ne. 'SWQI') stop 5
  abbrev = param_get_abbrev(3, 2, 0)
  if (abbrev .ne. 'CSKPROB') stop 5
  abbrev = param_get_abbrev(3, 192, 53)
  if (abbrev .ne. 'SBTAGR8') stop 5
  abbrev = param_get_abbrev(3, 192, 54)
  if (abbrev .ne. 'SBTAGR9') stop 5
  abbrev = param_get_abbrev(3, 192, 55)
  if (abbrev .ne. 'SBTAGR10') stop 5
  abbrev = param_get_abbrev(3, 192, 56)
  if (abbrev .ne. 'SBTAGR11') stop 5
  abbrev = param_get_abbrev(3, 192, 57)
  if (abbrev .ne. 'SBTAGR12') stop 5
  abbrev = param_get_abbrev(3, 192, 58)
  if (abbrev .ne. 'SBTAGR13') stop 5
  abbrev = param_get_abbrev(3, 192, 59)
  if (abbrev .ne. 'SBTAGR14') stop 5
  abbrev = param_get_abbrev(3, 192, 60)
  if (abbrev .ne. 'SBTAGR15') stop 5
  abbrev = param_get_abbrev(3, 192, 61)
  if (abbrev .ne. 'SBTAGR16') stop 5
  abbrev = param_get_abbrev(10, 0, 14)
  if (abbrev .ne. 'WWSDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 15)
  if (abbrev .ne. 'MWSPER') stop 5
  abbrev = param_get_abbrev(10, 0, 16)
  if (abbrev .ne. 'CDWW') stop 5
  abbrev = param_get_abbrev(10, 0, 17)
  if (abbrev .ne. 'FRICV') stop 5
  abbrev = param_get_abbrev(10, 0, 18)
  if (abbrev .ne. 'WSTR') stop 5
  abbrev = param_get_abbrev(10, 0, 19)
  if (abbrev .ne. 'NWSTR') stop 5
  abbrev = param_get_abbrev(10, 0, 20)
  if (abbrev .ne. 'MSSW') stop 5
  abbrev = param_get_abbrev(10, 0, 21)
  if (abbrev .ne. 'USSD') stop 5
  abbrev = param_get_abbrev(10, 0, 22)
  if (abbrev .ne. 'VSSD') stop 5
  abbrev = param_get_abbrev(10, 0, 23)
  if (abbrev .ne. 'PMAXWH') stop 5
  abbrev = param_get_abbrev(10, 0, 24)
  if (abbrev .ne. 'MAXWH') stop 5
  abbrev = param_get_abbrev(10, 0, 25)
  if (abbrev .ne. 'IMWF') stop 5
  abbrev = param_get_abbrev(10, 0, 26)
  if (abbrev .ne. 'IMFWW') stop 5
  abbrev = param_get_abbrev(10, 0, 27)
  if (abbrev .ne. 'IMFTSW') stop 5
  abbrev = param_get_abbrev(10, 0, 28)
  if (abbrev .ne. 'MZWPER') stop 5
  abbrev = param_get_abbrev(10, 0, 29)
  if (abbrev .ne. 'MZPWW') stop 5
  abbrev = param_get_abbrev(10, 0, 30)
  if (abbrev .ne. 'MZPTSW') stop 5
  abbrev = param_get_abbrev(10, 0, 31)
  if (abbrev .ne. 'WDIRW') stop 5
  abbrev = param_get_abbrev(10, 0, 32)
  if (abbrev .ne. 'DIRWWW') stop 5
  abbrev = param_get_abbrev(10, 0, 33)
  if (abbrev .ne. 'DIRWTS') stop 5
  abbrev = param_get_abbrev(10, 0, 34)
  if (abbrev .ne. 'PWPER') stop 5
  abbrev = param_get_abbrev(10, 0, 35)
  if (abbrev .ne. 'PPERWW') stop 5
  abbrev = param_get_abbrev(10, 0, 36)
  if (abbrev .ne. 'PPERTS') stop 5
  abbrev = param_get_abbrev(10, 0, 37)
  if (abbrev .ne. 'ALTWH') stop 5
  abbrev = param_get_abbrev(10, 0, 38)
  if (abbrev .ne. 'ALCWH') stop 5
  abbrev = param_get_abbrev(10, 0, 39)
  if (abbrev .ne. 'ALRRC') stop 5
  abbrev = param_get_abbrev(10, 0, 40)
  if (abbrev .ne. 'MNWSOW') stop 5
  abbrev = param_get_abbrev(10, 0, 41)
  if (abbrev .ne. 'MWDIRW') stop 5
  abbrev = param_get_abbrev(10, 0, 42)
  if (abbrev .ne. 'WESP') stop 5
  abbrev = param_get_abbrev(10, 0, 43)
  if (abbrev .ne. 'KSSEW') stop 5
  abbrev = param_get_abbrev(10, 0, 44)
  if (abbrev .ne. 'BENINX') stop 5
  abbrev = param_get_abbrev(10, 0, 45)
  if (abbrev .ne. 'SPFTR') stop 5
  abbrev = param_get_abbrev(10, 0, 46)
  if (abbrev .ne. 'PWAVEDIR') stop 5
  abbrev = param_get_abbrev(10, 0, 47)
  if (abbrev .ne. 'SWHFSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 48)
  if (abbrev .ne. 'SWHSSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 49)
  if (abbrev .ne. 'SWHTSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 50)
  if (abbrev .ne. 'MWPFSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 51)
  if (abbrev .ne. 'MWPSSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 52)
  if (abbrev .ne. 'MWPTSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 53)
  if (abbrev .ne. 'MWDFSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 54)
  if (abbrev .ne. 'MWDSSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 55)
  if (abbrev .ne. 'MWDTSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 56)
  if (abbrev .ne. 'WDWFSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 57)
  if (abbrev .ne. 'WDWSSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 58)
  if (abbrev .ne. 'WDWTSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 59)
  if (abbrev .ne. 'WFWFSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 60)
  if (abbrev .ne. 'WFWSSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 61)
  if (abbrev .ne. 'WFWTSWEL') stop 5
  abbrev = param_get_abbrev(10, 0, 62)
  if (abbrev .ne. 'WAVEFREW') stop 5
  abbrev = param_get_abbrev(10, 0, 63)
  if (abbrev .ne. 'FREWWW') stop 5
  abbrev = param_get_abbrev(10, 0, 64)
  if (abbrev .ne. 'FREWTSW') stop 5
  abbrev = param_get_abbrev(10, 1, 4)
  if (abbrev .ne. 'RIPCOP') stop 5
  abbrev = param_get_abbrev(10, 2, 9)
  if (abbrev .ne. 'ICEPRS') stop 5
  abbrev = param_get_abbrev(10, 2, 10)
  if (abbrev .ne. 'ZVCICEP') stop 5
  abbrev = param_get_abbrev(10, 2, 11)
  if (abbrev .ne. 'MVCICEP') stop 5
  abbrev = param_get_abbrev(10, 2, 12)
  if (abbrev .ne. 'CICES') stop 5
  abbrev = param_get_abbrev(10, 3, 2)
  if (abbrev .ne. 'CH') stop 5
  abbrev = param_get_abbrev(10, 3, 3)
  if (abbrev .ne. 'PRACTSAL') stop 5
  abbrev = param_get_abbrev(10, 3, 204)
  if (abbrev .ne. 'FRZSPR') stop 5
  abbrev = param_get_abbrev(10, 4, 13)
  if (abbrev .ne. 'ACWSRD') stop 5
  abbrev = param_get_abbrev(10, 3, 205)
  if (abbrev .ne. 'TWLWAV') stop 5
  abbrev = param_get_abbrev(10, 3, 206)
  if (abbrev .ne. 'RUNUP') stop 5
  abbrev = param_get_abbrev(10, 3, 207)
  if (abbrev .ne. 'SETUP') stop 5
  abbrev = param_get_abbrev(10, 3, 208)
  if (abbrev .ne. 'SWASH') stop 5
  ! abbrev = param_get_abbrev(10, 3, 200)
  ! if (abbrev .ne. 'TWLDT') stop 5
  abbrev = param_get_abbrev(10, 3, 210)
  if (abbrev .ne. 'TWLDC') stop 5
  abbrev = param_get_abbrev(10, 3, 250)
  if (abbrev .ne. 'ETCWL') stop 5
  abbrev = param_get_abbrev(10, 3, 251)
  if (abbrev .ne. 'TIDE') stop 5
  abbrev = param_get_abbrev(10, 3, 252)
  if (abbrev .ne. 'EROSNP') stop 5
  abbrev = param_get_abbrev(10, 3, 253)
  if (abbrev .ne. 'OWASHP') stop 5
  abbrev = param_get_abbrev(10, 4, 14)
  if (abbrev .ne. 'WDEPTH') stop 5
  abbrev = param_get_abbrev(10, 4, 15)
  if (abbrev .ne. 'WTMPSS') stop 5
  abbrev = param_get_abbrev(10, 4, 16)
  if (abbrev .ne. 'WATERDEN') stop 5
  abbrev = param_get_abbrev(10, 4, 17)
  if (abbrev .ne. 'WATDENA') stop 5
  abbrev = param_get_abbrev(10, 4, 18)
  if (abbrev .ne. 'WATPTEMP') stop 5
  abbrev = param_get_abbrev(10, 4, 19)
  if (abbrev .ne. 'WATPDEN') stop 5
  abbrev = param_get_abbrev(10, 4, 20)
  if (abbrev .ne. 'WATPDENA') stop 5
  abbrev = param_get_abbrev(10, 4, 21)
  if (abbrev .ne. 'PRTSAL') stop 5
  abbrev = param_get_abbrev(0, 16, 3)
  if (abbrev .ne. 'RETOP') stop 5
  abbrev = param_get_abbrev(1, 0, 5)
  if (abbrev .ne. 'BGRUN') stop 5
  abbrev = param_get_abbrev(1, 0, 6)
  if (abbrev .ne. 'SSRUN') stop 5
  abbrev = param_get_abbrev(2, 3, 5)
  if (abbrev .ne. 'SOILL') stop 5

 call param_all(1019, g1_table_version, g1_val, g2_discipline, g2_category, &
       g2_param_num, g2_abbrev)
  if (g1_table_version .ne. 130 .or. g1_val .ne. 160 .or. g2_discipline .ne. 2 .or. &
       g2_category .ne. 3 .or. g2_param_num .ne. 5 .or. g2_abbrev .ne. 'SOILL') stop 10

  print *, 'Writing a CSV file with all parameters...'
  open(LU, FILE='noaa_grib2_params.csv', IOSTAT = ios)
  if (ios .ne. 0) stop 50

  write(LU, *, IOSTAT = ios) 'Index, GRIB1_version, GRIB1_value, GRIB2_discipline, GRIB2_category, GRIB2_parameter'
  if (ios .ne. 0) stop 70

  ! Send a CSV list of params to a file.
  do i = 1, 2000
     call param_all(i, g1_table_version, g1_val, g2_discipline, g2_category, &
          g2_param_num, g2_abbrev)
     if (g1_table_version .eq. 0 .and. g1_val .eq. 0 .and. g2_discipline .eq. 0 .and.  g2_category .eq. 0 .and. &
          g2_param_num .eq. 0) cycle
     write(LU, '(i4, a, i6, a, i6, a, i6, a, i6, a, i6, a, a8)', IOSTAT = ios) i, ',', g1_table_version, ',', &
          g1_val, ',', g2_discipline, ',', g2_category, ',', g2_param_num, ', ', g2_abbrev
  end do

  close(LU)
  
  print *, 'SUCCESS!'
  
end program test_params
