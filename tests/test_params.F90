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
  abbrev = param_get_abbrev(10, 3, 209)
  if (abbrev .ne. 'TWLDT') stop 5
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

  print *, 'Testing all parameters with param_g1_to_g2()...'
  call param_g1_to_g2(1, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21

  call param_g1_to_g2(1, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(1, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(2, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(3, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(4, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(5, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(6, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(7, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(8, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(9, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(10, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(11, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(12, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(13, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(14, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(15, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(16, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(17, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(18, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(19, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(20, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(21, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(22, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(23, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(24, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(25, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(26, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(27, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(28, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(29, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(30, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(31, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(32, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(33, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(34, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(35, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(36, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(37, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(38, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(39, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(40, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(41, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(42, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(43, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(44, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(45, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 15) stop 21
  call param_g1_to_g2(46, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 16) stop 21
  call param_g1_to_g2(47, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(48, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(49, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(50, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(51, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(52, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(53, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(54, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(55, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(56, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(57, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(58, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(59, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(60, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(61, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(62, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(63, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(64, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(65, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(66, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(67, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(68, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(69, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(70, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(71, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(72, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(73, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(74, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(75, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(76, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(77, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(78, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(79, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 15) stop 21
  call param_g1_to_g2(80, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(81, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(82, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(83, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(84, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(85, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(86, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(87, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(88, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(89, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(90, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(91, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(92, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(93, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(94, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(95, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(96, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(97, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(98, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(99, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 16) stop 21
  call param_g1_to_g2(100, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(101, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(102, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(103, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(104, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(105, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(106, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(107, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(108, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(109, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(110, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(111, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(112, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(113, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(114, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(115, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(116, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(117, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(118, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(119, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(120, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(121, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(122, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(123, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 20) stop 21
  call param_g1_to_g2(124, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 17) stop 21
  call param_g1_to_g2(125, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 18) stop 21
  call param_g1_to_g2(126, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 19) stop 21
  call param_g1_to_g2(127, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(229, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(153, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 22) stop 21
  call param_g1_to_g2(140, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(141, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(142, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(143, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(214, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(135, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(194, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(228, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(136, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(172, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(196, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(197, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(252, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(253, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(130, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(204, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(211, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(205, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(212, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(213, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(132, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(157, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(156, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(190, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(131, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(158, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(176, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(177, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(234, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(235, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(144, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(155, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(207, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(208, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(223, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(226, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(154, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(222, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(145, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(146, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(147, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(148, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(221, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(230, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(160, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(171, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(219, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(222, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(224, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(225, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(230, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(231, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(238, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(240, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(131, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(132, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(133, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(134, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(135, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(136, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(137, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(138, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(139, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(140, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(159, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(170, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(170, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 24) stop 21
  call param_g1_to_g2(171, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 25) stop 21
  call param_g1_to_g2(181, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(203, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(246, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(247, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(248, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(249, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(254, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(190, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 1 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(191, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(171, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(180, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(181, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(193, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(195, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 1 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(180, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 22) stop 21
  call param_g1_to_g2(31, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(32, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(33, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(34, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(100, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(101, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(103, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(107, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(108, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(109, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(110, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(156, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 13 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(157, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 13 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(11, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(129, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(163, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 13 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(164, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 13 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(178, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 23) stop 21
  call param_g1_to_g2(179, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 32) stop 21
  call param_g1_to_g2(186, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 206) stop 21
  call param_g1_to_g2(187, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 17 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(188, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 206) stop 21
  call param_g1_to_g2(189, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 15) stop 21
  call param_g1_to_g2(198, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 207) stop 21
  call param_g1_to_g2(239, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(128, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(137, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(141, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(200, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(201, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(201, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 207) stop 21
  call param_g1_to_g2(209, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(216, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(211, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(212, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(161, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(168, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 209) stop 21
  call param_g1_to_g2(169, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(181, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(182, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(183, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(184, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(254, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(91, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(49, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(50, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(80, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(82, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(88, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(49, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(50, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(80, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(88, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(40, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(67, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(2, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(7, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(130, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(217, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(218, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 27) stop 21
  call param_g1_to_g2(161, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 29) stop 21
  call param_g1_to_g2(165, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(166, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(167, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(192, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(193, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(188, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(189, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(207, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(208, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(198, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(33, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(34, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(2, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(7, 1, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(186, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(187, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(177, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(178, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(179, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(183, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(184, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(179, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(185, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 206) stop 21
  call param_g1_to_g2(186, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 207) stop 21
  call param_g1_to_g2(187, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(177, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 209) stop 21
  call param_g1_to_g2(178, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(189, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 211) stop 21
  call param_g1_to_g2(190, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(191, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 213) stop 21
  call param_g1_to_g2(192, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 214) stop 21
  call param_g1_to_g2(149, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(188, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 211) stop 21
  call param_g1_to_g2(192, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(219, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(220, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(179, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 228) stop 21
  call param_g1_to_g2(198, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(199, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(200, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 229) stop 21
  call param_g1_to_g2(210, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 230) stop 21
  call param_g1_to_g2(182, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(241, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(242, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(168, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 20) stop 21
  call param_g1_to_g2(169, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 20) stop 21
  call param_g1_to_g2(170, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 21) stop 21
  call param_g1_to_g2(171, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 21) stop 21
  call param_g1_to_g2(172, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 22) stop 21
  call param_g1_to_g2(173, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 22) stop 21
  call param_g1_to_g2(174, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 25) stop 21
  call param_g1_to_g2(175, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(176, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(177, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(178, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(179, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(180, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(181, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(182, 140, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(76, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(104, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(105, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(106, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(102, 0, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(213, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(214, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(215, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(216, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(221, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(222, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(228, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(229, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(149, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(150, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(151, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(152, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(202, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(33, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(34, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(40, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(124, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 17) stop 21
  call param_g1_to_g2(125, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 18) stop 21
  call param_g1_to_g2(8, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(13, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(88, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(49, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(50, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(215, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(217, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(154, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(250, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(251, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(160, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(162, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(163, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(164, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(165, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(166, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(167, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(168, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(169, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(206, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(219, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(220, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 206) stop 21
  call param_g1_to_g2(244, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(246, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(243, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 213) stop 21
  call param_g1_to_g2(245, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 214) stop 21
  call param_g1_to_g2(249, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 215) stop 21
  call param_g1_to_g2(247, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(248, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 209) stop 21
  call param_g1_to_g2(202, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(232, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(233, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(231, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(202, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(203, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 232) stop 21
  call param_g1_to_g2(238, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(66, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(133, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(134, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(191, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(195, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(171, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(180, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(193, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(194, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(190, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(185, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(199, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(197, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(159, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 216) stop 21
  call param_g1_to_g2(175, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(223, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 65) stop 21
  call param_g1_to_g2(224, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 66) stop 21
  call param_g1_to_g2(225, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 67) stop 21
  call param_g1_to_g2(226, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 68) stop 21
  call param_g1_to_g2(227, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(87, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(130, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 1 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(240, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(164, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(165, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(166, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(167, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(168, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(169, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(203, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(206, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(220, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(234, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 192 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(201, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(195, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 218) stop 21
  call param_g1_to_g2(204, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 219) stop 21
  call param_g1_to_g2(205, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 220) stop 21
  call param_g1_to_g2(181, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(182, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 211) stop 21
  call param_g1_to_g2(183, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(184, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 213) stop 21
  call param_g1_to_g2(236, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 214) stop 21
  call param_g1_to_g2(154, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 215) stop 21
  call param_g1_to_g2(196, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 216) stop 21
  call param_g1_to_g2(197, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 217) stop 21
  call param_g1_to_g2(202, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 207) stop 21
  call param_g1_to_g2(209, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(219, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 209) stop 21
  call param_g1_to_g2(173, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 217) stop 21
  call param_g1_to_g2(174, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(175, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(188, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(139, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(239, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(185, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(186, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 209) stop 21
  call param_g1_to_g2(193, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 218) stop 21
  call param_g1_to_g2(229, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(194, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(185, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 219) stop 21
  call param_g1_to_g2(182, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(173, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(174, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 218) stop 21
  call param_g1_to_g2(1, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(2, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(3, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(4, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(5, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(6, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(7, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(8, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(9, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(10, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(11, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(12, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(13, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(14, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(15, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(16, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(17, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(18, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(19, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(20, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(21, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(22, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(23, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(24, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(25, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(26, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(27, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(28, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(29, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(30, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(31, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(32, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(33, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(34, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(35, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(36, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(37, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(38, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(39, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(40, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(41, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(42, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(43, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(44, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(45, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 15) stop 21
  call param_g1_to_g2(46, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 16) stop 21
  call param_g1_to_g2(47, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(48, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(49, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(50, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(51, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(52, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(53, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(54, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(55, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(56, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(57, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(58, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(59, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(60, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(61, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(62, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(63, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(64, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(65, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(66, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(67, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(68, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(69, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(70, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(71, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(72, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(73, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(74, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(75, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(76, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(77, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(78, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 14) stop 21
  call param_g1_to_g2(79, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 15) stop 21
  call param_g1_to_g2(80, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(81, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(82, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(83, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(84, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(85, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(86, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(87, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(88, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(89, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(90, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(91, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(92, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(93, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(94, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(95, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(96, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(97, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(98, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 2 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(99, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 16) stop 21
  call param_g1_to_g2(100, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(101, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(102, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(103, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(104, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(105, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(106, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(107, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(108, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(109, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(110, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(111, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(112, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(113, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(114, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(115, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(116, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(117, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(118, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(119, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(120, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(121, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(122, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(123, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 20) stop 21
  call param_g1_to_g2(124, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 17) stop 21
  call param_g1_to_g2(125, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 18) stop 21
  call param_g1_to_g2(126, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 19) stop 21
  call param_g1_to_g2(127, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 255 .or. g2cat .ne. 255 .or. g2num .ne. 255) stop 21
  call param_g1_to_g2(128, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(130, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(131, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(132, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(134, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(135, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(136, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(137, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 219) stop 21
  call param_g1_to_g2(140, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(141, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(142, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(143, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(144, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(145, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(146, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(147, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(148, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(149, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(150, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 211) stop 21
  call param_g1_to_g2(151, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(152, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 213) stop 21
  call param_g1_to_g2(153, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 22) stop 21
  call param_g1_to_g2(155, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(156, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(157, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(158, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(159, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(160, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(161, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 29) stop 21
  call param_g1_to_g2(162, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 221) stop 21
  call param_g1_to_g2(163, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 214) stop 21
  call param_g1_to_g2(164, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 215) stop 21
  call param_g1_to_g2(165, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 222) stop 21
  call param_g1_to_g2(166, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(167, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(168, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(169, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(170, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(171, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(172, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(173, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 210) stop 21
  call param_g1_to_g2(174, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 218) stop 21
  call param_g1_to_g2(175, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(176, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(177, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(178, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 23) stop 21
  call param_g1_to_g2(179, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 228) stop 21
  call param_g1_to_g2(180, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 17) stop 21
  call param_g1_to_g2(181, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(182, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(183, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 216) stop 21
  call param_g1_to_g2(184, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 19) stop 21
  call param_g1_to_g2(187, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 217) stop 21
  call param_g1_to_g2(188, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 206) stop 21
  call param_g1_to_g2(189, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 218) stop 21
  call param_g1_to_g2(190, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(191, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(192, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 191 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(194, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 1 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(196, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(197, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(198, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 212) stop 21
  call param_g1_to_g2(199, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(200, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 229) stop 21
  call param_g1_to_g2(202, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 223) stop 21
  call param_g1_to_g2(203, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(204, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(205, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(206, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 224) stop 21
  call param_g1_to_g2(207, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(208, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(210, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 230) stop 21
  call param_g1_to_g2(211, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(212, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(213, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(214, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(216, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(218, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 211) stop 21
  call param_g1_to_g2(219, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(220, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(221, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(222, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(223, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(224, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(225, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(226, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(227, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 219) stop 21
  call param_g1_to_g2(228, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(229, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(230, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 195) stop 21
  call param_g1_to_g2(231, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(232, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 220) stop 21
  call param_g1_to_g2(233, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 221) stop 21
  call param_g1_to_g2(234, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 192) stop 21
  call param_g1_to_g2(235, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 193) stop 21
  call param_g1_to_g2(237, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 222) stop 21
  call param_g1_to_g2(238, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(239, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 208) stop 21
  call param_g1_to_g2(240, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(241, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 223) stop 21
  call param_g1_to_g2(242, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 224) stop 21
  call param_g1_to_g2(243, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 225) stop 21
  call param_g1_to_g2(244, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 226) stop 21
  call param_g1_to_g2(245, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 227) stop 21
  call param_g1_to_g2(246, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(247, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(248, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 204) stop 21
  call param_g1_to_g2(249, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 205) stop 21
  call param_g1_to_g2(250, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 4 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(251, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 5 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(252, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 196) stop 21
  call param_g1_to_g2(253, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(254, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 194) stop 21
  call param_g1_to_g2(62, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(63, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(220, 131, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(231, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 200) stop 21
  call param_g1_to_g2(232, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(240, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 197) stop 21
  call param_g1_to_g2(191, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 6 .or. g2num .ne. 201) stop 21
  call param_g1_to_g2(233, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 202) stop 21
  call param_g1_to_g2(234, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 14 .or. g2num .ne. 203) stop 21
  call param_g1_to_g2(242, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 242) stop 21
  call param_g1_to_g2(243, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 243) stop 21
  call param_g1_to_g2(244, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 244) stop 21
  call param_g1_to_g2(245, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 245) stop 21
  call param_g1_to_g2(246, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 246) stop 21
  call param_g1_to_g2(247, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 247) stop 21
  call param_g1_to_g2(248, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 248) stop 21
  call param_g1_to_g2(249, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 3 .or. g2num .ne. 249) stop 21
  call param_g1_to_g2(1, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 3 .or. g2num .ne. 0) stop 21
  call param_g1_to_g2(52, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(63, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(61, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(41, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(100, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(101, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(103, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(104, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(105, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(107, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 10) stop 21
  call param_g1_to_g2(108, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 11) stop 21
  call param_g1_to_g2(109, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(110, 3, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 0 .or. g2num .ne. 13) stop 21
  call param_g1_to_g2(192, 133, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 191 .or. g2num .ne. 1) stop 21
  call param_g1_to_g2(193, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 225) stop 21
  call param_g1_to_g2(194, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 227) stop 21
  call param_g1_to_g2(195, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 241) stop 21
  call param_g1_to_g2(196, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 0 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(195, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 4) stop 21
  call param_g1_to_g2(196, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 5) stop 21
  call param_g1_to_g2(197, 128, g2disc, g2cat, g2num)
  if (g2disc .ne. 10 .or. g2cat .ne. 4 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(64, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 12) stop 21
  call param_g1_to_g2(241, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 6) stop 21
  call param_g1_to_g2(242, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 7) stop 21
  call param_g1_to_g2(243, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 8) stop 21
  call param_g1_to_g2(244, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 3 .or. g2cat .ne. 192 .or. g2num .ne. 9) stop 21
  call param_g1_to_g2(235, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 198) stop 21
  call param_g1_to_g2(236, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 7 .or. g2num .ne. 199) stop 21
  call param_g1_to_g2(237, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 220) stop 21
  call param_g1_to_g2(238, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 221) stop 21
  call param_g1_to_g2(253, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 222) stop 21
  call param_g1_to_g2(254, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 223) stop 21
  call param_g1_to_g2(241, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 2 .or. g2num .ne. 224) stop 21
  call param_g1_to_g2(250, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 2 .or. g2cat .ne. 4 .or. g2num .ne. 2) stop 21
  call param_g1_to_g2(175, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 234) stop 21
  call param_g1_to_g2(176, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 233) stop 21
  call param_g1_to_g2(236, 2, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 19 .or. g2num .ne. 217) stop 21
  call param_g1_to_g2(230, 129, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 1 .or. g2num .ne. 242) stop 21
  call param_g1_to_g2(206, 130, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 15 .or. g2num .ne. 3) stop 21
  call param_g1_to_g2(255, 255, g2disc, g2cat, g2num)
  if (g2disc .ne. 0 .or. g2cat .ne. 0 .or. g2num .ne. 255) stop 21
  ! call param_g1_to_g2(240, 129, g2disc, g2cat, g2num)
  ! if (g2disc .ne. 0 .or. g2cat .ne. 16 .or. g2num .ne. 3) stop 21
  ! call param_g1_to_g2(234, 2, g2disc, g2cat, g2num)
  ! if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 5) stop 21
  ! call param_g1_to_g2(235, 2, g2disc, g2cat, g2num)
  ! if (g2disc .ne. 1 .or. g2cat .ne. 0 .or. g2num .ne. 6) stop 21
  ! call param_g1_to_g2(160, 130, g2disc, g2cat, g2num)
  ! if (g2disc .ne. 2 .or. g2cat .ne. 3 .or. g2num .ne. 5) stop 21

  print *, 'SUCCESS!'
  
end program test_params
