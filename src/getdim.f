!> @file
!> @brief This subroutine returns the info of GRIB2 Grid Definition
!> Section 3 format.
!> @author Stephen Gilbert @date 2002-12-11

!> This subroutine returns the dimensions and scanning mode of a grid
!> definition packed in GRIB2 Grid Definition Section 3 format.
!>
!> @param[in] csec3 Character array that contains the packed GRIB2
!> GDS.
!> @param[in] lcsec3 Length (in octets) of section 3.
!> @param[out] width x (or i) dimension of the grid.
!> @param[out] height y (or j) dimension of the grid.
!> @param[out] iscan Scanning mode (see [Code Table 3.4]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table3-4.shtml)).
!>
!> @note Returns width and height set to zero, if grid template not
!> recognized.
!>
!> @author Stephen Gilbert @date 2002-12-11
      subroutine getdim(csec3,lcsec3,width,height,iscan)
!      use grib_mod
    
      character(len=1),intent(in) :: csec3(*)
      integer,intent(in) :: lcsec3
      integer,intent(out) :: width,height,iscan
      
      integer,pointer,dimension(:) :: igdstmpl,list_opt
      integer :: igds(5)
      integer iofst,igdtlen,num_opt,jerr

      interface
         subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                         mapgridlen,ideflist,idefnum,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: igdstmpl,ideflist
            integer,intent(out) :: igds(5)
            integer,intent(out) :: ierr,idefnum
         end subroutine gf_unpack3
      end interface

      nullify(igdstmpl,list_opt)
        !
      iofst=0       ! set offset to beginning of section
      call gf_unpack3(csec3,lcsec3,iofst,igds,igdstmpl,
     &                 igdtlen,list_opt,num_opt,jerr)
      if (jerr.eq.0) then
         selectcase( igds(5) )     !  Template number
           case (0:3)   ! Lat/Lon
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(19)
           case (10)   ! Mercator
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(16)
           case (20)   ! Polar Stereographic
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(18)
           case (30)   ! Lambert Conformal
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(18)
           case (40:43)   ! Gaussian
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(19)
           case (90)   ! Space View/Orthographic
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(17)
           case (110)   ! Equatorial Azimuthal
              width=igdstmpl(8)
              height=igdstmpl(9)
              iscan=igdstmpl(16)
           case default
              width=0
              height=0
              iscan=0
         end select
      else
         width=0
         height=0
      endif
        !
      if (associated(igdstmpl)) deallocate(igdstmpl)
      if (associated(list_opt)) deallocate(list_opt)

      return
      end
