!>    @file
!>    @brief This subroutine returns the J, K, and M pentagonal
!>    resolution parameters specified in a GRIB Grid Definition Section.
!>    @author Stephen Gilbert @date 2002-12-11
!>

!>    This subroutine returns the J, K, and M pentagonal resolution
!>    parameters specified in a GRIB Grid Definition Section used
!>    spherical harmonic coefficients using GDT 5.50 through 5.53.
!>    
!>    @param[in] csec3 Character array containing the packed GRIB2 GDS
!>    @param[in] lcsec3 Length (in octets) of section 3
!>    @param[out] JJ =J pentagonal resolution parameter
!>    @param[out] KK =K pentagonal resolution parameter
!>    @param[out] MM =M pentagonal resolution parameter
!>
!>    @note Returns JJ, KK, and MM set to zero, if grid template not
!>    recognized.
!>
!>    @author Stephen Gilbert @date 2002-12-11
!>

      subroutine getpoly(csec3,lcsec3,jj,kk,mm)

!      use grib_mod
    
      character(len=1),intent(in) :: csec3(*)
      integer,intent(in) :: lcsec3
      integer,intent(out) :: jj,kk,mm
      
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
           case (50:53)   ! Spherical harmonic coefficients
              jj=igdstmpl(1)
              kk=igdstmpl(2)
              mm=igdstmpl(3)
           case default
              jj=0
              kk=0
              mm=0
         end select
      else
         jj=0
         kk=0
         mm=0
      endif
        !
      if (associated(igdstmpl)) deallocate(igdstmpl)
      if (associated(list_opt)) deallocate(list_opt)

      return
      end
