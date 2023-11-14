!> @file
!> @brief Pack a data field using simple packing algorithm.
!> @author Stephen Gilbert @date 2000-06-21

!> Pack a data field using a simple packing algorithm.
!> 
!> This subroutine also fills in GRIB2 Data Representation Template 5.0
!> with the appropriate values.
!> 
!> @param[in] fld Contains the data values to pack.
!> @param[in] ndpts The number of data values in array fld.
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3.
!> - (1) Reference value - ignored on input
!> - (2) Binary Scale Factor
!> - (3) Decimal Scale Factor
!> - (4) Number of bits used to pack data, if value is > 0 and <=
!> 31. If this input value is 0 or outside above range then the num
!> of bits is calculated based on given data and scale factors.
!> - (5) Original field type - currently ignored on input Data values
!> assumed to be reals.
!> @param[out] cpack The packed data field (character*1 array).
!> @param[out] lcpack length of packed field cpack.
!>
!> @author Stephen Gilbert @date 2000-06-21
      subroutine simpack(fld,ndpts,idrstmpl,cpack,lcpack)
      use intmath
      implicit none
      
      integer,intent(in) :: ndpts
      real,intent(in) :: fld(ndpts)
      character(len=1),intent(out) :: cpack(*)
      integer,intent(inout) :: idrstmpl(*)
      integer,intent(out) :: lcpack

      real(4) :: ref,rmin4

      integer(4) :: iref
      integer :: ifld(ndpts)
      integer,parameter :: zero=0
      integer :: nbittot, nbits, maxnum, maxdif, left
      integer :: imax, imin, j
      real :: rmax, rmin, temp, dscale, bscale
      
      bscale=2.0**real(-idrstmpl(2))
      dscale=10.0**real(idrstmpl(3))
      if (idrstmpl(4).le.0.OR.idrstmpl(4).gt.31) then
         nbits=0
      else
         nbits=idrstmpl(4)
      endif

!     Find max and min values in the data
      if(ndpts<1) then
         rmin=0
         rmax=0
      else
         rmax=fld(1)
         rmin=fld(1)
         do j=2,ndpts
            if (fld(j).gt.rmax) rmax=fld(j)
            if (fld(j).lt.rmin) rmin=fld(j)
         enddo
      endif

!     If max and min values are not equal, pack up field.
!     If they are equal, we have a constant field, and the reference
!     value (rmin) is the value for each point in the field and
!     set nbits to 0.
      if (rmin.ne.rmax) then

        !  Determine which algorithm to use based on user-supplied 
        !  binary scale factor and number of bits.
        if (nbits.eq.0.AND.idrstmpl(2).eq.0) then

           !  No binary scaling and calculate minumum number of 
           !  bits in which the data will fit.
           imin=nint(rmin*dscale)
           imax=nint(rmax*dscale)
           maxdif=imax-imin
           temp=i1log2(maxdif+1)
           nbits=ceiling(temp)
           rmin=real(imin)
           !   scale data
           do j=1,ndpts
             ifld(j)=nint(fld(j)*dscale)-imin
           enddo
        elseif (nbits.ne.0.AND.idrstmpl(2).eq.0) then

           !  Use minimum number of bits specified by user and
           !  adjust binary scaling factor to accomodate data.
           rmin=rmin*dscale
           rmax=rmax*dscale
           maxnum=(2**nbits)-1
           temp=ilog2(nint(real(maxnum)/(rmax-rmin)))
           idrstmpl(2)=ceiling(-1.0*temp)
           bscale=2.0**real(-idrstmpl(2))
           !   scale data
           do j=1,ndpts
             ifld(j)=max(0,nint(((fld(j)*dscale)-rmin)*bscale))
           enddo
        elseif (nbits.eq.0.AND.idrstmpl(2).ne.0) then

           !  Use binary scaling factor and calculate minumum number of 
           !  bits in which the data will fit.
           rmin=rmin*dscale
           rmax=rmax*dscale
           maxdif=nint((rmax-rmin)*bscale)
           temp=i1log2(maxdif)
           nbits=ceiling(temp)
           !   scale data
           do j=1,ndpts
             ifld(j)=max(0,nint(((fld(j)*dscale)-rmin)*bscale))
           enddo
        elseif (nbits.ne.0.AND.idrstmpl(2).ne.0) then

           !  Use binary scaling factor and use minumum number of 
           !  bits specified by user.   Dangerous - may loose
           !  information if binary scale factor and nbits not set
           !  properly by user.
           rmin=rmin*dscale
           !   scale data
           do j=1,ndpts
             ifld(j)=max(0,nint(((fld(j)*dscale)-rmin)*bscale))
           enddo
        endif

        !  Pack data, Pad last octet with Zeros, if necessary,
        !  and calculate the length of the packed data in bytes
        call g2_sbytesc(cpack,ifld,0,nbits,0,ndpts)
        nbittot=nbits*ndpts
        left=8-mod(nbittot,8)
        if (left.ne.8) then
          call g2_sbytec(cpack,zero,nbittot,left)    ! Pad with zeros to fill Octet
          nbittot=nbittot+left
        endif
        lcpack=nbittot/8

      else
        !print *,'nbits 0'
        nbits=0
        lcpack=0
      endif


!     Fill in ref value and number of bits in Template 5.0
      rmin4 = real(rmin, 4)
      call mkieee(rmin4,ref,1)   ! ensure reference value is IEEE format
      iref=transfer(ref,iref)
      idrstmpl(1)=iref
      idrstmpl(4)=nbits
      idrstmpl(5)=0         ! original data were reals

      end
