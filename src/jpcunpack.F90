!> @file
!> @brief Unpack a data field from a JPEG2000 code stream.
!> @author Stephen Gilbert @date 2002-12-17

!> Unpack a data field from a JPEG2000 code stream.
!>
!> This subroutine unpacks a data field that was packed into a
!> JPEG2000 code stream using info from the GRIB2 Data Representation
!> Template 5.40 or 5.40000.
!>
!> @param[in] cpack The packed data field (character*1 array).
!> @param[in] len length of packed field cpack().
!> @param[in] idrstmpl Array of values for Data Representation
!> Template 5.40 or 5.40000.
!> @param[in] ndpts The number of data values to unpack.
!> @param[out] fld Contains the unpacked data values.
!>
!> @author Stephen Gilbert @date 2002-12-17
      subroutine jpcunpack(cpack,len,idrstmpl,ndpts,fld)

      character(len=1),intent(in) :: cpack(len)
      integer,intent(in) :: ndpts,len
      integer,intent(in) :: idrstmpl(*)
      real,intent(out) :: fld(ndpts)

      integer :: ifld(ndpts)
      integer(4) :: ieee
      integer(8) :: len8
      real :: ref,bscale,dscale

      interface
         function dec_jpeg2000(cin, len, ifld) &
              bind(c, name="g2c_dec_jpeg2000")
           use iso_c_binding
           character(kind = c_char), intent(in) :: cin(*)       
           integer(c_size_t), value, intent(in) :: len
           integer(c_int), intent(inout) :: ifld(*)
           integer(c_int) :: dec_jpeg2000
         end function dec_jpeg2000
      end interface

      ieee = idrstmpl(1)
      call rdieee(ieee,ref,1)
      bscale = 2.0**real(idrstmpl(2))
      dscale = 10.0**real(-idrstmpl(3))
      nbits = idrstmpl(4)

      !  if nbits equals 0, we have a constant field where the reference value
      !  is the data value at each gridpoint
      if (nbits.ne.0) then
         !         call g2_gbytesc(cpack,ifld,0,nbits,0,ndpts)
         len8 = len
         iret=dec_jpeg2000(cpack,len8,ifld)
         do j=1,ndpts
           fld(j)=((real(ifld(j))*bscale)+ref)*dscale
         enddo
      else
         do j=1,ndpts
           fld(j)=ref
         enddo
      endif

      end
