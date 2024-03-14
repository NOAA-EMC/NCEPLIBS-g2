!> @file
!> @brief Pack/unpack a data field into a JPEG2000 code stream as defined in
!> [Data Representation Template
!> 5.40](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-40.shtml).
!> @author Stephen Gilbert @date 2002-12-17

!> Pack a data field into a JPEG2000 code stream as defined in
!> [Data Representation Template
!> 5.40](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-40.shtml).
!>
!> After the data field is scaled, and the reference value is
!> subtracted out, it is treated as a grayscale image and passed to a
!> JPEG2000 encoder. This subroutine also fills in the DRT values.
!>
!> @param[in] fld The data values to pack.
!> @param[in] width number of points in the x direction
!> @param[in] height number of points in the y direction
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3
!> - idrstmpl(1) Reference value - ignored on input.
!> - idrstmpl(2) Binary Scale Factor.
!> - idrstmpl(3) Decimal Scale Factor.
!> - idrstmpl(4) Number of bits containing each grayscale pixel value
!> - idrstmpl(5) Original field type, currently set = 0 on output
!> Data values assumed to be reals.
!> - idrstmpl(6) = 0 use lossless compression; = 1 use lossy compression.
!> - idrstmpl(7) Desired compression ratio, if idrstmpl(6)=1.
!> @param[out] cpack The packed data field (character*1 array).
!> @param[inout] lcpack When function is called, contains the length
!> of buffer cpack. After functions returns, contains the length of
!> the packed data in bytes.
!>
!> @author Stephen Gilbert @date 2002-12-17
subroutine jpcpack(fld,width,height,idrstmpl,cpack,lcpack)
  use g2bytes
  implicit none
  
  integer,intent(in) :: width,height
  real,intent(in) :: fld(width*height)
  character(len=1),intent(out) :: cpack(*)
  integer,intent(inout) :: idrstmpl(*)
  integer,intent(inout) :: lcpack
  integer :: ndpts, nbits, maxdif, imin, imax, j, nbytes
  real :: dscale, bscale, temp

  interface
     function enc_jpeg2000(cin, width, height, nbits, ltype, ratio, retry, outjpc, jpclen) &
          bind(c, name="g2c_enc_jpeg2000")
       use iso_c_binding
       character(kind = c_char), intent(in) :: cin(*)       
       integer(c_int), value, intent(in) :: width, height, nbits, ltype, ratio, retry
       character(kind = c_char), intent(inout) :: outjpc(*)
       integer(c_size_t), value, intent(in) :: jpclen
       integer(c_int) :: enc_jpeg2000
     end function enc_jpeg2000
  end interface

  real(4) :: ref, rmin4(1)
  real(8) :: rmin, rmax
  integer(4) :: iref
  integer(8) :: nsize
  integer :: ifld(width * height), retry
  integer, parameter :: zero = 0
  character(len = 1), allocatable :: ctemp(:)

  ndpts = width * height
  bscale = 2.0**real(-idrstmpl(2))
  dscale = 10.0**real(idrstmpl(3))

  ! Find max and min values in the data.
  if (ndpts > 0) then
     rmax = fld(1)
     rmin = fld(1)
  else
     rmax = 1.0
     rmin = 1.0
  endif
  do j = 2, ndpts
     if (fld(j) .gt. rmax) rmax = fld(j)
     if (fld(j) .lt. rmin) rmin = fld(j)
  enddo
  if (idrstmpl(2) .eq. 0) then
     maxdif = nint(rmax * dscale) - nint(rmin * dscale)
  else
     maxdif = nint((rmax - rmin) * dscale * bscale)
  endif

  ! If max and min values are not equal, pack up field. If they are
  ! equal, we have a constant field, and the reference value (rmin) is
  ! the value for each point in the field and set nbits to 0.
  if (rmin .ne. rmax .AND. maxdif .ne. 0) then
     ! Determine which algorithm to use based on user-supplied binary
     ! scale factor and number of bits.
     if (idrstmpl(2) .eq. 0) then
        ! No binary scaling and calculate minimum number of bits in
        ! which the data will fit.
        imin = nint(rmin * dscale)
        imax = nint(rmax * dscale)
        maxdif = imax - imin
        temp = alog(real(maxdif + 1)) / alog(2.0)
        nbits = ceiling(temp)
        rmin = real(imin)
        ! Scale data.
        do j = 1, ndpts
           ifld(j) = nint(fld(j) * dscale) - imin
        enddo
     else
        ! Use binary scaling factor and calculate minimum number of
        ! bits in which the data will fit.
        rmin = rmin * dscale
        rmax = rmax * dscale
        maxdif = nint((rmax - rmin) * bscale)
        temp = alog(real(maxdif + 1)) / alog(2.0)
        nbits = ceiling(temp)
        !   scale data
        do j = 1, ndpts
           ifld(j) = max(0, nint(((fld(j) * dscale) - rmin) * bscale))
        enddo
     endif

     ! Pack data into full octets, then do JPEG2000 encode.  and
     ! calculate the length of the packed data in bytes
     retry = 0
     nbytes = (nbits + 7) / 8
     nsize = lcpack      ! needed for input to enc_jpeg2000
     allocate(ctemp(nbytes * ndpts))
     call g2_sbytesc(ctemp, ifld, 0, nbytes * 8, 0, ndpts)
     lcpack = enc_jpeg2000(ctemp, width, height, nbits, idrstmpl(6), &
          idrstmpl(7), retry, cpack, nsize)
     if (lcpack .le. 0) then
        print *,'jpcpack: ERROR Packing JPC = ',lcpack
        if (lcpack .eq. -3) then
           retry = 1
           print *,'jpcpack: Retrying....'
           lcpack = enc_jpeg2000(ctemp, width, height, nbits, idrstmpl(6), &
                idrstmpl(7), retry, cpack, nsize)
           if (lcpack .le. 0) then
              print *,'jpcpack: Retry Failed.'
           else
              print *,'jpcpack: Retry Successful.'
           endif
        endif
     endif
     deallocate(ctemp)
  else
     nbits = 0
     lcpack = 0
  endif

  ! Fill in ref value and number of bits in Template 5.0.
  rmin4(1) = real(rmin, 4)
  call mkieee(rmin4, ref, 1)   ! ensure reference value is IEEE format.
  iref = transfer(ref, iref)
  idrstmpl(1) = iref
  idrstmpl(4) = nbits
  idrstmpl(5) = 0         ! original data were reals.
  if (idrstmpl(6) .eq. 0) idrstmpl(7) = 255       ! lossy not used

end subroutine

!> Unpack a data field from a JPEG2000 code stream as defined in
!> [Data Representation Template
!> 5.40](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp5-40.shtml).
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
  implicit none
  
  character(len=1),intent(in) :: cpack(len)
  integer,intent(in) :: ndpts,len
  integer,intent(in) :: idrstmpl(*)
  real,intent(out) :: fld(ndpts)

  integer :: ifld(ndpts)
  integer(4) :: ieee
  integer(8) :: len8
  real :: ref,bscale,dscale
  integer :: nbits, j, iret

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

end subroutine jpcunpack
