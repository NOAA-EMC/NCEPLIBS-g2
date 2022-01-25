!> @file
!> @brief This subroutine packs up a data field into PNG image format.
!> @author Stephen Gilbert @date 2002-12-21

!> This subroutine packs up a data field into PNG image format. After
!> the data field is scaled, and the reference value is subtracted out,
!> it is treated as a grayscale image and passed to a PNG encoder. It
!> also fills in GRIB2 Data Representation Template 5.41 or 5.40010
!> with the appropriate values.
!>
!> @param[in] fld Contains the data values to pack.
!> @param[in] width number of points in the x direction.
!> @param[in] height number of points in the y direction.
!> @param[inout] idrstmpl Contains the array of values for Data
!> Representation Template 5.2 or 5.3.
!> - idrstmpl(1) Reference value - ignored on input.
!> - idrstmpl(2) Binary Scale Factor.
!> - idrstmpl(3) Decimal Scale Factor.
!> - idrstmpl(4) Number of bits containing each grayscale pixel value.
!> - idrstmpl(5) Original field type, currently set = 0 on output
!> Data values assumed to be reals.
!> - idrstmpl(6) = 0 use lossless compression; = 1 use lossy compression.
!> - idrstmpl(7) Desired compression ratio, if idrstmpl(6)=1.
!> @param[out] cpack The packed data field (character*1 array)
!> @param[out] lcpack length of packed field cpack.
!>
!> @author Stephen Gilbert @date 2002-12-21
subroutine pngpack(fld, width, height, idrstmpl, cpack, lcpack)

  integer, intent(in) :: width, height
  real, intent(in) :: fld(width * height)
  character(len = 1), intent(out) :: cpack(*)
  integer, intent(inout) :: idrstmpl(*)
  integer, intent(out) :: lcpack

  real(4) :: ref, rmin4
  real(8) :: rmin, rmax
  integer(4) :: iref
  integer :: ifld(width * height), nbits
  integer :: enc_png
  character(len = 1), allocatable :: ctemp(:)

  ndpts = width * height
  bscale = 2.0**real(-idrstmpl(2))
  dscale = 10.0**real(idrstmpl(3))

  !  Find max and min values in the data
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
  maxdif = nint((rmax - rmin) * dscale * bscale)

  !  If max and min values are not equal, pack up field.
  !  If they are equal, we have a constant field, and the reference
  !  value (rmin) is the value for each point in the field and
  !  set nbits to 0.
  if (rmin .ne. rmax .AND. maxdif .ne. 0) then

     !  Determine which algorithm to use based on user-supplied
     !  binary scale factor and number of bits.
     if (idrstmpl(2) .eq. 0) then

        !  No binary scaling and calculate minimum number of
        !  bits in which the data will fit.
        imin = nint(rmin * dscale)
        imax = nint(rmax * dscale)
        maxdif = imax - imin
        temp = alog(real(maxdif+1)) / alog(2.0)
        nbits = ceiling(temp)
        rmin = real(imin)
        !   scale data
        do j = 1, ndpts
           ifld(j) = nint(fld(j) * dscale) - imin
        enddo
     else

        !  Use binary scaling factor and calculate minimum number of
        !  bits in which the data will fit.
        rmin = rmin * dscale
        rmax = rmax * dscale
        maxdif = nint((rmax - rmin) * bscale)
        temp = alog(real(maxdif+1)) / alog(2.0)
        nbits = ceiling(temp)
        !   scale data
        do j = 1, ndpts
           ifld(j) = max(0, nint(((fld(j) * dscale) - rmin) * bscale))
        enddo
     endif
     dd = 2
     !  Pack data into full octets, then do PNG encode.
     !  and calculate the length of the packed data in bytes
     if (nbits .le. 8) then
        nbits = 8
     elseif (nbits .le. 16) then
        nbits = 16
     elseif (nbits .le. 24) then
        nbits = 24
     else
        nbits = 32
     endif
     nbytes = (nbits / 8) * ndpts
     allocate(ctemp(nbytes))
     call g2_sbytesc(ctemp, ifld, 0, nbits, 0, ndpts)

     ! Encode data into PNG Format.
     lcpack = enc_png(ctemp, width, height, nbits, cpack)
     if (lcpack .le. 0) then
        print *,'pngpack: ERROR Encoding PNG = ', lcpack
     endif
     deallocate(ctemp)

  else
     nbits = 0
     lcpack = 0
  endif


  ! Fill in ref value and number of bits in Template 5.0
  rmin4 = rmin
  call mkieee(rmin4, ref, 1)  ! Ensure reference value is IEEE format
  ! call g2_gbytec(ref,idrstmpl(1),0,32)
  iref = transfer(ref, iref)
  idrstmpl(1) = iref
  idrstmpl(4) = nbits
  idrstmpl(5) = 0         ! original data were reals

  return
end subroutine pngpack
