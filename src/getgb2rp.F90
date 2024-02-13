!> @file
!> @brief Extract a grib message from a file given the index of the requested
!> field.
!> @author Stephen Gilbert @date 2003-12-31

!> Extract a grib message from a file given the index (index format 1)
!> of the requested field.
!>
!> This subroutine is maintained for backward compatibility. New code
!> should use getgb2rp2().
!>
!> The GRIB message returned can contain only the requested field
!> (extract=.true.), or the complete GRIB message originally
!> containing the desired field can be returned (extract=.false.) even
!> if other fields were included in the GRIB message.
!>
!> If the GRIB field is not found, then the return code will be
!> nonzero.
!>
!> @note For files greater than 2 GB, this subroutine will only work
!> if the message is within the first 2 GB of the file.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] cindex Index record of the grib field (see docunentation of
!> subroutine ix2gb2() for description of an index record.)
!> @param[in] extract Logical value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] gribm Returned grib message.
!> @param[out] leng Length of returned grib message in bytes.
!> @param[out] iret Return code:
!> - 0 No error.
!> - 97 Error reading grib file.
!>
!> @author Stephen Gilbert, Ed Hartnett @date 2003-12-31
subroutine getgb2rp(lugb, cindex, extract, gribm, leng, iret)
  implicit none

  integer, intent(in) :: lugb
  character(len = 1), intent(in) :: cindex(*)
  logical, intent(in) :: extract
  character(len = 1), pointer, dimension(:) :: gribm
  integer, intent(out) :: leng, iret

  interface
     subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng, iret)
       integer, intent(in) :: lugb, idxver
       character(len = 1), intent(in) :: cindex(*)
       logical, intent(in) :: extract
       character(len = 1), pointer, dimension(:) :: gribm
       integer, intent(out) :: leng, iret
     end subroutine getgb2rp2
  end interface

  call getgb2rp2(lugb, 1, cindex, extract, gribm, leng, iret)

end subroutine getgb2rp

!> Extract a grib message from a file given the version 1 or 2 index
!> of the requested field.
!>
!> The GRIB message returned can contain only the requested field
!> (extract=.true.), or the complete GRIB message originally
!> containing the desired field can be returned (extract=.false.) even
!> if other fields were included in the GRIB message.
!>
!> If the GRIB field is not found, then the return code will be
!> nonzero.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.  file
!> must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] idxver Version of index, use 1 for legacy, 2 if files
!> may be > 2 GB.
!> @param[in] cindex Index record of the grib field (see docunentation of
!> subroutine ixgb2() for description of an index record.)
!> @param[in] extract Logical value indicating whether to return a
!> GRIB2 message with just the requested field, or the entire
!> GRIB2 message containing the requested field.
!> - .true. = return grib2 message containing only the requested field.
!> - .false. = return entire grib2 message containing the requested field.
!> @param[out] gribm Returned grib message.
!> @param[out] leng Length of returned grib message in bytes.
!> @param[out] iret Return code:
!> - 0 No error.
!> - 97 Error reading grib file.
!>
!> @author Edward Hartnett, Stephen Gilbert @date Feb 13, 2024
subroutine getgb2rp2(lugb, idxver, cindex, extract, gribm, leng, iret)
  implicit none

  integer, intent(in) :: lugb, idxver
  character(len = 1), intent(in) :: cindex(*)
  logical, intent(in) :: extract
  integer, intent(out) :: leng, iret
  character(len = 1), pointer, dimension(:) :: gribm

  integer, parameter :: zero = 0
  character(len = 1), allocatable, dimension(:) :: csec2, csec6, csec7
  character(len = 4) :: ctemp
  integer :: lencur, lread, len0, ibmap, ipos, iskip
  integer :: len7, len8, len3, len4, len5, len6, len1, len2
  integer :: iskp2, iskp6, iskp7

  iret = 0

  ! Extract grib message from file.
  if (extract) then
     len0 = 16
     len8 = 4
     call g2_gbytec(cindex, iskip, 4*8, 4*8)    ! bytes to skip in file
     call g2_gbytec(cindex, iskp2, 8*8, 4*8)    ! bytes to skip for section 2
     if (iskp2 .gt. 0) then
        call baread(lugb, iskip + iskp2, 4, lread, ctemp)
        call g2_gbytec(ctemp, len2, 0, 4*8)      ! length of section 2
        allocate(csec2(len2))
        call baread(lugb, iskip + iskp2, len2, lread, csec2)
     else
        len2 = 0
     endif
     call g2_gbytec(cindex, len1, 44*8, 4*8)      ! length of section 1
     ipos = 44 + len1
     call g2_gbytec(cindex, len3, ipos*8, 4*8)      ! length of section 3
     ipos = ipos + len3
     call g2_gbytec(cindex, len4, ipos*8, 4*8)      ! length of section 4
     ipos = ipos + len4
     call g2_gbytec(cindex, len5, ipos*8, 4*8)      ! length of section 5
     ipos = ipos + len5
     call g2_gbytec(cindex, len6, ipos*8, 4*8)      ! length of section 6
     ipos = ipos + 5
     call g2_gbytec(cindex, ibmap, ipos*8, 1*8)      ! bitmap indicator
     if (ibmap .eq. 254) then
        call g2_gbytec(cindex, iskp6, 24*8, 4*8)    ! bytes to skip for section 6
        call baread(lugb, iskip + iskp6, 4, lread, ctemp)
        call g2_gbytec(ctemp, len6, 0, 4*8)      ! length of section 6
     endif

     !  read in section 7 from file
     call g2_gbytec(cindex, iskp7, 28*8, 4*8)    ! bytes to skip for section 7
     call baread(lugb, iskip + iskp7, 4, lread, ctemp)
     call g2_gbytec(ctemp, len7, 0, 4*8)      ! length of section 7
     allocate(csec7(len7))
     call baread(lugb, iskip + iskp7, len7, lread, csec7)

     leng = len0 + len1 + len2 + len3 + len4 + len5 + len6 + len7 + len8
     if (.not. associated(gribm)) allocate(gribm(leng))

     ! Create Section 0
     gribm(1) = 'G'
     gribm(2) = 'R'
     gribm(3) = 'I'
     gribm(4) = 'B'
     gribm(5) = char(0)
     gribm(6) = char(0)
     gribm(7) = cindex(42)
     gribm(8) = cindex(41)
     gribm(9) = char(0)
     gribm(10) = char(0)
     gribm(11) = char(0)
     gribm(12) = char(0)
     call g2_sbytec(gribm, leng, 12*8, 4*8)

     ! Copy Section 1
     gribm(17:16 + len1) = cindex(45:44 + len1)
     lencur = 16 + len1
     ipos = 44 + len1

     ! Copy Section 2, if necessary
     if (iskp2 .gt. 0) then
        gribm(lencur + 1:lencur + len2) = csec2(1:len2)
        lencur = lencur + len2
     endif

     ! Copy Sections 3 through 5
     gribm(lencur + 1:lencur + len3 + len4 + len5) = cindex(ipos + 1:ipos + len3 + len4 + len5)
     lencur = lencur + len3 + len4 + len5
     ipos = ipos + len3 + len4 + len5

     ! Copy Section 6
     if (len6 .eq. 6 .and. ibmap .ne. 254) then
        gribm(lencur + 1:lencur + len6) = cindex(ipos + 1:ipos + len6)
        lencur = lencur + len6
     else
        call g2_gbytec(cindex, iskp6, 24*8, 4*8)    ! bytes to skip for section 6
        call baread(lugb, iskip + iskp6, 4, lread, ctemp)
        call g2_gbytec(ctemp, len6, 0, 4*8)      ! length of section 6
        allocate(csec6(len6))
        call baread(lugb, iskip + iskp6, len6, lread, csec6)
        gribm(lencur + 1:lencur + len6) = csec6(1:len6)
        lencur = lencur + len6
        if (allocated(csec6)) deallocate(csec6)
     endif

     ! Copy Section 7
     gribm(lencur + 1:lencur + len7) = csec7(1:len7)
     lencur = lencur + len7

     ! Section 8
     gribm(lencur + 1) = '7'
     gribm(lencur + 2) = '7'
     gribm(lencur + 3) = '7'
     gribm(lencur + 4) = '7'

     !  clean up
     if (allocated(csec2)) deallocate(csec2)
     if (allocated(csec7)) deallocate(csec7)
  else    ! do not extract field from message :  get entire message
     call g2_gbytec(cindex, iskip, 4*8, 4*8)    ! bytes to skip in file
     call g2_gbytec(cindex, leng, 36*8, 4*8)      ! length of grib message
     if (.not. associated(gribm)) allocate(gribm(leng))
     call baread(lugb, iskip, leng, lread, gribm)
     if (leng .ne. lread ) then
        deallocate(gribm)
        nullify(gribm)
        iret = 97
        return
     endif
  endif
end subroutine getgb2rp2
