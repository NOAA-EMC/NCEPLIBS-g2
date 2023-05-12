!> @file
!> @brief Finalize a GRIB2 message after all grids and fields have
!> been added.
!> @author Stephen Gilbert @date 2000-05-02

!> Finalize a GRIB2 message after all grids and fields have
!> been added.
!>
!> This subroutine adds the End Section ("7777") to the end of the
!> GRIB message and calculates the length and stores it in the
!> appropriate place in Section 0. This routine is used with routines
!> gribcreate(), addlocal(), addgrid(), and addfield() to create a
!> complete GRIB2 message.
!>
!> @param[inout] cgrib Character array to contain the GRIB2 message.
!> @param[in] lcgrib Maximum Length (in bytes) of array cgrib.
!> @param[out] lengrib Length of the final GRIB2 message in bytes.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 GRIB message was not initialized - call routine gribcreate() first.
!> - 2 GRIB message already complete.
!> - 3 Sum of Section byte counts doesn't add to total byte count.
!> - 4 Previous Section was not 7.
!>
!> @author Stephen Gilbert @date 2000-05-02
subroutine gribend(cgrib, lcgrib, lengrib, ierr)
  implicit none

  character(len = 1), intent(inout) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(out) :: lengrib, ierr

  integer ilen, isecnum
  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4):: ctemp
  integer iofst, lencurr, len

  ierr = 0

  ! Check to see if beginning of GRIB message exists.
  ctemp = cgrib(1) // cgrib(2) // cgrib(3) // cgrib(4)
  if (ctemp .ne. grib) then
     print *, 'gribend: GRIB not found in given message.'
     ierr = 1
     return
  endif

  ! Get current length of GRIB message.
  call g2_gbytec(cgrib, lencurr, 96, 32)

  ! Loop through all current sections of the GRIB message to
  ! find the last section number.
  len = 16                    ! Length of Section 0
  do
     ! Get number and length of next section.
     iofst = len * 8
     call g2_gbytec(cgrib, ilen, iofst, 32)
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8)
     len = len + ilen

     ! Exit loop if last section reached.
     if (len .eq. lencurr) exit

     ! If byte count for each section doesn't match current total
     ! length, then there is a problem.
     if (len .gt. lencurr) then
        print *, 'gribend: Section byte counts don''t add ' &
             ,'to total.'
        print *, 'gribend: Sum of section byte counts = ', len
        print *, 'gribend: Total byte count in Section 0 = ', &
             lencurr
        ierr = 3
        return
     endif
  enddo

  ! Can only add End Section (Section 8) after Section 7.
  if (isecnum .ne. 7) then
     print *, 'gribend: Section 8 can only be added after Section 7.'
     print *, 'gribend: Section ', isecnum, &
          ' was the last found in',' given GRIB message.'
     ierr = 4
     return
  endif

  ! Add Section 8  - End Section.
  cgrib(lencurr + 1:lencurr + 4) = c7777

  ! Update current byte total of message in Section 0.
  lengrib = lencurr + 4
  call g2_sbytec(cgrib, lengrib, 96, 32)
end subroutine gribend
