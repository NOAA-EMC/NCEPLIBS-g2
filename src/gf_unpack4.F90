!> @file
!> @brief Unpack Section 4 ([Product Definition
!> Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect4.shtml))
!> of a GRIB2 message.
!> @author Stephen Gilbert @date 2000-05-26

!> Unpack Section 4 ([Product Definition Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect4.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 4.
!> @param[out] ipdsnum Product Definition Template Number ([Code Table 4.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table4-0.shtml)).
!> @param[out] ipdstmpl Contains the data values for the
!> Product Definition Template specified by ipdsnum. A safe dimension for this array
!> can be obtained in advance from maxvals(4), which is returned from
!> subroutine gribinfo.
!> @param[out] mappdslen Number of elements in ipdstmpl. i.e. number
!> of entries in Product Defintion Template specified by ipdsnum.
!> @param[out] coordlist Pointer to real array containing floating
!> point values intended to document the vertical discretisation
!> associated to model data on hybrid coordinate vertical
!> levels (part of Section 4). Must be deallocated by caller.
!> @param[out] numcoord Number of values in array coordlist.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 5 "GRIB" message contains an undefined Grid Definition Template.
!> - 6 memory allocation error.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
     mappdslen, coordlist, numcoord, ierr)
  use pdstemplates
  use re_alloc              !  needed for subroutine realloc
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib
  integer, intent(inout) :: iofst
  real, pointer, dimension(:) :: coordlist
  integer, pointer, dimension(:) :: ipdstmpl
  integer, intent(out) :: ipdsnum
  integer, intent(out) :: ierr, numcoord

  real(4), allocatable :: coordieee(:)
  integer, allocatable :: mappds(:)
  integer :: mappdslen
  logical needext
  integer :: lensec, nbits, newmappdslen
  integer :: istat1, istat, isign, iret, i

  ierr = 0
  nullify(ipdstmpl, coordlist)

  ! Get Length of Section.
  call g2_gbytec(cgrib, lensec, iofst, 32)
  iofst = iofst + 32
  iofst = iofst + 8     ! skip section number
  allocate(mappds(lensec))

  ! Get num of coordinate values.
  call g2_gbytec(cgrib, numcoord, iofst, 16)
  iofst = iofst + 16
  ! Get Prod. Def Template num.
  call g2_gbytec(cgrib, ipdsnum, iofst, 16)
  iofst = iofst + 16
  ! Get Product Definition Template.
  call getpdstemplate(ipdsnum, mappdslen, mappds, needext, iret)
  if (iret.ne.0) then
     ierr = 5
     if (allocated(mappds)) deallocate(mappds)
     return
  endif

  ! Unpack each value into array ipdstmpl from the the appropriate
  ! number of octets, which are specified in corresponding entries in
  ! array mappds.
  istat = 0
  if (mappdslen.gt.0) allocate(ipdstmpl(mappdslen), stat = istat)
  if (istat.ne.0) then
     ierr = 6
     nullify(ipdstmpl)
     if (allocated(mappds)) deallocate(mappds)
     return
  endif
  do i = 1, mappdslen
     nbits = iabs(mappds(i))*8
     if (mappds(i).ge.0) then
        call g2_gbytec(cgrib, ipdstmpl(i), iofst, nbits)
     else
        call g2_gbytec(cgrib, isign, iofst, 1)
        call g2_gbytec(cgrib, ipdstmpl(i), iofst + 1, nbits-1)
        if (isign.eq.1) ipdstmpl(i) = -ipdstmpl(i)
     endif
     iofst = iofst + nbits
  enddo

  ! Check to see if the Product Definition Template needs to be
  ! extended. The number of values in a specific template may vary
  ! depending on data specified in the "static" part of the template.
  if (needext) then
     call extpdstemplate(ipdsnum, ipdstmpl, newmappdslen, mappds)
     call realloc(ipdstmpl, mappdslen, newmappdslen, istat)
     ! Unpack the rest of the Product Definition Template.
     do i = mappdslen + 1, newmappdslen
        nbits = iabs(mappds(i))*8
        if (mappds(i).ge.0) then
           call g2_gbytec(cgrib, ipdstmpl(i), iofst, nbits)
        else
           call g2_gbytec(cgrib, isign, iofst, 1)
           call g2_gbytec(cgrib, ipdstmpl(i), iofst + 1, nbits-1)
           if (isign.eq.1) ipdstmpl(i) = -ipdstmpl(i)
        endif
        iofst = iofst + nbits
     enddo
     mappdslen = newmappdslen
  endif
  if (allocated(mappds)) deallocate(mappds)

  ! Get Optional list of vertical coordinate values
  ! after the Product Definition Template, if necessary.
  nullify(coordlist)
  if (numcoord .ne. 0) then
     allocate (coordieee(numcoord), stat = istat1)
     allocate(coordlist(numcoord), stat = istat)
     if ((istat1 + istat).ne.0) then
        ierr = 6
        nullify(coordlist)
        if (allocated(coordieee)) deallocate(coordieee)
        return
     endif
     call g2_gbytesc(cgrib, coordieee, iofst, 32, 0, numcoord)
     call rdieee(coordieee, coordlist, numcoord)
     deallocate (coordieee)
     iofst = iofst + (32 * numcoord)
  endif
end subroutine gf_unpack4
