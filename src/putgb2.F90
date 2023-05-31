!> @file
!> @brief Packs a single field into a GRIB2 message and
!> writes out that message to the file.
!>
!> @author Stephen Gilbert @date 2002-01-11

!> This subroutine packs a single field into a grib2 message and
!> writes out that message to the file associated with unit lugb.
!> Note that file/unit lugb should be opened with a call to subroutine
!> [baopen() or baopenw()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before this routine is
!> called.
!>
!> The information to be packed into the grib field is stored in a
!> derived type variable, gfld. gfld is of type gribfield, which is
!> defined in module grib_mod, so users of this routine will need to
!> include the line "use grib_mod" in their calling routine. Each
!> component of the gribfield type is described in the input argument
!> list section below.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2002-04-22 | Stephen Gilbert | Initial.
!> 2005-02-28 | Stephen Gilbert | Changed dim of array cgrib to be a multiple of gfld%ngrdpts instead of gfld%ndpts.
!> 2009-03-10 | Boi Vuong | Initialize variable coordlist.
!> 2011-06-09 | Boi Vuong | Initialize variable gfld%list_opt.
!> 2012-02-28 | Boi Vuong | Initialize variable ilistopt.
!> 2022-07-19 | Ed Hartnett | Fixed memory problem.
!>
!> @param[in] lugb integer unit of the unblocked grib data file.  File
!> must be opened with [baopen() or baopenw()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling this
!> routine.
!> @param[in] gfld derived type @ref grib_mod::gribfield.
!> @param[out] iret integer return code
!> - 0 No error.
!> - 2 Memory allocation error.
!> - 10 No Section 1 info available.
!> - 11 No Grid Definition Template info available.
!> - 12 Missing some required data field info.
!>
!> @note Derived type gribfield contains pointers to many arrays of
!> data (See @ref grib_mod::gribfield). The memory for these arrays is
!> allocated when the values in the arrays are set, to help minimize
!> problems with array overloading. Users must free up this memory,
!> when it is no longer needed, by a call to subroutine gf_free().
!>
!> @author Stephen Gilbert @date 2002-04-22
subroutine putgb2(lugb, gfld, iret)
  use grib_mod
  implicit none

  integer, intent(in) :: lugb
  type(gribfield), intent(in) :: gfld
  integer, intent(out) :: iret

  character(len = 1), allocatable, dimension(:) :: cgrib
  integer :: listsec0(2)
  integer :: igds(5)
  real    :: coordlist
  integer :: ilistopt
  integer :: ierr, is, lcgrib, lengrib

  listsec0 = (/0, 2/)
  igds = (/0, 0, 0, 0, 0/)
  coordlist = 0.0
  ilistopt = 0

  ! Figure out the maximum length of the GRIB2 message.
  lcgrib = 16 + 21 + 4 ! Sections 0, 1, and 8.
  ! Check for Section 2.
  if (associated(gfld%local) .AND. gfld%locallen .gt. 0) then
     lcgrib = lcgrib + gfld%locallen * 4
  endif
  ! Maximum size for Sections 3, 4, and 5 < 512 each.
  lcgrib = lcgrib + 512 + 512 + 512
  ! Is there a section 6?
  if (gfld%ibmap .eq. 0) then
     lcgrib = lcgrib + gfld%ngrdpts
  endif
  ! Section 7 holds the data.
  lcgrib = lcgrib + gfld%ngrdpts * 4

#ifdef LOGGING
  print *, 'putgb2 lugb ', lugb, ' lcgrib ', lcgrib
#endif

  ! Allocate array for grib2 field.
  allocate(cgrib(lcgrib), stat = is)
  if (is .ne. 0) then
     print *, 'putgb2: cannot allocate memory. ', is
     iret = 2
  endif

  ! Create new message.
  listsec0(1) = gfld%discipline
  listsec0(2) = gfld%version
  if (associated(gfld%idsect)) then
     call gribcreate(cgrib, lcgrib, listsec0, gfld%idsect, ierr)
     if (ierr .ne. 0) then
        write(6, *) 'putgb2: ERROR creating new GRIB2 field = ', ierr
     endif
  else
     print *, 'putgb2: No Section 1 info available. '
     iret = 10
     deallocate(cgrib)
     return
  endif

  ! Add local use section to grib2 message.
  if (associated(gfld%local) .AND. gfld%locallen .gt. 0) then
     call addlocal(cgrib, lcgrib, gfld%local, gfld%locallen, ierr)
     if (ierr .ne. 0) then
        write(6, *) 'putgb2: ERROR adding local info = ', ierr
     endif
  endif

  ! Add grid to grib2 message.
  igds(1) = gfld%griddef
  igds(2) = gfld%ngrdpts
  igds(3) = gfld%numoct_opt
  igds(4) = gfld%interp_opt
  igds(5) = gfld%igdtnum
  if (associated(gfld%igdtmpl)) then
     call addgrid(cgrib, lcgrib, igds, gfld%igdtmpl, gfld%igdtlen,  &
          ilistopt, gfld%num_opt, ierr)
     if (ierr .ne. 0) then
        write(6, *) 'putgb2: ERROR adding grid info = ', ierr
     endif
  else
     print *, 'putgb2: No GDT info available. '
     iret = 11
     deallocate(cgrib)
     return
  endif

  ! Add data field to grib2 message.
  if (associated(gfld%ipdtmpl) .AND. &
       associated(gfld%idrtmpl) .AND. &
       associated(gfld%fld)) then
  print *, gfld%ipdtnum, gfld%ipdtmpl
  print *, gfld%ipdtlen, gfld%num_coord
  print *, gfld%idrtnum, gfld%idrtmpl, gfld%idrtlen
  print *, gfld%fld, gfld%ngrdpts, gfld%ibmap, gfld%bmap
  print *, cgrib, lcgrib, coordlist, ierr
     call addfield(cgrib, lcgrib, gfld%ipdtnum, gfld%ipdtmpl,  &
          gfld%ipdtlen, coordlist, gfld%num_coord,  &
          gfld%idrtnum, gfld%idrtmpl, gfld%idrtlen,  &
          gfld%fld, gfld%ngrdpts, gfld%ibmap, gfld%bmap,  &
          ierr)
     if (ierr .ne. 0) then
        write(6, *) 'putgb2: ERROR adding data field = ', ierr
     endif
  else
     print *, 'putgb2: Missing some field info. '
     iret = 12
     deallocate(cgrib)
     return
  endif

  ! Close grib2 message and write to file.
  call gribend(cgrib, lcgrib, lengrib, ierr)
  call wryte(lugb, lengrib, cgrib)

  deallocate(cgrib)
end subroutine putgb2
