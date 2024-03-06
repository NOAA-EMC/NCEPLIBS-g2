!> @file
!> @brief Subroutines to write and get a field and free memmory from
!> @ref grib_mod::gribfield.
!> @author Edward Hartnett @date Mar 6, 2024

!> Pack a field into a grib2 message and write that message to a file.
!>
!> The information to be packed into the grib field is stored in a
!> derived type variable, gfld. gfld is of type grib_mod::gribfield,
!> which is defined in module grib_mod, so users of this routine will
!> need to include the line "use grib_mod" in their calling
!> routine. Each component of the gribfield type is described in the
!> input argument list section below.
!>
!> @note Derived type gribfield contains pointers to many arrays of
!> data (See @ref grib_mod::gribfield). The memory for these arrays is
!> allocated when the values in the arrays are set. Users must free
!> this memory, when it is no longer needed, by a call to subroutine
!> gf_free().
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

!> Return the Grid Definition, and Product Definition for a
!> given data field.
!>
!> All of the information returned is stored in a derived type
!> variable, gfld. Gfld is of type grib_mod::gribfield, which is
!> defined in gribmod.F90; users of this routine will need to include
!> the line "USE GRIB_MOD" in their calling routine.
!>
!> Since there can be multiple data fields packed into a GRIB2
!> message, the calling routine indicates which field is being
!> requested with the ifldnum argument.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[in] ifldnum Specifies which field in the GRIB2 message to
!> return.
!> @param[in] unpack Logical value indicating whether to unpack
!> bitmap/data. .true. = unpack bitmap and data values; .false. = do
!> not unpack bitmap and data values.
!> @param[in] expand Boolean value indicating whether the data points
!> should be expanded to the correspond grid, if a bit-map is
!> present.
!> - 1 if possible, expand data field to grid, inserting zero
!> values at gridpoints that are bitmapped out.
!> - 0 do not expand data field, leaving it an array of consecutive
!> data points for each "1" in the bitmap. This argument is ignored
!> if unpack == 0 OR if the returned field does not contain a
!> bit-map.
!> @param[out] gfld derived type @ref grib_mod::gribfield.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 1 Beginning characters "GRIB" not found.
!> - 2 GRIB message is not Edition 2.
!> - 3 The data field request number was not positive.
!> - 4 End string "7777" found, but not where expected.
!> - 5 End string "7777" not found at end of message.
!> - 6 GRIB message did not contain the requested number of data fields.
!> - 7 End string "7777" not found at end of message.
!> - 9 Data Representation Template 5.NN not yet implemented.
!> - 10 Error unpacking Section 3.
!> - 11 Error unpacking Section 4.
!> - 12 Error unpacking Section 5.
!> - 13 Error unpacking Section 6.
!> - 14 Error unpacking Section 7.
!> - 17 Previous bitmap specified, but none exists.
!>
!> @note Note that derived type @ref grib_mod::gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set, to help minimize
!> problems with array overloading. Because of this users should free
!> this memory, when it is no longer needed, by a call to subroutine
!> gf_free(). Subroutine gb_info() can be used to first determine how
!> many data fields exist in a given GRIB message.
!>
!> It may not always be possible to expand a bit-mapped data
!> field. If a pre-defined bit-map is used and not included in the
!> GRIB2 message itself, this routine would not have the necessary
!> information to expand the data. In this case, gfld\%expanded would
!> be set to 0 (false), regardless of the value of input argument
!> expand.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_getfld(cgrib, lcgrib, ifldnum, unpack, expand, gfld, ierr)

  use grib_mod
  implicit none

  character(len = 1), intent(in) :: cgrib(lcgrib)
  integer, intent(in) :: lcgrib, ifldnum
  logical, intent(in) :: unpack, expand
  type(gribfield), intent(out) :: gfld
  integer, intent(out) :: ierr

  character(len = 4), parameter :: grib = 'GRIB', c7777 = '7777'
  character(len = 4) :: ctemp
  real, pointer, dimension(:) :: newfld
  integer:: listsec0(2), igds(5)
  integer iofst, istart
  logical*1, pointer, dimension(:) :: bmpsave
  logical have3, have4, have5, have6, have7

  !implicit none additions
  integer :: numfld, j, lengrib, ipos, lensec0, lensec
  integer :: isecnum, jerr, n, numlocal

  interface
     subroutine gf_unpack1(cgrib, lcgrib, iofst, ids, idslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: ids
       integer, intent(out) :: ierr, idslen
     end subroutine gf_unpack1
     subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: lencsec2
       integer, intent(out) :: ierr
       character(len = 1), pointer, dimension(:) :: csec2
     end subroutine gf_unpack2
     subroutine gf_unpack3(cgrib, lcgrib, iofst, igds, igdstmpl, &
          mapgridlen, ideflist, idefnum, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: igdstmpl, ideflist
       integer, intent(out) :: igds(5)
       integer, intent(out) :: mapgridlen
       integer, intent(out) :: ierr, idefnum
     end subroutine gf_unpack3
     subroutine gf_unpack4(cgrib, lcgrib, iofst, ipdsnum, ipdstmpl, &
          mappdslen, coordlist, numcoord, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       real, pointer, dimension(:) :: coordlist
       integer, pointer, dimension(:) :: ipdstmpl
       integer, intent(out) :: ipdsnum
       integer, intent(out) :: mappdslen
       integer, intent(out) :: ierr, numcoord
     end subroutine gf_unpack4
     subroutine gf_unpack5(cgrib, lcgrib, iofst, ndpts, idrsnum, &
          idrstmpl, mapdrslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: ndpts, idrsnum
       integer, pointer, dimension(:) :: idrstmpl
       integer, intent(out) :: mapdrslen
       integer, intent(out) :: ierr
     end subroutine gf_unpack5
     subroutine gf_unpack6(cgrib, lcgrib, iofst, ngpts, ibmap, bmap, &
          ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib, ngpts
       integer, intent(inout) :: iofst
       integer, intent(out) :: ibmap
       integer, intent(out) :: ierr
       logical*1, pointer, dimension(:) :: bmap
     end subroutine gf_unpack6
     subroutine gf_unpack7(cgrib, lcgrib, iofst, igdsnum, igdstmpl, &
          idrsnum, idrstmpl, ndpts, fld, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib, ndpts, idrsnum, igdsnum
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: idrstmpl, igdstmpl
       integer, intent(out) :: ierr
       real, pointer, dimension(:) :: fld
     end subroutine gf_unpack7
  end interface

  have3 = .false.
  have4 = .false.
  have5 = .false.
  have6 = .false.
  have7 = .false.
  ierr = 0
  numfld = 0
  gfld%locallen = 0
  nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
  nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)

  !     Check for valid request number
  if (ifldnum .le. 0) then
     print *, 'gf_getfld: Request for field number ' &
          ,'must be positive.'
     ierr = 3
     return
  endif

  !     Check for beginning of GRIB message in the first 100 bytes
  istart = 0
  do j = 1, 100
     ctemp = cgrib(j) // cgrib(j + 1) // cgrib(j + 2) // cgrib(j + 3)
     if (ctemp .eq. grib) then
        istart = j
        exit
     endif
  enddo
  if (istart .eq. 0) then
     print *, 'gf_getfld:  Beginning characters GRIB not found.'
     ierr = 1
     return
  endif

  !     Unpack Section 0 - Indicator Section
  iofst = 8 * (istart + 5)
  call g2_gbytec(cgrib, listsec0(1), iofst, 8) ! Discipline
  iofst = iofst + 8
  call g2_gbytec(cgrib, listsec0(2), iofst, 8) ! GRIB edition number
  iofst = iofst + 8
  iofst = iofst + 32
  call g2_gbytec(cgrib, lengrib, iofst, 32) ! Length of GRIB message
  iofst = iofst + 32
  lensec0 = 16
  ipos = istart + lensec0

  !     Currently handles only GRIB Edition 2.
  if (listsec0(2) .ne. 2) then
     print *, 'gf_getfld: can only decode GRIB edition 2.'
     ierr = 2
     return
  endif

  !     Loop through the remaining sections keeping track of the length of
  !     each. Also keep the latest Grid Definition Section info. Unpack
  !     the requested field number.
  do
     !         Check to see if we are at end of GRIB message
     ctemp = cgrib(ipos) // cgrib(ipos + 1) // cgrib(ipos + 2) // &
          cgrib(ipos + 3)
     if (ctemp .eq. c7777) then
        ipos = ipos + 4
        !             If end of GRIB message not where expected, issue error
        if (ipos.ne.(istart + lengrib)) then
           print *, 'gf_getfld: "7777" found, but not ' &
                ,'where expected.'
           ierr = 4
           return
        endif
        exit
     endif

     !         Get length of Section and Section number
     iofst = (ipos - 1) * 8
     call g2_gbytec(cgrib, lensec, iofst, 32) ! Get Length of Section
     iofst = iofst + 32
     call g2_gbytec(cgrib, isecnum, iofst, 8) ! Get Section number
     iofst = iofst + 8

     !         Check to see if section number is valid
     if ((isecnum .lt. 1) .or. (isecnum .gt. 7)) then
        print *, 'gf_getfld: Unrecognized Section Encountered = ', &
             isecnum
        ierr = 8
        return
     endif

     !         If found Section 1, decode elements in Identification Section.
     if (isecnum .eq. 1) then
        iofst = iofst - 40    ! reset offset to beginning of section
        call gf_unpack1(cgrib, lcgrib, iofst, gfld%idsect, &
             gfld%idsectlen, jerr)
        if (jerr .ne. 0) then
           ierr = 15
           return
        endif
     endif

     !         If found Section 2, Grab local section. Save in case this is
     !         the latest one before the requested field.
     if (isecnum .eq. 2) then
        iofst = iofst - 40    ! reset offset to beginning of section
        if (associated(gfld%local)) deallocate(gfld%local)
        call gf_unpack2(cgrib, lcgrib, iofst, gfld%locallen, &
             gfld%local, jerr)
        if (jerr .ne. 0) then
           call gf_free(gfld)
           ierr = 16
           return
        endif
     endif

     !         If found Section 3, unpack the GDS info using the appropriate
     !         template. Save in case this is the latest grid before the
     !         requested field.
     if (isecnum .eq. 3) then
        iofst = iofst - 40    ! reset offset to beginning of section
        if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
        if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
        call gf_unpack3(cgrib, lcgrib, iofst, igds, gfld%igdtmpl, &
             gfld%igdtlen, gfld%list_opt, gfld%num_opt, jerr)
        if (jerr .ne. 0) then
           call gf_free(gfld)
           ierr = 10
           return
        endif
        have3 = .true.
        gfld%griddef = igds(1)
        gfld%ngrdpts = igds(2)
        gfld%numoct_opt = igds(3)
        gfld%interp_opt = igds(4)
        gfld%igdtnum = igds(5)
     endif

     !         If found Section 4, check to see if this field is the one
     !         requested.
     if (isecnum .eq. 4) then
        numfld = numfld + 1
        if (numfld .eq. ifldnum) then
           gfld%discipline = listsec0(1)
           gfld%version = listsec0(2)
           gfld%ifldnum = ifldnum
           gfld%unpacked = unpack
           gfld%expanded = .false.
           iofst = iofst-40 ! reset offset to beginning of section
           call gf_unpack4(cgrib, lcgrib, iofst, gfld%ipdtnum, &
                gfld%ipdtmpl, gfld%ipdtlen, gfld%coord_list, &
                gfld%num_coord, jerr)
           if (jerr .ne. 0) then
              call gf_free(gfld)
              ierr = 11
              return
           endif
           have4 = .true.
        endif
     endif

     !         If found Section 5, check to see if this field is the one
     !         requested.
     if ((isecnum .eq. 5).and.(numfld .eq. ifldnum)) then
        iofst = iofst-40    ! reset offset to beginning of section
        call gf_unpack5(cgrib, lcgrib, iofst, gfld%ndpts, &
             gfld%idrtnum, gfld%idrtmpl, gfld%idrtlen, jerr)
        if (jerr .ne. 0) then
           call gf_free(gfld)
           ierr = 12
           return
        endif
        have5 = .true.
     endif

     !         If found Section 6, Unpack bitmap. Save in case this is the
     !         latest bitmap before the requested field.
     if (isecnum .eq. 6) then
        if (unpack) then  ! unpack bitmap
           iofst = iofst - 40 ! reset offset to beginning of section
           bmpsave => gfld%bmap ! save pointer to previous bitmap
           call gf_unpack6(cgrib, lcgrib, iofst, gfld%ngrdpts, &
                gfld%ibmap, gfld%bmap, jerr)
           if (jerr .ne. 0) then
              call gf_free(gfld)
              ierr = 13
              return
           endif
           have6 = .true.
           if (gfld%ibmap .eq. 254) then ! use previously specified bitmap
              if (associated(bmpsave)) then
                 gfld%bmap => bmpsave
              else
                 print *, 'gf_getfld:  Previous bit-map ' &
                      ,'specified, but none exists, '
                 call gf_free(gfld)
                 ierr = 17
                 return
              endif
           else          ! get rid of it
              if (associated(bmpsave)) deallocate(bmpsave)
           endif
        else              ! do not unpack bitmap
           call g2_gbytec(cgrib, gfld%ibmap, iofst, 8) ! Get BitMap Indicator
           have6 = .true.
        endif
     endif

     !         If found Section 7, check to see if this field is the one
     !         requested.
     if ((isecnum .eq. 7) .and. (numfld .eq. ifldnum) .and. unpack) &
          then
        iofst = iofst - 40    ! reset offset to beginning of section
        call gf_unpack7(cgrib, lcgrib, iofst, gfld%igdtnum, &
             gfld%igdtmpl, gfld%idrtnum, &
             gfld%idrtmpl, gfld%ndpts, &
             gfld%fld, jerr)
        if (jerr .ne. 0) then
           call gf_free(gfld)
           print *, 'gf_getfld: return from gf_unpack7 = ', jerr
           ierr = 14
           return
        endif
        have7 = .true.

        !             If bitmap is used with this field, expand data field
        !             to grid, if possible.
        if (gfld%ibmap .ne. 255 .AND. associated(gfld%bmap)) then
           if (expand) then
              allocate(newfld(gfld%ngrdpts))
              n = 1
              do j = 1, gfld%ngrdpts
                 if (gfld%bmap(j)) then
                    newfld(j) = gfld%fld(n)
                    n = n + 1
                 else
                    newfld(j) = 0.0
                 endif
              enddo
              deallocate(gfld%fld);
              gfld%fld=>newfld;
              gfld%expanded = .true.
           else
              gfld%expanded = .false.
           endif
        else
           gfld%expanded = .true.
        endif
     endif

     !         Check to see if we read pass the end of the GRIB message and
     !         missed the terminator string '7777'.
     ipos = ipos + lensec      ! Update beginning of section pointer
     if (ipos .gt. (istart + lengrib)) then
        print *, 'gf_getfld: "7777"  not found at end ' &
             ,'of GRIB message.'
        call gf_free(gfld)
        ierr = 7
        return
     endif
     !
     !         If unpacking requested, return when all sections have been
     !         processed.
     if (unpack .and. have3 .and. have4 .and. have5 .and. have6 &
          .and. have7) return

     !         If unpacking is not requested, return when sections 3 through
     !         6 have been processed.
     if ((.not. unpack) .and. have3 .and. have4 .and. have5 .and. &
          have6) return
  enddo

  !     If exited from above loop, the end of the GRIB message was reached
  !     before the requested field was found.
  print *, 'gf_getfld: GRIB message contained ', numlocal, &
       ' different fields.'
  print *, 'gf_getfld: The request was for the ', ifldnum, &
       ' field.'
  ierr = 6
  call gf_free(gfld)
end subroutine gf_getfld

!> Free memory that was used to store array values in derived type
!> grib_mod::gribfield.
!>
!> @param gfld derived type grib_mod::gribfield.
!>
!> @author Stephen Gilbert @date 2000-05-26
subroutine gf_free(gfld)
  use grib_mod
  implicit none

  type(gribfield) :: gfld
  integer :: is

  if (associated(gfld%idsect)) then
     deallocate(gfld%idsect,stat=is)
  endif
  nullify(gfld%idsect)

  if (associated(gfld%local)) then
     deallocate(gfld%local,stat=is)
  endif
  nullify(gfld%local)

  if (associated(gfld%list_opt)) then
     deallocate(gfld%list_opt,stat=is)
  endif
  nullify(gfld%list_opt)

  if (associated(gfld%igdtmpl)) then
     deallocate(gfld%igdtmpl,stat=is)
  endif
  nullify(gfld%igdtmpl)

  if (associated(gfld%ipdtmpl)) then
     deallocate(gfld%ipdtmpl,stat=is)
  endif
  nullify(gfld%ipdtmpl)

  if (associated(gfld%coord_list)) then
     deallocate(gfld%coord_list,stat=is)
  endif
  nullify(gfld%coord_list)

  if (associated(gfld%idrtmpl)) then
     deallocate(gfld%idrtmpl,stat=is)
  endif
  nullify(gfld%idrtmpl)

  if (associated(gfld%bmap)) then
     deallocate(gfld%bmap,stat=is)
  endif
  nullify(gfld%bmap)

  if (associated(gfld%fld)) then
     deallocate(gfld%fld,stat=is)
  endif
  nullify(gfld%fld)

  return
end subroutine gf_free
