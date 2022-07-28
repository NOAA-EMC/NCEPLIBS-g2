!> @file
!> @brief Find a reference to the GRIB field requested in the index
!> file.
!> @author Stephen Gilbert @date 2002-01-15

!> Find a reference to the GRIB field requested in the index
!> file.
!>
!> The grib field request specifies the number of
!> messages to skip and the unpacked identification section, grid
!> definition template and product defintion section parameters.
!> (A requested parameter of -9999 means to allow any value of this
!> parameter to be found.)
!>
!> Each index record has the following form:
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before grib message
!> - byte 009 - 012 bytes to skip in message before lus (local use)
!> set = 0, if no local use section in grib2 message.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 grib version number ( currently 2 )
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within grib2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii+1-  jj grid definition section (gds)
!> - byte jj+1-  kk product definition section (pds)
!> - byte kk+1-  ll the data representation section (drs)
!> - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>
!> Most of the decoded information for the selected grib field is
!> returned in a derived type variable, gfld. gfld is of type @ref
!> grib_mod::gribfield. Users of this routine will need to include the
!> line "use grib_mod" in their calling routine.
!>
!> Each component of the gribfield type is described in the output
!> argument list section below. Only the unpacked bitmap and data
!> field components are not set by this routine.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial development
!> 2002-01-02 | Stephen Gilbert | Modified from getg1s to work with grib2
!> 2011-06-24 | Boi Vuong | initialize variable gfld%idsect and gfld%local
!>
!> @param[in] cbuf Buffer (of size nlen bytes) containing index data.
!> @param[in] nlen Total length of all index records.
!> @param[in] nnum Number of index records.
!> @param[in] j Number of fields to skip (0 to search from beginning).
!> @param[in] jdisc GRIB2 discipline number of requested field. See
!> [GRIB2 - TABLE 0.0 -
!> DISCIPLINE](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml).
!> Use -1 to accept any discipline.
!> @param[in] jids Array of values in the identification
!> section. (Set to -9999 for wildcard.)
!> - jids(1) Identification of originating centre. See [TABLE 0 -
!>   NATIONAL/INTERNATIONAL ORIGINATING
!>   CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html).
!> - jids(2) Identification of originating sub-centre. See [TABLE C -
!>   NATIONAL
!>   SUB-CENTERS](https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablec.html).
!> - jids(3) GRIB master tables version number. See [GRIB2 - TABLE 1.0
!>   - GRIB Master Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-0.shtml).
!> - jids(4) GRIB local tables version number. See [GRIB2 - TABLE 1.1
!>   - GRIB Local Tables Version
!>   Number](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-1.shtml).
!> - jids(5) Significance of reference time. See [GRIB2 - TABLE 1.2 -
!>   Significance of Reference
!>   Time](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-2.shtml).
!> - jids(6) year (4 digits)
!> - jids(7) month
!> - jids(8) day
!> - jids(9) hour
!> - jids(10) minute
!> - jids(11) second
!> - jids(12) Production status of processed data. See [GRIB2 - TABLE
!>   1.3 - Production Status of
!>   Data](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-3.shtml).
!> - jids(13) Type of processed data. See [GRIB2 - TABLE 1.4 - TYPE OF
!>   DATA](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table1-4.shtml).
!> @param[in] jpdtn Product Definition Template (PDT) number (n)
!> (if = -1, don't bother matching PDT - accept any)
!> @param[in] jpdt Array of values defining the Product Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[in] jgdtn Grid Definition Template (GDT) number (if = -1,
!> don't bother matching GDT - accept any).
!> @param[in] jgdt array of values defining the Grid Definition
!> Template of the field for which to search (=-9999 for wildcard).
!> @param[out] k Field number unpacked.
!> @param[out] gfld Derived type @ref grib_mod::gribfield.
!> @param[out] lpos Starting position of the found index record
!> within the complete index buffer, CBUF. = 0, if request not found.
!> @param[out] iret integer return code
!> - 0 No error.
!> - 97 Error reading GRIB file.
!> - other gf_getfld GRIB2 unpacker return code.
!>
!> @note This subprogram is intended for private use by getgb2()
!> routines only. Note that derived type gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set, to help
!> minimize problems with array overloading. Users must free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @author Stephen Gilbert @date 2002-01-15
subroutine getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
     jgdt, k, gfld, lpos, iret)
  use grib_mod
  implicit none

  character(len = 1), intent(in) :: cbuf(nlen)
  integer, intent(in) :: nlen, nnum, j, jdisc, jpdtn, jgdtn
  integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
  integer, intent(out) :: k, lpos, iret
  type(gribfield), intent(out) :: gfld

  integer :: kgds(5)
  logical :: match1, match3, match4
  integer :: ipos, inlen, lsec1, iof, icnd, jpos
  integer :: numgdt, lsec4, numpdt, lsec5, lsec3
  integer :: i

  interface
     subroutine gf_unpack1(cgrib, lcgrib, iofst, ids, idslen, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, pointer, dimension(:) :: ids
       integer, intent(out) :: ierr, idslen
     end subroutine gf_unpack1
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
  end interface

  ! Initialize.
  k = 0
  lpos = 0
  iret = 1
  ipos = 0
  nullify(gfld%idsect, gfld%local)
  nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
  nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)

  ! Search for request.
  do while (iret .ne. 0 .and. k .lt. nnum)
     k = k + 1
     call g2_gbytec(cbuf, inlen, ipos * 8, 4 * 8)    ! get length of current
     ! index record
     if (k .le. j) then           ! skip this index
        ipos = ipos + inlen
        cycle
     endif

     ! Check if grib2 discipline is a match.
     call g2_gbytec(cbuf, gfld%discipline, (ipos + 41) * 8, 1 * 8)
     if ((jdisc .ne. -1) .and. (jdisc .ne. gfld%discipline)) then
        ipos = ipos + inlen
        cycle
     endif

     ! Check if identification section is a match.
     match1 = .false.
     call g2_gbytec(cbuf, lsec1, (ipos + 44) * 8, 4 * 8)  ! get length of ids
     iof = 0
     call gf_unpack1(cbuf(ipos + 45), lsec1, iof, gfld%idsect, &
          gfld%idsectlen, icnd)
     if (icnd .eq. 0) then
        match1 = .true.
        do i = 1, gfld%idsectlen
           if ((jids(i) .ne. -9999) .and. &
                (jids(i) .ne. gfld%idsect(i))) then
              match1 = .false.
              exit
           endif
        enddo
     endif
     if (.not. match1) then
        deallocate(gfld%idsect)
        ipos = ipos + inlen
        cycle
     endif

     ! Check if grid definition template is a match.
     jpos = ipos + 44 + lsec1
     match3 = .false.
     call g2_gbytec(cbuf, lsec3, jpos * 8, 4 * 8)  ! get length of gds
     if (jgdtn .eq. -1) then
        match3 = .true.
     else
        call g2_gbytec(cbuf, numgdt, (jpos + 12) * 8, 2 * 8)  ! get gdt template no.
        if (jgdtn .eq. numgdt) then
           iof = 0
           call gf_unpack3(cbuf(jpos + 1), lsec3, iof, kgds, gfld%igdtmpl, &
                gfld%igdtlen, gfld%list_opt, gfld%num_opt, icnd)
           if (icnd .eq. 0) then
              match3 = .true.
              do i = 1, gfld%igdtlen
                 if ((jgdt(i) .ne. -9999) .and. &
                      (jgdt(i) .ne. gfld%igdtmpl(i))) then
                    match3 = .false.
                    exit
                 endif
              enddo
           endif
        endif
     endif
     if (.not. match3) then
        if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
        if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
        ipos = ipos + inlen
        cycle
     else
        gfld%griddef = kgds(1)
        gfld%ngrdpts = kgds(2)
        gfld%numoct_opt = kgds(3)
        gfld%interp_opt = kgds(4)
        gfld%igdtnum = kgds(5)
     endif

     ! Check if product definition template is a match.
     jpos = jpos + lsec3
     match4 = .false.
     call g2_gbytec(cbuf, lsec4, jpos * 8, 4 * 8)  ! get length of pds
     if (jpdtn .eq. -1) then
        match4 = .true.
     else
        call g2_gbytec(cbuf, numpdt, (jpos + 7) * 8, 2 * 8)  ! get pdt template no.
        if (jpdtn .eq. numpdt) then
           iof = 0
           call gf_unpack4(cbuf(jpos + 1), lsec4, iof, gfld%ipdtnum, &
                gfld%ipdtmpl, gfld%ipdtlen, &
                gfld%coord_list, gfld%num_coord, icnd)
           if (icnd .eq. 0) then
              match4 = .true.
              do i = 1, gfld%ipdtlen
                 if ((jpdt(i) .ne. -9999) .and. &
                      (jpdt(i) .ne. gfld%ipdtmpl(i))) then
                    match4 = .false.
                    exit
                 endif
              enddo
           endif
        endif
     endif
     if (.not. match4) then
        if (associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
        if (associated(gfld%coord_list)) deallocate(gfld%coord_list)
     endif

     ! If request is found set values for derived type gfld and return.
     if (match1 .and. match3 .and. match4) then
        lpos = ipos + 1
        call g2_gbytec(cbuf, gfld%version, (ipos + 40) * 8, 1 * 8)
        call g2_gbytec(cbuf, gfld%ifldnum, (ipos + 42) * 8, 2 * 8)
        gfld%unpacked = .false.
        jpos = ipos + 44 + lsec1
        if (jgdtn .eq. -1) then     ! unpack gds, if not done before
           iof = 0
           call gf_unpack3(cbuf(jpos + 1), lsec3, iof, kgds, gfld%igdtmpl, &
                gfld%igdtlen, gfld%list_opt, gfld%num_opt, icnd)
           gfld%griddef = kgds(1)
           gfld%ngrdpts = kgds(2)
           gfld%numoct_opt = kgds(3)
           gfld%interp_opt = kgds(4)
           gfld%igdtnum = kgds(5)
        endif
        jpos = jpos + lsec3
        if (jpdtn .eq. -1 ) then     ! unpack pds, if not done before
           iof = 0
           call gf_unpack4(cbuf(jpos + 1), lsec4, iof, gfld%ipdtnum, &
                gfld%ipdtmpl, gfld%ipdtlen, &
                gfld%coord_list, gfld%num_coord, icnd)
        endif
        jpos = jpos + lsec4
        call g2_gbytec(cbuf, lsec5 ,jpos * 8, 4 * 8)  ! get length of drs
        iof = 0
        call gf_unpack5(cbuf(jpos + 1), lsec5, iof, gfld%ndpts, &
             gfld%idrtnum, gfld%idrtmpl, &
             gfld%idrtlen, icnd)
        jpos = jpos + lsec5
        call g2_gbytec(cbuf, gfld%ibmap, (jpos + 5) * 8, 1 * 8)  ! get ibmap
        iret = 0
     else      ! pdt did not match
        ipos = ipos + inlen
     endif
  enddo
end subroutine getgb2s
