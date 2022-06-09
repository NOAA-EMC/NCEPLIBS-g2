!> @file
!> @brief This subroutine find a reference to the grib field
!> requested in the index file.
!> @author Stephen Gilbert @date 2002-01-15

!> This subroutine find in the index file for a reference to the grib
!> field requested. The grib field request specifies the number of
!> messages to skip and the unpacked identification section, grid
!> definition template and product defintion section parameters.
!> (a requested parameter of -9999 means to allow any value of this
!> parameter to be found.)
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
!> Most of the decoded information for the selected grib field is
!> returned in a derived type variable, GFLD. GFLD is of type
!> gribfield, which is defined in module grib_mod, so users of this
!> routine will need to include the line "use grib_mod" in their
!> calling routine. Each component of the gribfield type is described
!> in the output argument list section below. Only the unpacked
!> bitmap and data field components are not set by this routine.
!>
!> Program History log:
!> - 1995-10-31  Mark Iredell Initial development
!> - 2002-01-02  Stephen Gilbert Modified from getg1s to work with grib2
!> - 2011-06-24  Boi Vuong initialize variable gfld%idsect and gfld%local
!>
!> @param[in] CBUF character*1 (nlen) buffer containing index data.
!> @param[in] NLEN integer total length of all index records.
!> @param[in] NNUM integer number of index records.
!> @param[in] J integer number of fields to skip
!> (=0 to search from beginning)
!> @param[in] JDISC grib2 discipline number of requested field
!> (if = -1, accept any discipline see code table 0.0)
!> - 0 meteorological products
!> - 1 hydrological products
!> - 2 land surface products
!> - 3 space products
!> - 10 oceanographic products
!> @param[in] JIDS integer array of values in the identification section
!> (=-9999 for wildcard)
!> - JIDS(1) identification of originating centre
!> (see common code table c-1)
!> - JIDS(2) identification of originating sub-centre
!> - JIDS(3) grib master tables version number
!> (see code table 1.0)
!> - 0 experimental
!> - 1 initial operational version number.
!> - JIDS(4) grib local tables version number (see code table 1.1)
!> - 0 local tables not used
!> - 1-254 number of local tables version used.
!> - JIDS(5) significance of reference time (code table 1.2)
!> - 0 analysis
!> - 1 start of forecast
!> - 2 verifying time of forecast
!> - 3 observation time
!> - JIDS(6) year (4 digits)
!> - JIDS(7) month
!> - JIDS(8) day
!> - JIDS(9) hour
!> - JIDS(10) minute
!> - JIDS(11) second
!> - JIDS(12) production status of processed data (see code table 1.3)
!> - 0 operational products
!> - 1 operational test products;
!> - 2 research products
!> - 3 re-analysis products.
!> - JIDS(13) type of processed data (see code table 1.4)
!> - 0 analysis products
!> - 1 forecast products
!> - 2 analysis and forecast products
!> - 3 control forecast products
!> - 4 perturbed forecast products
!> - 5 control and perturbed forecast products
!> - 6 processed satellite observations
!> - 7 processed radar observations.
!> @param[in] JPDTN integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] JGDTN integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[out] K integer field number unpacked.
!> @param[out] GFLD derived type @ref grib_mod::gribfield.
!> @param[out] LPOS starting position of the found index record
!> within the complete index buffer, CBUF. = 0, if request not found.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 97 error reading grib file
!> - other gf_getfld grib2 unpacker return code
!>
!> @note This subprogram is intended for private use by getgb2
!> routines only. Note that derived type gribfield contains
!> pointers to many arrays of data. The memory for these arrays is
!> allocated when the values in the arrays are set, to help
!> minimize problems with array overloading. Users should free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @author Stephen Gilbert @date 2002-01-15
subroutine getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, &
        jgdt, k, gfld, lpos, iret)

    use grib_mod
    implicit none

    !      character(len=1),pointer,dimension(:) :: cbuf
    character(len = 1), intent(in) :: cbuf(nlen)
    integer, intent(in) :: nlen, nnum, j, jdisc, jpdtn, jgdtn
    integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
    integer, intent(out) :: k, lpos, iret
    type(gribfield), intent(out) :: gfld

    integer :: kgds(5)
    logical :: match1, match3, match4
    !      integer,pointer,dimension(:) :: kids,kpdt,kgdt
    !      integer,pointer,dimension(:) :: idef
    !      real,pointer,dimension(:) :: coord

    !implicit none additions
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

    !  initialize
    k = 0
    lpos = 0
    iret = 1
    ipos = 0
    nullify(gfld%idsect, gfld%local)
    nullify(gfld%list_opt, gfld%igdtmpl, gfld%ipdtmpl)
    nullify(gfld%coord_list, gfld%idrtmpl, gfld%bmap, gfld%fld)

    !  search for request
    do while(iret .ne. 0 .and. k .lt. nnum)
        k = k + 1
        call g2_gbytec(cbuf, inlen, ipos * 8, 4 * 8)    ! get length of current
        ! index record
        if ( k .le. j ) then           ! skip this index
            ipos = ipos + inlen
            cycle
        endif

        !  check if grib2 discipline is a match
        call g2_gbytec(cbuf, gfld%discipline, (ipos + 41) * 8, 1 * 8)
        if ( (jdisc .ne. -1) .and. (jdisc .ne. gfld%discipline) ) then
            ipos = ipos + inlen
            cycle
        endif

        !  check if identification section is a match
        match1 = .false.
        call g2_gbytec(cbuf, lsec1, (ipos + 44) * 8, 4 * 8)  ! get length of ids
        iof = 0
        call gf_unpack1(cbuf(ipos + 45), lsec1, iof, gfld%idsect, &
            gfld%idsectlen, icnd)
        if ( icnd .eq. 0 ) then
            match1 = .true.
            do i = 1, gfld%idsectlen
                if ( (jids(i) .ne. -9999) .and. &
                        (jids(i) .ne. gfld%idsect(i)) ) then
                    match1 = .false.
                    exit
                endif
            enddo
        endif
        if ( .not. match1 ) then
            deallocate(gfld%idsect)
            ipos = ipos + inlen
            cycle
        endif

        !  check if grid definition template is a match
        jpos = ipos + 44 + lsec1
        match3 = .false.
        call g2_gbytec(cbuf, lsec3, jpos * 8, 4 * 8)  ! get length of gds
        if ( jgdtn .eq. -1 ) then
            match3 = .true.
        else
            call g2_gbytec(cbuf, numgdt, (jpos + 12) * 8, 2 * 8)  ! get gdt template no.
            if ( jgdtn .eq. numgdt ) then
                iof = 0
                call gf_unpack3(cbuf(jpos + 1), lsec3, iof, kgds, gfld%igdtmpl, &
                        gfld%igdtlen, gfld%list_opt, gfld%num_opt, icnd)
                if ( icnd .eq. 0 ) then
                    match3 = .true.
                    do i = 1, gfld%igdtlen
                        if ( (jgdt(i) .ne. -9999) .and. &
                            (jgdt(i) .ne. gfld%igdtmpl(i)) ) then
                            match3 = .false.
                            exit
                        endif
                    enddo
                    !                 where ( jgdt(1:gfld%igdtlen).ne.-9999 )  &
                    !                   match3=all(jgdt(1:gfld%igdtlen).eq.gfld%igdtmpl(1:gfld%igdtlen))
                endif
            endif
        endif
        if ( .not. match3 ) then
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

        !  check if product definition template is a match
        jpos = jpos + lsec3
        match4 = .false.
        call g2_gbytec(cbuf, lsec4, jpos * 8, 4 * 8)  ! get length of pds
        if ( jpdtn .eq. -1 ) then
            match4 = .true.
        else
            call g2_gbytec(cbuf, numpdt, (jpos + 7) * 8, 2 * 8)  ! get pdt template no.
            if ( jpdtn .eq. numpdt ) then
                iof = 0
                call gf_unpack4(cbuf(jpos + 1), lsec4, iof, gfld%ipdtnum, &
                        gfld%ipdtmpl, gfld%ipdtlen, &
                        gfld%coord_list, gfld%num_coord, icnd)
                if ( icnd .eq. 0 ) then
                    match4 = .true.
                    do i = 1, gfld%ipdtlen
                        if ( (jpdt(i) .ne. -9999) .and. &
                            (jpdt(i) .ne. gfld%ipdtmpl(i)) ) then
                            match4 = .false.
                            exit
                        endif
                    enddo
                    !                 where ( jpdt.ne.-9999)  &
                    !                        match4=all( jpdt(1:gfld%ipdtlen) .eq. gfld%ipdtmpl(1:gfld%ipdtlen) )
                endif
            endif
        endif
        if ( .not. match4 ) then
            if (associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
            if (associated(gfld%coord_list)) deallocate(gfld%coord_list)
        endif

        !  if request is found
        !  set values for derived type gfld and return
        if(match1 .and. match3 .and. match4) then
            lpos = ipos + 1
            call g2_gbytec(cbuf, gfld%version, (ipos + 40) * 8, 1 * 8)
            call g2_gbytec(cbuf, gfld%ifldnum, (ipos + 42) * 8, 2 * 8)
            gfld%unpacked = .false.
            jpos = ipos + 44 + lsec1
            if ( jgdtn .eq. -1 ) then     ! unpack gds, if not done before
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
            if ( jpdtn .eq. -1 ) then     ! unpack pds, if not done before
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

    return
end subroutine getgb2s
