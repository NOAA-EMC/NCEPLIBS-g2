!> @file
!> @brief This subroutine generates an index record for each field in
!> a grib2 message.
!> @author Mark Iredell @date 1995-10-31

!> This subroutine generates an index record for each field in
!> a grib2 message. The index records are written to index buffer
!> pointed to by cbuf.
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
!> - byte 041 - 041 grib version number (currently 2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within grib2 message
!> - byte 045 - ii identification section (ids)
!> - byte ii+1 - jj grid definition section (gds)
!> - byte jj+1 - kk product definition section (pds)
!> - byte kk+1 - ll the data representation section (drs)
!> - byte ll+1 - ll+6 first 6 bytes of the bit map section (bms)
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1995-10-31 | Mark Iredell | Initial.
!> 1996-10-31 | Mark Iredell | augmented optional definitions to byte 320.
!> 2001-12-10 | Stephen Gilbert | modified from ixgb to create grib2 indexes.
!> 2002-01-31 | Stephen Gilbert | added identification section to index record.
!>
!> @param[in] lugb Unit of the unblocked grib file. Must
!> be opened by [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/).
!> @param[in] lskip Number of bytes to skip before grib message.
!> @param[in] lgrib Number of bytes in grib message.
!> @param[out] cbuf Pointer to a buffer that will get
!> index records. Users should free memory that cbuf points to
!> using deallocate(cbuf) when cbuf is no longer needed.
!> @param[out] numfld Number of index records created.
!> @param[out] mlen Total length of all index records.
!> @param[out] iret Return code
!> - 0 No error
!> - 1 Not enough memory available to hold full index buffer.
!> - 2 I/O error in read.
!> - 3 GRIB message is not edition 2.
!> - 4 Not enough memory to allocate extent to index buffer.
!> - 5 Unidentified GRIB section encountered.
!>
!> @author Mark Iredell @date 1995-10-31
subroutine ixgb2(lugb, lskip, lgrib, cbuf, numfld, mlen, iret)

  use re_alloc          ! needed for subroutine realloc
  implicit none
  character(len = 1),pointer,dimension(:) :: cbuf
  integer linmax, ixskp, mxskp, lugb, lskip, lgrib, numfld
  integer mlen, iret, ibread, ibskip, ilndrs, ilnpds, indbmp
  integer init, istat, ixds, ixfld, ixids, ixlen, ixlus, ixsbm, ixsdr, ixspd
  integer ixsgd, lbread, lensec, lensec1, lindex, mbuf
  integer mxbms, mxds, mxfld, mxlen, mxlus, mxsbm, mxsdr, mxsgd
  integer mxspd, newsize, next, numsec, mova2i
  parameter(linmax = 5000, init = 50000, next = 10000)
  parameter(ixskp = 4, ixlus = 8, ixsgd = 12, ixspd = 16, ixsdr = 20, ixsbm = 24, &
       ixds = 28, ixlen = 36, ixfld = 42, ixids = 44)
  parameter(mxskp = 4, mxlus = 4, mxsgd = 4, mxspd = 4, mxsdr = 4, mxsbm = 4, &
       mxds = 4, mxlen = 4, mxfld = 2, mxbms = 6)
  character cbread(linmax), cindex(linmax)
  character cver, cdisc
  character cids(linmax), cgds(linmax)
  character(len = 4) :: ctemp
  integer loclus, locgds, lengds, locbms
  integer i

  !print *, 'ixbg2 ', lugb, lskip, lgrib

  loclus = 0
  iret = 0
  mlen = 0
  numfld = 0
  if (associated(cbuf)) nullify(cbuf)
  mbuf = init
  allocate(cbuf(mbuf), stat = istat)    ! Allocate initial space for cbuf.
  if (istat .ne. 0) then
     iret = 1
     return
  endif

  ! Read sections 0 and 1 for version number and discipline.
  ibread = min(lgrib, linmax)
  !print *, 'ibread = ', ibread
  call baread(lugb, lskip, ibread, lbread, cbread)
  ! do i = 1, 20
  !    print *, ichar(cbread(i))
  ! end do
  !print *, 'lbread = ', lbread, ' ibread = ', ibreadz
  if (lbread .ne. ibread) then
     iret = 2
     return
  endif
  if (cbread(8) .ne. char(2)) then ! Not GRIB edition 2.
     iret = 3
     return
  endif
  cver = cbread(8)
  cdisc = cbread(7)
  call g2_gbytec(cbread, lensec1, 16 * 8, 4 * 8)
  lensec1 = min(lensec1, ibread)
  cids(1:lensec1) = cbread(17:16 + lensec1)
  ibskip = lskip + 16 + lensec1

  ! Loop through remaining sections creating an index for each field.
  ibread = max(5, mxbms)
  do
     call baread(lugb, ibskip, ibread, lbread, cbread)
     ctemp = cbread(1) // cbread(2) // cbread(3) // cbread(4)
     if (ctemp .eq. '7777') return        ! end of message found
     if (lbread .ne. ibread) then
        iret = 2
        return
     endif
     call g2_gbytec(cbread, lensec, 0*8, 4*8)
     call g2_gbytec(cbread, numsec, 4*8, 1*8)

     if (numsec .eq. 2) then                 ! save local use location
        loclus = ibskip - lskip
     elseif (numsec .eq. 3) then                 ! save gds info
        lengds = lensec
        cgds = char(0)
        call baread(lugb, ibskip, lengds, lbread, cgds)
        if (lbread .ne. lengds) then
           iret = 2
           return
        endif
        locgds = ibskip - lskip
     elseif (numsec .eq. 4) then                 ! found pds
        cindex = char(0)
        call g2_sbytec(cindex, lskip, 8 * ixskp, 8 * mxskp)    ! bytes to skip
        call g2_sbytec(cindex, loclus, 8 * ixlus, 8 * mxlus)   ! location of local use
        call g2_sbytec(cindex, locgds, 8 * ixsgd, 8 * mxsgd)   ! location of gds
        call g2_sbytec(cindex, ibskip - lskip, 8 * ixspd, 8 * mxspd)  ! location of pds
        call g2_sbytec(cindex, lgrib, 8 * ixlen, 8 * mxlen)    ! len of grib2
        cindex(41) = cver
        cindex(42) = cdisc
        call g2_sbytec(cindex, numfld + 1, 8 * ixfld, 8 * mxfld)   ! field num
        cindex(ixids + 1:ixids + lensec1) = cids(1:lensec1)
        lindex = ixids + lensec1
        cindex(lindex + 1:lindex + lengds) = cgds(1:lengds)
        lindex = lindex + lengds
        ilnpds = lensec
        call baread(lugb, ibskip, ilnpds, lbread, cindex(lindex + 1))
        if (lbread .ne. ilnpds) then
           iret = 2
           return
        endif
        !   cindex(lindex+1:lindex+ilnpds) = cbread(1:ilnpds)
        lindex = lindex + ilnpds
     elseif (numsec .eq. 5) then                 ! found drs
        call g2_sbytec(cindex, ibskip-lskip, 8 * ixsdr, 8 * mxsdr)  ! location of drs
        ilndrs = lensec
        call baread(lugb, ibskip, ilndrs, lbread, cindex(lindex+1))
        if (lbread .ne. ilndrs) then
           iret = 2
           return
        endif
        !   cindex(lindex+1:lindex+ilndrs) = cbread(1:ilndrs)
        lindex = lindex + ilndrs
     elseif (numsec .eq. 6) then                 ! found bms
        indbmp = mova2i(cbread(6))
        if (indbmp .lt. 254) then
           locbms = ibskip - lskip
           call g2_sbytec(cindex, locbms, 8 * ixsbm, 8 * mxsbm)  ! loc. of bms
        elseif ( indbmp .eq. 254 ) then
           call g2_sbytec(cindex, locbms, 8 * ixsbm, 8 * mxsbm)  ! loc. of bms
        elseif ( indbmp .eq. 255 ) then
           call g2_sbytec(cindex, ibskip-lskip, 8 * ixsbm, 8 * mxsbm)  ! loc. of bms
        endif
        cindex(lindex + 1:lindex + mxbms) = cbread(1:mxbms)
        lindex = lindex + mxbms
        call g2_sbytec(cindex, lindex, 0, 8 * 4)    ! num bytes in index record
     elseif (numsec .eq. 7) then                 ! found data section
        call g2_sbytec(cindex, ibskip-lskip, 8 * ixds, 8 * mxds)   ! loc. of data sec.
        numfld = numfld + 1
        if ((lindex + mlen) .gt. mbuf) then        ! allocate more space if necessary.
           newsize = max(mbuf + next, mbuf + lindex)
           call realloc(cbuf, mlen, newsize, istat)
           if ( istat .ne. 0 ) then
              numfld = numfld - 1
              iret = 4
              return
           endif
           mbuf = newsize
        endif
        cbuf(mlen + 1:mlen + lindex) = cindex(1:lindex)
        mlen = mlen + lindex
     else                           ! unrecognized section
        iret = 5
        return
     endif
     ibskip = ibskip + lensec
  enddo
end subroutine ixgb2
