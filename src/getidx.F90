!> @file
!> @brief This subroutine finds, reads or generates a grib2 index for
!> the grib2 file associated with unit lugb.
!> @author Stephen Gilbert @date 2005-03-15

!> This subroutine finds, reads or generates a grib2 index for
!> the grib2 file associated with unit lugb. If the index already
!> exists, it is returned, otherwise, the index is (1) read from an
!> existing indexfile associated with unit LUGI or (2) generated
!> from the grib2file LUGI. Users can force a regeneration of an
!> index. If LUGI equals LUGB, the index will be regenerated from
!> the data in file LUGB. If LUGI is less than zero, then the index
!> is re read from index file abs(lugi).
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2005-03-15 | Stephen Gilbert | Initial Programming
!> 2009-07-09 | Boi Vuong | Fixed bug for checking (LUGB) unit index file
!> 2016-03-29 | Boi Vuong | Restore getidx.f from 1.2.3; file num up to 9999; added templates: 4.60, 4.61
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] LUGI integer unit of the unblocked grib index file.
!> if nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine. (=0 to get index buffer from the grib file)
!> @param[out] CINDEX character*1 pointer to a buffer that contains
!> index records.
!> @param[out] NLEN integer total length of all index records
!> @param[out] NNUM integer number of index records
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 90 unit number out of range
!> - 96 error reading/creating index file
!>
!> @note allow file unit numbers in range 0 - 9999
!> the grib index will automatically generate the index file.
!>
!> @author Stephen Gilbert @date 2005-03-15
subroutine getidx(lugb, lugi, cindex, nlen, nnum, iret)
    implicit none

    integer, intent(in) :: lugb, lugi
    integer, intent(out) :: nlen, nnum, iret
    character(len = 1), pointer, dimension(:) :: cindex

    integer, parameter :: maxidx = 10000
    integer, parameter :: msk1 = 32000, msk2 = 4000

    !implicit none additions
    integer :: lux
    integer :: irgi, mskp, nmess 

    type gindex
        integer :: nlen
        integer :: nnum
        character(len = 1), pointer, dimension(:) :: cbuf
    end type gindex

    type(gindex), save :: idxlist(10000)

    data lux/0/

    !  declare interfaces (required for cbuf pointer)
    interface
        subroutine getg2i(lugi, cbuf, nlen, nnum, iret)
            character(len = 1), pointer, dimension(:) :: cbuf
            integer, intent(in) :: lugi
            integer, intent(out) :: nlen, nnum, iret
        end subroutine getg2i
        subroutine getg2ir(lugb, msk1, msk2, mnum, cbuf, nlen, nnum, &
            nmess, iret)
            character(len = 1), pointer, dimension(:) :: cbuf
            integer, intent(in) :: lugb, msk1, msk2, mnum
            integer, intent(out) :: nlen, nnum, nmess, iret
        end subroutine getg2ir
    end interface

    !  determine whether index buffer needs to be initialized
    lux = 0
    iret = 0
    if ( lugb .le. 0 .or. lugb .gt. 9999 ) then
        print *, ' '
        print *, ' file unit number out of range'
        print *, ' use unit numbers in range: 0 - 9999 '
        print *, ' '
        iret = 90
        return
    endif
    if (lugi .eq. lugb) then      ! force regeneration of index from grib2 file
        if ( associated( idxlist(lugb)%cbuf ) )  &
            deallocate(idxlist(lugb)%cbuf)
        nullify(idxlist(lugb)%cbuf)
        idxlist(lugb)%nlen = 0
        idxlist(lugb)%nnum = 0
        lux = 0
    endif

    if (lugi .lt. 0) then      ! force re-read of index from indexfile
        ! associated with unit abs(lugi)
        if ( associated( idxlist(lugb)%cbuf ) )  &
            deallocate(idxlist(lugb)%cbuf)
        nullify(idxlist(lugb)%cbuf)
        idxlist(lugb)%nlen = 0
        idxlist(lugb)%nnum = 0
        lux = abs(lugi)
    endif

    !  check if index already exists in memory
    if ( associated( idxlist(lugb)%cbuf ) ) then
        cindex => idxlist(lugb)%cbuf
        nlen = idxlist(lugb)%nlen
        nnum = idxlist(lugb)%nnum
        return
    endif

    irgi = 0
    if(lux .gt. 0) then
        call getg2i(lux, idxlist(lugb)%cbuf, nlen, nnum, irgi)
    elseif(lux .le. 0) then
        mskp = 0
        call getg2ir(lugb, msk1, msk2, mskp, idxlist(lugb)%cbuf, &
            nlen, nnum, nmess, irgi)
    endif
    if(irgi .eq. 0) then
        cindex => idxlist(lugb)%cbuf
        idxlist(lugb)%nlen = nlen
        idxlist(lugb)%nnum = nnum
    else
        nlen = 0
        nnum = 0
        print *, ' '
        print *, ' error reading index file '
        print *, ' '
        iret = 96
        return
    endif

    return
end subroutine getidx
