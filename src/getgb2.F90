!> @file
!> @brief This subroutine finds and unpacks a GRIB2 file.
!> @author Mark Iredell @date 1994-04-01

!> This subroutine finds and unpacks a GRIB2 message. It reads
!> a GRIB index file (or optionally the GRIB file itself) to
!> get the index buffer (i.e. table of contents) for the GRIB file.
!>
!> Find in the index buffer a reference to the GRIB field requested.
!>
!> The GRIB field request specifies the number of fields to skip
!> and the unpacked identification section, grid definition template
!> and product defintion section parameters. (A requested parameter
!> of -9999 means to allow any value of this parameter to be found.)
!>
!> If the requested GRIB field is found, then it is read from the
!> GRIB file and unpacked. Its number is returned along with
!> the associated unpacked parameters. the bitmap (if any);
!> the data values are unpacked only if argument "unpack" is set to
!> true. If the GRIB field is not found, then the return code
!> will be nonzero.
!>
!> The decoded information for the selected GRIB field is returned
!> in a derived type variable, gfld. Gfld is of type gribfield,
!> which is defined in module grib_mod, so users of this routine
!> will need to include the line "USE GRIB_MOD" in their calling
!> routine. Each component of the gribfield type is described in
!> the OUTPUT ARGUMENT LIST section below.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 1994-04-01 | Mark Iredell | Initial.
!> 1995-10-31 | Mark Iredell | modularized code into subprograms, allowed for unspecified index file,
!> 2002-01-11 | Stephen Gilbert | modified from getgb and getgbm to work with grib2
!> 2015-11-10 | Boi Vuong | modified doc block for gfld\%ngrdpts and gfld\%ndpts
!>
!> @param[in] LUGB integer unit of the unblocked grib data file.
!> File must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before calling
!> this routine.
!> @param[in] LUGI integer unit of the unblocked grib index file.
!> If nonzero, file must be opened with [baopen() or baopenr()]
!> (https://noaa-emc.github.io/NCEPLIBS-bacio/) before
!> calling this routine.
!> - >0 read index from index file lugi, if index doesn"t already exist.
!> - =0 to get index buffer from the grib file, if index
!> doesn"t already exist.
!> - <0 force reread of index from index file abs(lugi).
!> - =lugb force regeneration of index from grib2 file lugb.
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
!> (see code table 1.0) 0 experimental;1 initial operational version number.
!> - JIDS(4) grib local tables version number (see code table 1.1)
!> 0 local tables not used; 1-254 number of local tables version used.
!> - JIDS(5) significance of reference time (code table 1.2)
!> 0 analysis; 1 start of forecast; 2 verifying time of forecast; 3 observation time
!> - JIDS(6) year (4 digits)
!> - JIDS(7) month
!> - JIDS(8) day
!> - JIDS(9) hour
!> - JIDS(10) minute
!> - JIDS(11) second
!> - JIDS(12) production status of processed data (see code table 1.3)
!> 0 operational products; 1 operational test products;
!> 2 research products; 3 re-analysis products.
!> - JIDS(13) type of processed data (see code table 1.4)
!> 0 analysis products; 1 forecast products; 2 analysis and forecast
!> products; 3 control forecast products; 4 perturbed forecast products;
!> 5 control and perturbed forecast products; 6 processed satellite
!> observations; 7 processed radar observations.
!> @param[in] JPDTN integer product definition template number (n)
!> (if = -1, don't bother matching pdt - accept any)
!> @param[in] JPDT integer array of values defining the product definition
!> template 4.n of the field for which to search (=-9999 for wildcard)
!> @param[in] JGDTN integer grid definition template number (m)
!> (if = -1, don't bother matching gdt - accept any )
!> @param[in] JGDT integer array of values defining the grid definition
!> template 3.m of the field for which to search (=-9999 for wildcard)
!> @param[in] UNPACK logical value indicating whether to unpack bitmap/data
!> - .TRUE. unpack bitmap and data values
!> - .FALSE. do not unpack bitmap and data values
!> @param[out] K integer field number unpacked
!> @param[out] GFLD derived type @ref grib_mod::gribfield.
!> @param[out] IRET integer return code
!> - 0 all ok
!> - 96 error reading index
!> - 97 error reading grib file
!> - 99 request not found
!> - other gf_getfld grib2 unpacker return code
!>
!> @note Specify an index file if feasible to increase speed. Do
!> not engage the same logical unit from more than one
!> processor. Note that derived type gribfield contains pointers to
!> many arrays of data. The memory for these arrays is allocated
!> when the values in the arrays are set, to help minimize problems
!> with array overloading. Because of this users should free this
!> memory, when it is no longer needed, by a call to subroutine
!> gf_free().
!>
!> @author Mark Iredell @date 1994-04-01
subroutine getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
        unpack, k, gfld, iret)
    use grib_mod
    implicit none

    integer, intent(in) :: lugb, lugi, j, jdisc, jpdtn, jgdtn
    integer, dimension(:) :: jids(*), jpdt(*), jgdt(*)
    logical, intent(in) :: unpack
    integer, intent(out) :: k, iret
    type(gribfield), intent(out) :: gfld

    character(len =1), pointer, dimension(:) :: cbuf

    !implicit none additions
    integer :: irgi, irgs, jk, lpos, nlen, nnum

    !  declare interfaces (required for cbuf pointer)
    interface
        subroutine getidx(lugb, lugi, cbuf, nlen, nnum, irgi)
        character(len = 1), pointer, dimension(:) :: cbuf
        integer, intent(in) :: lugb, lugi
        integer, intent(out) :: nlen, nnum, irgi
        end subroutine getidx
    end interface

    !  determine whether index buffer needs to be initialized
    irgi = 0
    call getidx(lugb, lugi, cbuf, nlen, nnum, irgi)
    if(irgi .gt. 1) then
        iret = 96
        return
    endif

    !  search index buffer
    call getgb2s(cbuf, nlen, nnum, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
        jk, gfld, lpos, irgs)
    if(irgs .ne. 0) then
        iret = 99
        call gf_free(gfld)
        return
    endif

    !  read local use section, if available
    call getgb2l(lugb, cbuf(lpos), gfld, iret)

    !  read and unpack grib record
    if (unpack) then
        !    numfld=gfld%ifldnum
        !    call gf_free(gfld)
        call getgb2r(lugb, cbuf(lpos), gfld, iret)
    endif
    k = jk

    return
end subroutine getgb2
