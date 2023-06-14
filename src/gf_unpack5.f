!> @file
!> @brief Contains subroutines unpacks Section 5 ([Data Representation
!> Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect5.shtml))
!> of a GRIB2 message.
!> @author Stephen Gilbert @date 2000-05-26

!> This subroutine unpacks Section 5 ([Data Representation Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect5.shtml))
!> of a GRIB2 message, starting at octet 6 of that Section.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 5.
!> @param[out] ndpts Number of data points unpacked and returned.
!> @param[out] idrsnum Data Representation Template Number ([Code Table 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[out] idrstmpl Contains the data values for the specified
!> Data Representation Template (N=idrsnum). Each element of this
!> integer array contains an entry (in the order specified) of
!> Product Defintion Template 5.N A safe dimension for this array can
!> be obtained in advance from maxvals(6), which is returned from
!> subroutine gribinfo.
!> @param[out] mapdrslen Number of elements in idrstmpl. i.e. number
!> of entries in Data Representation Template 5.N (N=idrsnum).
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 6 memory allocation error.
!> - 7 "GRIB" message contains an undefined Grid Definition Template.
!>
!> @author Stephen Gilbert @date 2000-05-26
      subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
     &                   mapdrslen,ierr)

      use drstemplates
      use re_alloc        !  needed for subroutine realloc

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,intent(out) :: ndpts,idrsnum
      integer,pointer,dimension(:) :: idrstmpl
      integer,intent(out) :: ierr

      integer,allocatable :: mapdrs(:)
      integer :: mapdrslen
      logical needext

      ierr=0
      nullify(idrstmpl)

      call g2_gbytec(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      allocate(mapdrs(lensec))

      call g2_gbytec(cgrib,ndpts,iofst,32)    ! Get num of data points
      iofst=iofst+32
      call g2_gbytec(cgrib,idrsnum,iofst,16)     ! Get Data Rep Template Num.
      iofst=iofst+16
      !   Gen Data Representation Template
      call getdrstemplate(idrsnum,mapdrslen,mapdrs,needext,iret)
      if (iret.ne.0) then
        ierr=7
        if( allocated(mapdrs) ) deallocate(mapdrs)
        return
      endif
      !
      !   Unpack each value into array ipdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mappds.
      !
      istat=0
      if (mapdrslen.gt.0) allocate(idrstmpl(mapdrslen),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(idrstmpl)
         if( allocated(mapdrs) ) deallocate(mapdrs)
         return
      endif
      do i=1,mapdrslen
        nbits=iabs(mapdrs(i))*8
        if ( mapdrs(i).ge.0 ) then
          call g2_gbytec(cgrib,idrstmpl(i),iofst,nbits)
        else
          call g2_gbytec(cgrib,isign,iofst,1)
          call g2_gbytec(cgrib,idrstmpl(i),iofst+1,nbits-1)
          if (isign.eq.1) idrstmpl(i)=-idrstmpl(i)
        endif
        iofst=iofst+nbits
      enddo
      !
      !   Check to see if the Data Representation Template needs to be
      !   extended.
      !   The number of values in a specific template may vary
      !   depending on data specified in the "static" part of the
      !   template.
      !
      if ( needext ) then
        call extdrstemplate(idrsnum,idrstmpl,newmapdrslen,mapdrs)
        call realloc(idrstmpl,mapdrslen,newmapdrslen,istat)
        !   Unpack the rest of the Data Representation Template
        do i=mapdrslen+1,newmapdrslen
          nbits=iabs(mapdrs(i))*8
          if ( mapdrs(i).ge.0 ) then
            call g2_gbytec(cgrib,idrstmpl(i),iofst,nbits)
          else
            call g2_gbytec(cgrib,isign,iofst,1)
            call g2_gbytec(cgrib,idrstmpl(i),iofst+1,nbits-1)
            if (isign.eq.1) idrstmpl(i)=-idrstmpl(i)
          endif
          iofst=iofst+nbits
        enddo
        mapdrslen=newmapdrslen
      endif
      if( allocated(mapdrs) ) deallocate(mapdrs)

      return    ! End of Section 5 processing
      end

