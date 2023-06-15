!> @file
!> @brief Contains subroutines unpacks Section 7 ([Data Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect7.shtml))
!> of a GRIB2 message.
!> @author Stephen Gilbert @date 2002-01-24

!> This subroutine unpacks Section 7 ([Data Section]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect7.shtml))
!> of a GRIB2 message.
!>
!> @param[in] cgrib Character array that contains the GRIB2 message.
!> @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!> @param[inout] iofst Bit offset of the beginning/end(returned) of
!> Section 7.
!> @param[in] igdsnum Grid Definition Template Number (Code Table
!> 3.0) (Only required to unpack DRT 5.51).
!> @param[in] igdstmpl Pointer to an integer array containing the
!> data values for the specified Grid Definition. Template
!> (N=igdsnum). Each element of this integer array contains an entry
!> (in the order specified) of Grid Definition Template 3.N (Only
!> required to unpack DRT 5.51).
!> @param[in] idrsnum Data Representation Template Number ([Code Table
!> 5.0]
!> (https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table5-0.shtml)).
!> @param[in] idrstmpl Pointer to an integer array containing the data
!> values for the Data Representation Template specified by idrsnum. A
!> safe dimension for this array can be obtained in advance from
!> maxvals(6), which is returned from subroutine gribinfo.
!> @param[in] ndpts Number of data points unpacked and returned.
!> @param[out] fld Pointer to a real array containing the unpacked
!> data field.
!> @param[out] ierr Error return code.
!> - 0 no error.
!> - 4 Unrecognized Data Representation Template.
!> - 5 One of GDT 3.50 through 3.53 required to unpack DRT 5.51.
!> - 6 memory allocation error.
!> - 7 corrupt section 7.
!>
!> @author Stephen Gilbert @date 2002-01-24
      subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl,
     & idrsnum,idrstmpl,ndpts,fld,ierr)

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,ndpts,igdsnum,idrsnum
      integer,intent(inout) :: iofst
      integer,pointer,dimension(:) :: igdstmpl,idrstmpl
      integer,intent(out) :: ierr
      real,pointer,dimension(:) :: fld


      ierr=0
      nullify(fld)

      call g2_gbytec(cgrib,lensec,iofst,32) ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8 ! skip section number

      ipos=(iofst/8)+1
      istat=0
      allocate(fld(ndpts),stat=istat)
      if (istat.ne.0) then
         ierr=6
         return
      endif

      if (idrsnum.eq.0) then
        call simunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)
      elseif (idrsnum.eq.2.or.idrsnum.eq.3) then
        call comunpack(cgrib(ipos),lensec-5,lensec,idrsnum,idrstmpl,
     & ndpts,fld,ier)
        if ( ier .NE. 0 ) then
           ierr=7
           return
        endif
      elseif (idrsnum.eq.50) then ! Spectral simple
        call simunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts-1,
     & fld(2))
        ieee=idrstmpl(5)
        call rdieee(ieee,fld(1),1)
      elseif (idrsnum.eq.51) then ! Spectral complex
        if (igdsnum.ge.50.AND.igdsnum.le.53) then
          call specunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,
     & igdstmpl(1),igdstmpl(2),igdstmpl(3),fld)
        else
          print *,'gf_unpack7: Cannot use GDT 3.',igdsnum,
     & ' to unpack Data Section 5.51.'
          ierr=5
          nullify(fld)
          return
        endif

      elseif (idrsnum.eq.40 .OR. idrsnum.eq.40000) then
        call jpcunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)


      elseif (idrsnum.eq.41 .OR. idrsnum.eq.40010) then
        call pngunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)

      else
        print *,'gf_unpack7: Data Representation Template ',idrsnum,
     & ' not yet implemented.'
        ierr=4
        nullify(fld)
        return
      endif

      iofst=iofst+(8*lensec)

      return ! End of Section 7 processing
      end
