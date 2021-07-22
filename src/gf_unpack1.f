!>    @file
!>    @brief This subroutine unpacks Section 1 (Identification
!>    Section).
!>    @author Stephen Gilbert @date 2000-05-26
!>

!>    This subroutine unpacks Section 1 (Identification Section)
!>    starting at octet 6 of that Section.
!>    
!>    PROGRAM HISTORY LOG:
!>    - 2000-05-26 Stephen Gilbert Initial development.
!>    - 2002-01-24 Stephen Gilbert Changed to dynamically allocate
!>    arrays and to pass pointers to those arrays through the argument
!>    list.
!>    
!>    @param[in] cgrib Character array that contains the GRIB2 message.
!>    @param[in] lcgrib Length (in bytes) of GRIB message array cgrib.
!>    @param[inout] iofst Bit offset of the beginning/end(returned) of Section 1.
!>    @param[out] ids Pointer to integer array containing information
!>    read from Section 1, the Identification section.
!>    - ids(1) Identification of originating Centre (Common Code Table C-1).
!>    - ids(2) Identification of originating Sub-centre.
!>    - ids(3) GRIB Master Tables Version Number (Code Table 1.0).
!>    - ids(4) GRIB Local Tables Version Number (Code Table 1.1).
!>    - ids(5) Significance of Reference Time (Code Table 1.2).
!>    - ids(6) Year (4 digits).
!>    - ids(7) Month
!>    - ids(8) Day
!>    - ids(9) Hour
!>    - ids(10) Minute
!>    - ids(11) Second
!>    - ids(12) Production status of processed data (Code Table 1.3)
!>    - ids(13) Type of processed data (Code Table 1.4)
!>    @param[out] idslen Number of elements in ids.
!>    @param[out] ierr Error return code.
!>    - 0 no error.
!>    - 6 memory allocation error.
!>
!>    @author Stephen Gilbert @date 2000-05-26
!>

      subroutine gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,pointer,dimension(:) :: ids
      integer,intent(out) :: ierr,idslen

      integer,dimension(:) :: mapid(13)

      data mapid /2,2,1,1,1,2,1,1,1,1,1,1,1/

      ierr=0
      idslen=13
      nullify(ids)

      call g2_gbytec(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      !
      !   Unpack each value into array ids from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mapid.
      !
      istat=0
      allocate(ids(idslen),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(ids)
         return
      endif
      
      do i=1,idslen
        nbits=mapid(i)*8
        call g2_gbytec(cgrib,ids(i),iofst,nbits)
        iofst=iofst+nbits
      enddo
      
      return    ! End of Section 1 processing
      end
