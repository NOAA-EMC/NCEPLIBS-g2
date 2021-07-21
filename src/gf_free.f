!>    @file
!>    @brief This subroutine frees up memory in derived type gribfield.
!>    @author Stephen Gilbert @date 2000-05-26
!>

!>    This subroutine frees up memory that was used to store
!>    array values in derived type gribfield.
!>
!>    Program History log:
!>    - 2000-05-26 Stephen Gilbert Modified from getg1s to work with grib2
!>    - 2012-12-11 Boi Vuong initialize an undefine pointers
!>    - 2015-10-29 Boi Vuong Deallocate pointers in derived type gribfield
!>    @param[in] gfld derived type gribfield (defined in module grib_mod)
!>    @param[out] gfld derived type gribfield (defined in module grib_mod)
!>    (NOTE: See Remarks Section)
!>    - gfld\%version GRIB edition number (currently 2)
!>    - gfld\%discipline Message Discipline (see Code Table 0.0)
!>    - gfld\%idsect Contains the entries in the Identification Section
!>    (Section 1) This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%idsect(1) Identification of originating Centre
!>    (see Common Code Table C-1) 7 US National Weather Service
!>    - gfld\%idsect(2) Identification of originating Sub-centre
!>    - gfld\%idsect(3) GRIB Master Tables Version Number
!>    (see Code Table 1.0) 0 Experimental; 1 Initial operational version number
!>    - gfld\%idsect(4) GRIB Local Tables Version Number (see Code Table 1.1)
!>     - 0 Local tables not used
!>     - 0 1-254 Number of local tables version used
!>    - gfld\%idsect(5) Significance of Reference Time (Code Table 1.2)
!>     - 0 Analysis
!>     - 1 Start of forecast
!>     - 2 Verifying time of forecast
!>     - 3 Observation time.
!>    - gfld\%idsect(6) Year (4 digits)
!>    - gfld\%idsect(7) Month
!>    - gfld\%idsect(8) Day
!>    - gfld\%idsect(9) Hour
!>    - gfld\%idsect(10) Minute
!>    - gfld\%idsect(11) Second
!>    - gfld\%idsect(12) Production status of processed data (see Code
!>    Table 1.3)
!>     - 0 Operational products
!>     - 1 Operational test products
!>     - 2 Research products
!>     - 3 Re-analysis products
!>    - gfld\%idsect(13) Type of processed data (see Code Table 1.4)
!>     - 0 Analysis products
!>     - 1 Forecast products
!>     - 2 Analysis and forecast products
!>     - 3 Control forecast products
!>     - 4 Perturbed forecast products
!>     - 5 Control and perturbed forecast products
!>     - 6 Processed satellite observations
!>     - 7 Processed radar observations
!>    - gfld\%idsectlen Number of elements in gfld\%idsect
!>    - gfld\%local Pointer to character array containing contents
!>    of Local Section 2, if included
!>    - gfld\%locallen length of array gfld\%local
!>    - gfld\%ifldnum field number within GRIB message
!>    - gfld\%griddef Source of grid definition (see Code Table 3.0)
!>     - 0 Specified in Code table 3.1
!>     - 1 Predetermined grid Defined by originating centre
!>    - gfld\%ngrdpts Number of grid points in the defined grid.
!>    Note that the number of actual data values returned from getgb2
!>    (in gfld\%ndpts) may be less than this value if a logical bitmap
!>    is in use with grid points that are being masked out.
!>    - gfld\%numoct_opt Number of octets needed for each additional grid
!>    points definition. Used to define number of points in each row (or
!>    column) for non-regular grids. = 0, if using regular grid.
!>    - gfld\%interp_opt Interpretation of list for optional points
!>    definition.(Code Table 3.11)
!>    - gfld\%igdtnum Grid Definition Template Number (Code Table 3.1)
!>    - gfld\%igdtmpl Contains the data values for the specified Grid
!>    Definition Template (NN=gfld\%igdtnum). Each element of this
!>    integer array contains an entry (in the order specified) of Grid
!>    Defintion Template 3.NN This element is actually a pointer to an
!>    array that holds the data.
!>    - gfld\%igdtlen Number of elements in gfld\%igdtmpl. i.e. number
!>    of entries in Grid Defintion Template 3.NN (NN=gfld\%igdtnum).
!>    - gfld\%list_opt (Used if gfld\%numoct_opt .ne. 0) This array
!>    contains the number of grid points contained in each row (or
!>    column). (part of Section 3) This element is actually a pointer
!>    to an array that holds the data. This pointer is nullified
!>    if gfld\%numoct_opt=0.
!>    - gfld\%num_opt (Used if gfld\%numoct_opt .ne. 0) The number of
!>    entries in array ideflist. i.e. number of rows (or columns) for which
!>    optional grid points are defined. This value is set to zero,
!>    if gfld\%numoct_opt=0.
!>    - gfdl\%ipdtnum Product Definition Template Number (Code Table 4.0)
!>    - gfld\%ipdtmpl Contains the data values for the specified Product
!>    Definition Template (N=gfdl\%ipdtnum). Each element of this integer
!>    array contains an entry (in the order specified) of Product Defintion
!>    Template 4.N. This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%ipdtlen Number of elements in gfld\%ipdtmpl. i.e. number of
!>    entries in Product Defintion Template 4.N (N=gfdl\%ipdtnum).
!>    - gfld\%coord_list Real array containing floating point values
!>    intended to document the vertical discretisation associated to
!>    model data on hybrid coordinate vertical levels.(part of Section 4)
!>    This element is actually a pointer to an array
!>    that holds the data.
!>    - gfld\%num_coord number of values in array gfld\%coord_list.
!>    - gfld\%ndpts Number of data points unpacked and returned.
!>    Note that this number may be different from the value of
!>    - gfld\%ngrdpts if a logical bitmap is in use with grid points
!>    that are being masked out.
!>    - gfld\%idrtnum Data Representation Template Number (Code Table 5.0)
!>    - gfld\%idrtmpl Contains the data values for the specified Data
!>    Representation Template (N=gfld\%idrtnum). Each element of this
!>    integer array contains an entry (in the order specified) of
!>    Product Defintion Template 5.N. This element is actually a
!>    pointer to an array that holds the data.
!>    - gfld\%idrtlen Number of elements in gfld\%idrtmpl. i.e. number
!>    of entries in Data Representation Template 5.N (N=gfld\%idrtnum).
!>    - gfld\%unpacked logical value indicating whether the bitmap and
!>    data values were unpacked. If false, gfld\%bmap and gfld\%fld
!>    pointers are nullified.
!>    - gfld\%expanded Logical value indicating whether the data field
!>    was expanded to the grid in the case where a bit-map is present.
!>    If true, the data points in gfld\%fld match the grid points and
!>    zeros were inserted at grid points where data was bit-mapped out.
!>    If false, the data values in gfld\%fld were not expanded to the
!>    grid and are just a consecutive array of data points corresponding
!>    to each value of "1" in gfld\%bmap.
!>    - gfld\%ibmap Bitmap indicator (see Code Table 6.0)
!>     - 0 bitmap applies and is included in Section 6.
!>     - 1-253 Predefined bitmap applies
!>     - 254 Previously defined bitmap applies to this field
!>     - 255 Bit map does not apply to this product.
!>    - gfld\%bmap Logical*1 array containing decoded bitmap, if ibmap=0
!>    or ibap=254. Otherwise nullified. This element is actually a
!>    pointer to an array that holds the data.
!>    - gfld\%fld Array of gfld\%ndpts unpacked data points. This element
!>    is actually a pointer to an array that holds the data.
!>    @author Stephen Gilbert @date 2000-05-26
!>

      subroutine gf_free(gfld)

      use grib_mod
    
      type(gribfield) :: gfld

      if (associated(gfld%idsect)) then
         !nullify(gfld%idsect)
         deallocate(gfld%idsect,stat=is)
         !print *,'gfld%idsect: ',is
      endif
      nullify(gfld%idsect)

      if (associated(gfld%local)) then
         ! nullify(gfld%local)
         deallocate(gfld%local,stat=is)
         !print *,'gfld%local: ',is
      endif
      nullify(gfld%local)

      if (associated(gfld%list_opt)) then
         ! nullify(gfld%list_opt)
         deallocate(gfld%list_opt,stat=is)
         !print *,'gfld%list_opt: ',is
      endif
      nullify(gfld%list_opt)

      if (associated(gfld%igdtmpl)) then
         !nullify(gfld%igdtmpl)
         deallocate(gfld%igdtmpl,stat=is)
         !print *,'gfld%igdtmpl: ',is
      endif
      nullify(gfld%igdtmpl)

      if (associated(gfld%ipdtmpl)) then
         !nullify(gfld%ipdtmpl)
         deallocate(gfld%ipdtmpl,stat=is)
         !print *,'gfld%ipdtmpl: ',is
      endif
      nullify(gfld%ipdtmpl)

      if (associated(gfld%coord_list)) then
         ! nullify(gfld%coord_list)
         deallocate(gfld%coord_list,stat=is)
         !print *,'gfld%coord_list: ',is
      endif
      nullify(gfld%coord_list)

      if (associated(gfld%idrtmpl)) then
         !nullify(gfld%idrtmpl)
         deallocate(gfld%idrtmpl,stat=is)
         !print *,'gfld%idrtmpl: ',is
      endif
      nullify(gfld%idrtmpl)

      if (associated(gfld%bmap)) then
         ! nullify(gfld%bmap)
         deallocate(gfld%bmap,stat=is)
         !print *,'gfld%bmap: ',is
      endif
      nullify(gfld%bmap)

      if (associated(gfld%fld)) then
        ! nullify(gfld%fld)
        deallocate(gfld%fld,stat=is)
        ! print *,'gfld%fld: ',is
      endif
      nullify(gfld%fld)

      return
      end
