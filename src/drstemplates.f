!>    @file
!>    @brief This Fortran Module contains info on all the available
!>    GRIB2 Data Representation Templates used in Section 5 (DRS).
!>    @author Stephen Gilbert @date 2001-04-03
!>     

!>    This Fortran Module contains info on all the available
!>    GRIB2 Data Representation Templates used in Section 5 (DRS).
!>    Each Template has three parts: The number of entries in the template
!>    (mapgridlen);  A map of the template (mapgrid), which contains the
!>    number of octets in which to pack each of the template values; and
!>    a logical value (needext) that indicates whether the Template needs 
!>    to be extended.  In some cases the number of entries in a template 
!>    can vary depending upon values specified in the "static" part of 
!>    the template.  ( See Template 5.1 as an example )
!>
!>    This module also contains two subroutines.  Subroutine getdrstemplate
!>    returns the octet map for a specified Template number, and
!>    subroutine extdrstemplate will calculate the extended octet map
!>    of an appropriate template given values for the "static" part of the 
!>    template.  See docblocks below for the arguments and usage of these 
!>    routines.
!>
!>    @note Array mapgrid contains the number of octets in which the 
!>    corresponding template values will be stored.  A negative value in
!>    mapgrid is used to indicate that the corresponding template entry can
!>    contain negative values.  This information is used later when packing
!>    (or unpacking) the template data values.  Negative data values in GRIB
!>    are stored with the left most bit set to one, and a negative number
!>    of octets value in mapgrid indicates that this possibility should
!>    be considered.  The number of octets used to store the data value
!>    in this case would be the absolute value of the negative value in 
!>    mapgrid.
!>     
!>    @author Stephen Gilbert @date 2001-04-03
!>
      module drstemplates

      integer,parameter :: MAXLEN=200,MAXTEMP=9

      type drstemplate
          integer :: template_num
          integer :: mapdrslen
          integer,dimension(MAXLEN) :: mapdrs
          logical :: needext
      end type drstemplate

      type(drstemplate),dimension(MAXTEMP) :: templates

      data templates(1)%template_num /0/     !<     Simple Packing
      data templates(1)%mapdrslen /5/ 
      data templates(1)%needext /.false./
      data (templates(1)%mapdrs(j),j=1,5) 
     &                             /4,-2,-2,1,1/

      data templates(2)%template_num /2/     !<     Complex Packing
      data templates(2)%mapdrslen /16/
      data templates(2)%needext /.false./
      data (templates(2)%mapdrs(j),j=1,16)
     &                        /4,-2,-2,1,1,1,1,4,4,4,1,1,4,1,4,1/

      data templates(3)%template_num /3/     !<     Complex Packing - Spatial Diff
      data templates(3)%mapdrslen /18/
      data templates(3)%needext /.false./
      data (templates(3)%mapdrs(j),j=1,18)
     &                        /4,-2,-2,1,1,1,1,4,4,4,1,1,4,1,4,1,1,1/

      data templates(4)%template_num /50/     !<     Simple Packing - Spectral Data
      data templates(4)%mapdrslen /5/
      data templates(4)%needext /.false./
      data (templates(4)%mapdrs(j),j=1,5)
     &                         /4,-2,-2,1,4/

      data templates(5)%template_num /51/    !<     Complex Packing - Spectral Data
      data templates(5)%mapdrslen /10/
      data templates(5)%needext /.false./
      data (templates(5)%mapdrs(j),j=1,10)
     &                         /4,-2,-2,1,-4,2,2,2,4,1/

      data templates(6)%template_num /40000/     !<     JPEG2000 Encoding
      data templates(6)%mapdrslen /7/ 
      data templates(6)%needext /.false./
      data (templates(6)%mapdrs(j),j=1,7) 
     &                             /4,-2,-2,1,1,1,1/

      data templates(7)%template_num /40010/     !<     PNG Encoding
      data templates(7)%mapdrslen /5/ 
      data templates(7)%needext /.false./
      data (templates(7)%mapdrs(j),j=1,5) 
     &                             /4,-2,-2,1,1/

      data templates(8)%template_num /40/     !<     JPEG2000 Encoding
      data templates(8)%mapdrslen /7/ 
      data templates(8)%needext /.false./
      data (templates(8)%mapdrs(j),j=1,7) 
     &                             /4,-2,-2,1,1,1,1/

      data templates(9)%template_num /41/     !<     PNG Encoding
      data templates(9)%mapdrslen /5/ 
      data templates(9)%needext /.false./
      data (templates(9)%mapdrs(j),j=1,5) 
     &                             /4,-2,-2,1,1/

!         data templates(5)%template_num /1/      !<     Simple Packing - Matrix
!         data templates(5)%mapdrslen /15/ 
!         data templates(5)%needext /.true./
!         data (templates(5)%mapdrs(j),j=1,15)
!        &                        /4,-2,-2,1,1,1,4,2,2,1,1,1,1,1,1/


      contains

!>    @brief  This function returns the index of specified Data
!>    Representation Template 5.NN (NN=number) in array templates.                                    .
!>    @author Stephen Gilbert            @date 2001-06-28
!>     

!>    @param[in] number NN, indicating the number of the Data Representation 
!>    Template 5.NN that is being requested.
!>
!>    @return Index of DRT 5.NN in array templates, if template exists.
!>    = -1, otherwise.
!>
!>    @author Stephen Gilbert            @date 2001-06-28
!>
         integer function getdrsindex(number)
           integer,intent(in) :: number

           getdrsindex=-1

           do j=1,MAXTEMP
              if (number.eq.templates(j)%template_num) then
                 getdrsindex=j
                 return
              endif
           enddo

         end function

!>    @brief This subroutine returns DRS template information for a                                   .
!>    specified Data Representation Template 5.NN.
!>    @author Stephen Gilbert            @date 2000-05-11
!>     

!>    The number of entries in the template is returned along with a map
!>    of the number of octets occupied by each entry. Also, a flag is
!>    returned to indicate whether the template would need to be extended.
!>
!>    @param[in] number NN, indicating the number of the Data Representation 
!>    Template 5.NN that is being requested.
!>    @param[out] nummap Number of entries in the Template
!>    @param[out] map An array containing the number of octets that each 
!>    template entry occupies when packed up into the DRS.
!>    @param[out] needext Logical variable indicating whether the Data Representation
!>    Template has to be extended.  
!>    @param[out] iret Error return code.
!>    - 0 = no error
!>    - 1 = Undefined Data Representation Template number.
!>
!>    @author Stephen Gilbert            @date 2000-05-11
!>
         subroutine getdrstemplate(number,nummap,map,needext,iret)
           integer,intent(in) :: number
           integer,intent(out) :: nummap,map(*),iret
           logical,intent(out) :: needext

           iret=0

           index=getdrsindex(number)

           if (index.ne.-1) then
              nummap=templates(index)%mapdrslen
              needext=templates(index)%needext
              map(1:nummap)=templates(index)%mapdrs(1:nummap)
           else
             nummap=0
             needext=.false.
             print *,'getdrstemplate: DRS Template ',number,
     &               ' not defined.'
             iret=1
           endif

         end subroutine

!>    @brief This subroutine generates the remaining octet map for a
!>    given Data Representation Template, if required.
!>    @author  Stephen Gilbert            @date 2000-05-11
!>     

!>    Some Templates can vary depending on data values given in an earlier part 
!>    of the Template, and it is necessary to know some of the earlier entry
!>    values to generate the full octet map of the Template.
!>
!>    @param[in] number NN, indicating the number of the Data Representation 
!>    Template 5.NN that is being requested.
!>    @param[in] list The list of values for each entry in the 
!>    Data Representation Template 5.NN.
!>    @param[out] nummap Number of entries in the Template
!>    @param[out] map An array containing the number of octets that each 
!>    template entry occupies when packed up into the GDS.
!>
!>    @author  Stephen Gilbert            @date 2000-05-11
!>
         subroutine extdrstemplate(number,list,nummap,map)
           integer,intent(in) :: number,list(*)
           integer,intent(out) :: nummap,map(*)

           index=getdrsindex(number)
           if (index.eq.-1) return

           if ( .not. templates(index)%needext ) return
           nummap=templates(index)%mapdrslen
           map(1:nummap)=templates(index)%mapdrs(1:nummap)

           if ( number.eq.1 ) then
              N=list(11)+list(13)
              do i=1,N
                map(nummap+i)=4
              enddo
              nummap=nummap+N
           endif

         end subroutine

      end module



