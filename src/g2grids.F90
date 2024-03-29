!> @file
!> @brief This Fortran Module allows access to predefined GRIB2 Grid
!> Definition Templates (GDT) stored in a file.
!> @author Stephen Gilbert @date 2004-04-27

!> @brief Allow access to predefined GRIB2 Grid Definition Templates
!> (GDT) stored in a file.
!>
!> The GDTs are represented by a predefined number or a character
!> abbreviation. At the first request, all the grid GDT entries in the
!> file associated with input Fortran file unit number, lunit, are
!> read into a linked list named gridlist. This list is searched for
!> the requested entry. Users of this Fortran module should only call
!> routines getgridbynum() and getgridbyname(), and freegridlist() to
!> release all allocated memory.
!>
!> The format of the file scanned by routines in this module is as
!> follows. Each line contains one Grid entry containing five
!> fields, each separated by a colon, ":". The fields are:
!>
!> 1. predefined grid number
!> 2. Up to an 8 character abbreviation
!> 3. Grid Definition Template number
!> 4. Number of entries in the Grid Definition Template
!> 5. A list of values for each entry in the Grid Definition Template.
!>
!> As an example, this is the entry for the 1x1 GFS global grid:
!> <pre>
!> 3:gbl_1deg: 0:19: 0 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0
!> </pre>
!>
!> @author Stephen Gilbert @date 2004-04-27
module g2grids
  implicit none
  integer, parameter :: MAXTEMP = 200 !< maximum template number for grid definition.

  type,private :: g2grid
     integer :: grid_num
     integer :: gdt_num
     integer :: gdt_len
     integer, dimension(MAXTEMP) :: gridtmpl
     character(len = 8) :: cdesc
     type(g2grid), pointer :: next
  end type g2grid

  type(g2grid), pointer, private :: gridlist
  integer :: num_grids = 0 !< the number of grids.

contains

  !> This function reads the list of GDT entries in the file
  !> associated with fortran unit, lunit. All the entries are stored in a
  !> linked list called gridlist.
  !>
  !> @param[in] lunit Fortran unit number associated the the GDT file.
  !> @return The number of Grid Definition Templates read in.
  !> @author Stephen Gilbert  @date 2001-06-28
  integer function readgrids(lunit)
    implicit none

    integer, intent(in) :: lunit

    integer, parameter :: linelen = 1280
    character(len = 8) :: desc
    character(len = linelen) :: cline
    integer :: ient, igdtn, igdtmpl(200), igdtlen
    integer :: pos1, pos2, pos3, pos4
    type(g2grid), pointer :: gtemp
    type(g2grid), pointer :: prev => NULL()
    integer :: count

    integer :: j

    count = 0

    ! For each line in the file....
    do
       ! Read line into buffer.
       cline(1 : linelen) = ' '
       read(lunit, end = 999, fmt = '(a)') cline

       ! Skip line if commented out.
       if (cline(1 : 1) .eq. '#') cycle

       ! Find positions of delimiters, ":".
       pos1 = index(cline, ':')
       cline(pos1 : pos1) = ';'
       pos2 = index(cline, ':')
       cline(pos2 : pos2) = ';'
       pos3 = index(cline, ':')
       cline(pos3 : pos3) = ';'
       pos4 = index(cline, ':')
       if ( pos1 .eq. 0 .or. pos2 .eq. 0 .or. pos3 .eq. 0 .or.  &
            pos4 .eq. 0) cycle

       ! Read each of the five fields.
       read(cline(1 : pos1 - 1), *) ient
       read(cline(pos1 + 1 : pos2 - 1), *) desc
       read(cline(pos2 + 1 : pos3 - 1), *) igdtn
       read(cline(pos3 + 1 : pos4 - 1), *) igdtlen
       read(cline(pos4 + 1 : linelen), *) (igdtmpl(j), j = 1, igdtlen)

       ! Allocate new type(g2grid) variable to store the GDT.
       allocate(gtemp)
       count = count + 1
       gtemp%grid_num = ient
       gtemp%gdt_num = igdtn
       gtemp%gdt_len = igdtlen
       gtemp%gridtmpl = igdtmpl
       gtemp%cdesc = desc
       nullify(gtemp%next)              ! defines end of linked list.
       if ( count .eq. 1 ) then
          gridlist => gtemp
       else                       ! make sure previous entry in list
          prev%next => gtemp      ! points to the new entry,
       endif
       prev => gtemp
    enddo
999 backspace(lunit)
    readgrids = count
    return

  end function readgrids

  !> This subroutine searches a file referenced by fortran unit lunit
  !> for a Grid Definition Template assigned to the requested number.
  !> The input file format is described at the top of this module.
  !>
  !> Callers should call freegridlist() to free the memory allocated
  !> when this function is called.
  !>
  !> @param[in] lunit Unit number of file containing Grid definitions
  !> @param[in] number Grid number of the requested Grid definition
  !> @param[out] igdtn NN, indicating the number of the Grid Definition
  !> Template 3.NN
  !> @param[out] igdtmpl An array containing the values of each entry in
  !> the Grid Definition Template.
  !> @param[out] iret Error return code.
  !> - 0 no error
  !> - -1 Undefined Grid number.
  !> - 3 Could not read any grids from file.
  !>
  !> @author Stephen Gilbert @date 2004-04-26
  subroutine getgridbynum(lunit, number, igdtn, igdtmpl, iret)
    implicit none
    integer, intent(in) :: lunit, number
    integer, intent(out) :: igdtn, igdtmpl(*), iret
    type(g2grid), pointer :: tempgrid

    iret = 0
    igdtn = -1

    ! If no grids in list, try reading them from the file.
    if (num_grids .eq. 0) then
       num_grids = readgrids(lunit)
    endif

    if (num_grids .eq. 0) then
       iret = 3                         ! problem reading file
       return
    endif

    tempgrid => gridlist

    ! Search through list.
    do while (associated(tempgrid))
       if (number .eq. tempgrid%grid_num) then
          igdtn = tempgrid%gdt_num
          igdtmpl(1 : tempgrid%gdt_len) = tempgrid%gridtmpl(1 : tempgrid%gdt_len)
          return
       else
          tempgrid => tempgrid%next
       endif
    enddo

    iret = -1
    return

  end subroutine getgridbynum

  !> This subroutine searches a file referenced by fortran unit lunit
  !> for a Grid Definition Template assigned to the requested name.
  !> The input file format is described at the top of this module.
  !>
  !> Callers should call freegridlist() to free the memory allocated
  !> when this function is called.
  !>
  !> @param[in] lunit Unit number of file containing Grid definitions
  !> @param[in] name Grid name of the requested Grid definition
  !> @param[out] igdtn NN, indicating the number of the Grid Definition
  !> Template 3.NN
  !> @param[out] igdtmpl An array containing the values of each entry in
  !> the Grid Definition Template.
  !> @param[out] iret Error return code.
  !> - 0 no error
  !> - -1 Undefined Grid name.
  !> - 3 Could not read any grids from file.
  !>
  !> @author Stephen Gilbert @date 2004-04-26
  subroutine getgridbyname(lunit, name, igdtn, igdtmpl, iret)
    implicit none
    integer, intent(in) :: lunit
    character(len = 8), intent(in) :: name
    integer, intent(out) :: igdtn, igdtmpl(*), iret
    type(g2grid), pointer :: tempgrid

    iret = 0
    igdtn = -1

    ! If no grids in list, try reading them from the file.
    if (num_grids .eq. 0) then
       num_grids = readgrids(lunit)
    endif

    if (num_grids .eq. 0) then
       iret = 3                         ! problem reading file
       return
    endif

    tempgrid => gridlist

    ! Search through list.
    do while (associated(tempgrid))
       if (name .eq. tempgrid%cdesc) then
          igdtn = tempgrid%gdt_num
          igdtmpl(1:tempgrid%gdt_len) = tempgrid%gridtmpl(1:tempgrid%gdt_len)
          return
       else
          tempgrid => tempgrid%next
       endif
    enddo

    iret = -1
    return
  end subroutine getgridbyname

  !> This subroutine frees the memory allocated for the linked list
  !> of grid templates stored in module variable gridlist.
  !>
  !> @author Ed Hartnett @date July 16, 2022
  subroutine freegridlist()
    implicit none
    integer :: i
    type(g2grid), pointer :: gridnext

    do i = 1, num_grids
       gridnext => gridlist%next
       deallocate(gridlist)
       gridlist => gridnext
    end do
  end subroutine freegridlist
end module g2grids
