!> @file
!> @brief Module for the NCEPLIBS-g2 file-based GRIB2 API.
!> @author Edward Hartnett @date 2020-16-12

!> @brief Module for the NCEPLIBS-g2 file-based GRIB2 API.
!>
!> @author Edward Hartnett @date 2020-16-12
module g2cf
  use g2c_interface

contains
  !> Add a C_NULL_CHAR to a string to create a C compatible
  !> string. Assumes target variable will be of length
  !> LEN(string)+1. Trailing blanks will be stripped from string and
  !> length of trimmed string will be returned in nlen.
  !>
  !> @param string the string to trimmed and null-terminated
  !> @param nlen the length of the returned string, including
  !> null-terminator.
  !>
  !> @return the trimmed, null-terminated string
  !>
  !> This function was originally written by, Richard Weed, Ph.D., as part of
  !> netcdf-fortran.
  !>
  !> @author Edward Hartnett @date 2024-06-12
  function addcnullchar(string, nlen) result(cstring)
    use iso_c_binding
    implicit none

    character(len=*), intent(in)    :: string
    integer,          intent(inout) :: nlen
    character(len = (len(string) + 1))  :: cstring

    integer :: inull

    ! First check to see if we already have a C NULL char attached
    ! to string and strip trailing blanks. We will use it if its present 
    ! otherwise we add one. The length of the trimmed string plus the
    ! C_NULL_CHAR is returned in nlen.
    nlen  = len_trim(string)
    inull = scan(string, C_NULL_CHAR)
    cstring = repeat(" ", len(cstring)) ! init to blanks
    if (inull > 0)  then ! string has a NULL char
       nlen = inull
       cstring = string(1:nlen)
    else ! append null char to trimmed string
       cstring = string(1:nlen)//C_NULL_CHAR
       nlen = nlen + 1
    endif
  end function addcnullchar

  !> Open a GRIB2 file.
  !>
  !> @param path the path to the file
  !> @param mode flag with open mode information
  !> @param g2id the ID of the open file
  !>
  !> @return 0 for success, error code otherwise.
  !>
  !> @author Edward Hartnett @date 2024-06-12
  function g2cf_open(path, mode, g2id) result (status)
    use iso_c_binding    
    implicit none
    character(len = *), intent(in) :: path
    integer, intent(in) :: mode
    integer, intent(inout) :: g2id
    integer :: status

    integer(c_int) :: cmode, cg2id, cstatus
    character(len = (len(path) + 1)) :: cpath
    integer :: ie
    
    cmode = mode
    cg2id = 0
    
    ! Check for C null character on path and add one if not present.
    cpath = addCNullChar(path, ie) 
    
    ! Call g2c_open to open GRIB2 file.
    ! cstatus = g2c_open(cpath(1:ie), cmode, cg2id)
    
    ! If (cstatus == NC_NOERR) Then
    !    g2id   = cg2id
    ! EndIf
    ! status = cstatus

  end function g2cf_open

end module g2cf
