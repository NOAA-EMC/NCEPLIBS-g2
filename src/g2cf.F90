!> @file
!> @brief Expose the g2c API from the NCEPLIBS-g2c GRIB2 library.
!> @author Ed Hartnett @date 2022-11-21

!> Module to expose g2c API from NCEPLIBS-g2c.
!>
!> @author Ed Hartnett @date 2022-11-21
module g2cf
  
contains
  !> Add a c_null_char to a string to create a c compatible
  !> string. assumes target variable will be of length
  !> len(string)+1. trailing blanks will be stripped
  !> from string and length of trimmed string will
  !> be returned in nlen.
  !>
  !> @param[in] string The string to null-terminate.
  !> @param[inout] nlen
  !>
  !> @return The null-terminated string.
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function addcnullchar(string, nlen) result(cstring)
    use iso_c_binding, only: c_null_char
    implicit none
    character(len=*), intent(in)    :: string
    integer, intent(inout) :: nlen
    character(len=(len(string)+1))  :: cstring
    integer :: inull

    ! first check to see if we already have a c null char attached
    ! to string and strip trailing blanks. we will use it if its present
    ! otherwise we add one. the length of the trimmed string plus the
    ! c_null_char is returned in nlen
    nlen  = len_trim(string)
    inull = scan(string, c_null_char)

    cstring = repeat(" ", len(cstring)) ! init to blanks

    if (inull > 0)  then ! string has a null char
       nlen = inull
       cstring = string(1:nlen)
    else ! append null char to trimmed string
       cstring = string(1:nlen)//c_null_char
       nlen = nlen + 1
    endif
  end function addcnullchar

  !> Open a GRIB2 file.
  !>
  !> @param path Path to file.
  !> @param mode Open mode, may be NC_CLOBBER (0) or NC_NOCLOBBER.
  !> @param g2cid File ID.
  !>
  !> @return
  !> - 0 No error
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function g2cf_open(path, mode, g2cid) result (status)
    use iso_c_binding
    implicit none
      
    character(len=*), intent(in) :: path
    integer, intent(in) :: mode
    integer, intent(inout) :: g2cid
    integer :: status
    integer :: g2c_open
    
    integer(c_int) :: cmode, cg2cid, cstatus
    character(len = (len(path) + 1)) :: cpath
    integer :: ie
    
    cmode = mode
    cg2cid = 0
    
    ! check for c null character on path and add one if not present.
    cpath = addcnullchar(path, ie)
    
    ! call nc_create to create file
    cstatus = g2c_open(cpath(1:ie), cmode, cg2cid)
    
    if (cstatus == 0) then
       g2cid   = cg2cid
    endif
    status = cstatus
    
  end function g2cf_open
end module g2cf
