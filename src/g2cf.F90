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
    
    interface
       function g2c_open(path, mode, g2cid) bind(c)
         use iso_c_binding, only: c_char, c_int
         character(kind=c_char), intent(in)  :: path(*)
         integer(c_int), value :: mode
         integer(c_int), intent(out) :: g2cid
         integer(c_int) :: g2c_open
       end function g2c_open
    end interface

    character(len=*), intent(in) :: path
    integer, intent(in) :: mode
    integer, intent(inout) :: g2cid
    integer :: status
    
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

  !> Close a GRIB2 file.
  !>
  !> @param g2cid File ID.
  !>
  !> @return
  !> - 0 No error
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function g2cf_close(g2cid) result (status)
    use iso_c_binding
    implicit none
      
    interface
       function g2c_close(g2cid) bind(c)
         use iso_c_binding, only: c_int
         integer(c_int), value :: g2cid
         integer(c_int) :: g2c_close
       end function g2c_close
    end interface
    
    integer, intent(in) :: g2cid
    integer :: status
    integer(c_int) :: cg2cid, cstatus
    
    cg2cid = g2cid
    cstatus = g2c_close(cg2cid)
    status = cstatus
  end function g2cf_close

  !> Turn on logging.
  !>
  !> @param log_level Logging level.
  !>
  !> @return
  !> - 0 No error
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function g2cf_set_log_level(log_level) result (status)
    use iso_c_binding
    implicit none
      
    interface
       function g2c_set_log_level(log_level) bind(c)
         use iso_c_binding, only: c_int
         integer(c_int), value :: log_level
         integer(c_int) :: g2c_set_log_level
       end function g2c_set_log_level
    end interface
    
    integer, intent(in) :: log_level
    integer :: status
    integer(c_int) :: clog_level, cstatus
    
    clog_level = log_level
    cstatus = g2c_set_log_level(clog_level)
    status = cstatus
  end function g2cf_set_log_level
end module g2cf
