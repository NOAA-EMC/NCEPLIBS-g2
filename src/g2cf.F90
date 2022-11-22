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
    
    integer, value :: log_level
    integer :: status
    integer(c_int) :: clog_level, cstatus
    
    clog_level = log_level
    cstatus = g2c_set_log_level(clog_level)
    status = cstatus
  end function g2cf_set_log_level

  !> Inquire how many messages in a GRIB2 file.
  !>
  !> @param[in] g2cid ID of open GRIB2 file.
  !> @param[out] num_msg Number of GRIB2 messages in the file.
  !>
  !> @return
  !> - 0 No error
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function g2cf_inq(g2cid, num_msg) result (status)
    use iso_c_binding
    implicit none
      
    interface
       function g2c_inq(g2cid, num_msg) bind(c)
         use iso_c_binding, only: c_int
         integer(c_int), value :: g2cid
         integer(c_int), intent(out) :: num_msg
         integer(c_int) :: g2c_inq
       end function g2c_inq
    end interface
    
    integer, value :: g2cid
    integer, intent(out) :: num_msg
    integer :: status
    integer(c_int) :: cg2cid, cnum_msg, cstatus
    
    cg2cid = g2cid
    cstatus = g2c_inq(cg2cid, cnum_msg)
    if (cstatus .eq. 0) num_msg = cnum_msg
    status = cstatus
  end function g2cf_inq

  !> Inquire about a messages in a GRIB2 file.
  !>
  !> @param[in] g2cid ID of the opened file, as from g2cf_open().
  !> @param[in] msg_num Number of the message in the file, starting with the
  !> first message as 1.
  !>
  !> @param[out] discipline Gets the discipline from the message.
  !> @param[out] num_fields Gets the number of fields in the message.
  !> @param[out] num_local Gets the number of local sections in the
  !> message.
  !> @param[out] center Gets the code for the producing center from
  !> the message.
  !> @param[out] subcenter Gets the code for the producing subcenter
  !> from the message.
  !> @param[out] master_version Gets the master version from the
  !> message.
  !> @param[out] local_version Gets the local version from the
  !> message.
  !>
  !> @return
  !> - 0 No error
  !>
  !> @author Ed Hartnett @date 2022-11-21
  function g2cf_inq_msg(g2cid, msg_num, discipline, num_fields, num_local, center, &
       subcenter, master_version, local_version) result (status)
    use iso_c_binding
    implicit none
      
    interface
       function g2c_inq_msg(g2cid, msg_num, discipline, num_fields, num_local, center, &
       subcenter, master_version, local_version) bind(c)
         use iso_c_binding
         integer(c_int), value :: g2cid
         integer(c_int), value :: msg_num
         integer(c_signed_char), intent(out) :: discipline
         integer(c_int), intent(out) :: num_fields, num_local
         integer(c_short), intent(out) :: center, subcenter
         integer(c_signed_char), intent(out) :: master_version, local_version
         integer(c_int) :: g2c_inq_msg
       end function g2c_inq_msg
    end interface
    
    integer, value :: g2cid
    integer, value :: msg_num    
    integer(kind = 1), intent(out) :: discipline
    integer(kind = 4), intent(out) :: num_fields, num_local
    integer(kind = 2), intent(out) :: center, subcenter
    integer(kind = 1), intent(out) :: master_version, local_version
    integer :: status
    integer(c_int) :: cstatus, cmsg_num
    integer(c_signed_char) :: cdiscipline
    integer(c_int) :: cg2cid, cnum_fields, cnum_local
    integer(c_short) :: ccenter, csubcenter
    integer(c_signed_char) :: cmaster_version, clocal_version
    
    cg2cid = g2cid
    cmsg_num = msg_num - 1
    cstatus = g2c_inq_msg(cg2cid, cmsg_num, cdiscipline, cnum_fields, cnum_local, ccenter, &
         csubcenter, cmaster_version, clocal_version)
    if (cstatus .eq. 0) then
       discipline = cdiscipline
       num_fields = cnum_fields
       num_local = cnum_local
       center = ccenter
       subcenter = csubcenter
       master_version = cmaster_version
       local_version = clocal_version
    endif
    status = cstatus
  end function g2cf_inq_msg
end module g2cf
