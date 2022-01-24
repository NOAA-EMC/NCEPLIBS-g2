!> @file
!> @brief This subroutine frees up memory in derived type gribfield.
!> @author Stephen Gilbert @date 2000-05-26

!> This subroutine frees up memory that was used to store
!> array values in derived type gribfield.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2000-05-26 | Stephen Gilbert | Modified from getg1s to work with grib2
!> 2012-12-11 | Boi Vuong | initialize an undefine pointers
!> 2015-10-29 | Boi Vuong | Deallocate pointers in derived type gribfield
!>
!> @param gfld derived type @ref grib_mod::gribfield.
!>
!> @author Stephen Gilbert @date 2000-05-26
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
end subroutine gf_free
