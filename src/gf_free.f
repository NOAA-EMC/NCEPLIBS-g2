!> @file
!> @brief Frees memory in derived type grib_mod::gribfield.
!> @author Stephen Gilbert @date 2000-05-26

!> Free memory that was used to store
!> array values in derived type grib_mod::gribfield.
!>
!> @param gfld derived type grib_mod::gribfield.
!>
!> @author Stephen Gilbert @date 2000-05-26
      subroutine gf_free(gfld)

      use grib_mod
    
      type(gribfield) :: gfld

      if (associated(gfld%idsect)) then
         deallocate(gfld%idsect,stat=is)
      endif
      nullify(gfld%idsect)

      if (associated(gfld%local)) then
         deallocate(gfld%local,stat=is)
      endif
      nullify(gfld%local)

      if (associated(gfld%list_opt)) then
         deallocate(gfld%list_opt,stat=is)
      endif
      nullify(gfld%list_opt)

      if (associated(gfld%igdtmpl)) then
         deallocate(gfld%igdtmpl,stat=is)
      endif
      nullify(gfld%igdtmpl)

      if (associated(gfld%ipdtmpl)) then
         deallocate(gfld%ipdtmpl,stat=is)
      endif
      nullify(gfld%ipdtmpl)

      if (associated(gfld%coord_list)) then
         deallocate(gfld%coord_list,stat=is)
      endif
      nullify(gfld%coord_list)

      if (associated(gfld%idrtmpl)) then
         deallocate(gfld%idrtmpl,stat=is)
      endif
      nullify(gfld%idrtmpl)

      if (associated(gfld%bmap)) then
         deallocate(gfld%bmap,stat=is)
      endif
      nullify(gfld%bmap)

      if (associated(gfld%fld)) then
        deallocate(gfld%fld,stat=is)
      endif
      nullify(gfld%fld)

      return
      end
