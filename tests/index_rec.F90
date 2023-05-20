! This is part of the NCEPLIBS-g2 project.
!
! This is a module to help handle index records.
!
! Ed Hartnett 5/18/23
module index_rec
  use grib_mod
  implicit none
  
  type index_rec_data
     integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
     integer :: total_bytes, grib_version, discipline, field_number
  end type index_rec_data

contains
  ! Initialize a gribmod.
  subroutine init_gribmod(version, idsectlen, idsect, locallen, gfld)
    use grib_mod
    implicit none
    integer, intent(in) :: version    
    integer, intent(in) :: idsectlen    
    integer, intent(in) :: idsect(:)    
    integer, intent(in) :: locallen    
    type(gribfield), intent(inout) :: gfld
    integer :: i
    
    gfld%version = version
    gfld%idsectlen = idsectlen
    allocate(gfld%idsect(idsectlen))
    do i = 1, idsectlen
       gfld%idsect(i) = idsect(i)
    end do
    gfld%locallen = locallen
    
  end subroutine init_gribmod

  ! Compare two gribmods, return 0 if they are equal.
  integer function cmp_gribmod(gfld1, gfld2)
    use grib_mod
    implicit none
    type(gribfield), intent(in) :: gfld1, gfld2
    integer :: dc ! difference count
    integer :: i

    dc = 0
    if (gfld1%version .ne. gfld2%version) dc = dc + 1
    if (gfld1%idsectlen .ne. gfld2%idsectlen) dc = dc + 1
    do i = 1, gfld1%idsectlen
       if (gfld1%idsect(i) .ne. gfld2%idsect(i)) dc = dc + 1
    end do
!    if (gfld1%locallen .ne. gfld2%locallen) dc = dc + 1
    
    ! Return 0 for no differences.
    cmp_gribmod = dc
  end function cmp_gribmod

  ! Print contents of a gribmod.
  subroutine print_gribmod(gfld)
    implicit none
    type(gribfield), intent(in) :: gfld
    
    print *, 'version ', gfld%version
    print *, 'idsect ', gfld%idsect
    print *, 'idsectlen ', gfld%idsectlen
    print *, 'locallen ', gfld%locallen
    print *, 'ifldnum ', gfld%ifldnum
    print *, 'griddef ', gfld%griddef
    print *, 'ngrdpts ', gfld%ngrdpts
    print *, 'numoct_opt ', gfld%numoct_opt
    print *, 'interp_opt ', gfld%interp_opt
    print *, 'num_opt ', gfld%num_opt
!    print *, 'list_opt ', gfld%list_opt
    print *, 'igdtnum ', gfld%igdtnum
    print *, 'igdtlen ', gfld%igdtlen
!    print *, 'igdtmpl ', gfld%igdtmpl
    print *, 'ipdtnum ', gfld%ipdtnum
    print *, 'ipdtlen ', gfld%ipdtlen
!    print *, 'ipdtmpl ', gfld%ipdtmpl
    print *, 'num_coord ', gfld%num_coord
!    print *, 'coord_list ', gfld%coord_list
    print *, 'ndpts ', gfld%ndpts
    print *, 'idrtnum ', gfld%idrtnum
    print *, 'idrtlen ', gfld%idrtlen
!    print *, 'idrtmpl ', gfld%idrtmpl
    print *, 'unpacked ', gfld%unpacked
    print *, 'expanded ', gfld%expanded
    print *, 'ibmap ', gfld%ibmap
  end subroutine print_gribmod

  ! Initialize an index record.
  subroutine init_index(index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, &
       b2s_data, total_bytes, grib_version, discipline, field_number, idx)
    implicit none
    integer, intent(in) :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
    integer, intent(in) :: total_bytes, grib_version, discipline, field_number
    type (index_rec_data), intent(inout) :: idx

    idx%index_rec_len = index_rec_len
    idx%b2s_message = b2s_message
    idx%b2s_lus = b2s_lus
    idx%b2s_gds = b2s_gds
    idx%b2s_pds = b2s_pds
    idx%b2s_drs = b2s_drs
    idx%b2s_bms = b2s_bms
    idx%b2s_data = b2s_data
    idx%total_bytes = total_bytes
    idx%grib_version = grib_version
    idx%discipline = discipline
    idx%field_number = field_number
    
  end subroutine init_index

  ! Print an index record.
  subroutine print_index(idx)
    implicit none
    type (index_rec_data), intent(in) :: idx

    print *, 'index_rec_len = ', idx%index_rec_len, ' b2s_message = ', idx%b2s_message
    print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', &
         idx%b2s_lus, idx%b2s_gds, idx%b2s_pds, idx%b2s_drs, idx%b2s_bms, idx%b2s_data
    print *, 'total_bytes, grib_version, discipline, field_number: ', &
         idx%total_bytes, idx%grib_version, idx%discipline, idx%field_number
  end subroutine print_index

  ! Parse buffer with index file contents into index information.
  subroutine parse_cbuf(cbuf, idx)
    implicit none
    character(len=1), pointer, dimension(:), intent(in) :: cbuf(:)
    type (index_rec_data), intent(out) :: idx

    ! Break out the index record into component values.
    call g2_gbytec(cbuf, idx%index_rec_len, 0, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_message, 8 * 4, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_lus, 8 * 8, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_gds, 8 * 12, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_pds, 8 * 16, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_drs, 8 * 20, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_bms, 8 * 24, 8 * 4)
    call g2_gbytec(cbuf, idx%b2s_data, 8 * 28, 8 * 4)
    call g2_gbytec(cbuf, idx%total_bytes, 8 * 32, 8 * 8)
    call g2_gbytec(cbuf, idx%grib_version, 8 * 40, 8 * 1)
    call g2_gbytec(cbuf, idx%discipline, 8 * 41, 8 * 1)
    call g2_gbytec(cbuf, idx%field_number, 8 * 42, 8 * 2)

  end subroutine parse_cbuf
  
  ! Compare two indexes, return 0 if they are equal.
  integer function cmp_idx(idx1, idx2)
    implicit none
    type (index_rec_data), intent(in) :: idx1, idx2
    integer res;
    
    res = 0

    if (idx1%index_rec_len .ne. idx2%index_rec_len) res = res + 1
    if (idx1%index_rec_len .ne. idx2%index_rec_len) res = res + 1
    if (idx1%b2s_message .ne. idx2%b2s_message) res = res + 1
    if (idx1%b2s_lus .ne. idx2%b2s_lus) res = res + 1
    if (idx1%b2s_gds .ne. idx2%b2s_gds) res = res + 1
    if (idx1%b2s_pds .ne. idx2%b2s_pds) res = res + 1
    if (idx1%b2s_drs .ne. idx2%b2s_drs) res = res + 1
    if (idx1%b2s_bms .ne. idx2%b2s_bms) res = res + 1
    if (idx1%b2s_data .ne. idx2%b2s_data) res = res + 1
    if (idx1%total_bytes .ne. idx2%total_bytes) res = res + 1
    if (idx1%grib_version .ne. idx2%grib_version) res = res + 1
    if (idx1%discipline .ne. idx2%discipline) res = res + 1
    if (idx1%field_number .ne. idx2%field_number) res = res + 1

    ! Return result, 0 if there is no difference between idx1 and idx2.
    cmp_idx = res
  end function cmp_idx

end module index_rec
