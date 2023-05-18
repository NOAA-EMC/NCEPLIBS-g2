! This is part of the NCEPLIBS-g2 project.
!
! This is a module to help handle index records.
!
! Ed Hartnett 5/18/23
module index_rec
  implicit none
  
  type index_rec_data
     integer :: index_rec_len, b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
     integer :: total_bytes, grib_version, discipline, field_number
  end type index_rec_data

contains
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

  subroutine print_index(idx)
    implicit none
    type (index_rec_data), intent(in) :: idx

    print *, 'index_rec_len = ', idx%index_rec_len, ' b2s_message = ', idx%b2s_message
    print *, 'b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data: ', &
         idx%b2s_lus, idx%b2s_gds, idx%b2s_pds, idx%b2s_drs, idx%b2s_bms, idx%b2s_data
    print *, 'total_bytes, grib_version, discipline, field_number: ', &
         idx%total_bytes, idx%grib_version, idx%discipline, idx%field_number
  end subroutine print_index

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

  integer function cmp_idx(idx1, idx2)
    implicit none
    type (index_rec_data), intent(in) :: idx1, idx2

    cmp_idx = 0

    if (idx1%index_rec_len .ne. idx2%index_rec_len) cmp_idx = cmp_idx + 1
    if (idx1%index_rec_len .ne. idx2%index_rec_len) cmp_idx = cmp_idx + 1
    if (idx1%b2s_message .ne. idx2%b2s_message) cmp_idx = cmp_idx + 1
    if (idx1%b2s_lus .ne. idx2%b2s_lus) cmp_idx = cmp_idx + 1
    if (idx1%b2s_gds .ne. idx2%b2s_gds) cmp_idx = cmp_idx + 1
    if (idx1%b2s_pds .ne. idx2%b2s_pds) cmp_idx = cmp_idx + 1
    if (idx1%b2s_drs .ne. idx2%b2s_drs) cmp_idx = cmp_idx + 1
    if (idx1%b2s_bms .ne. idx2%b2s_bms) cmp_idx = cmp_idx + 1
    if (idx1%b2s_data .ne. idx2%b2s_data) cmp_idx = cmp_idx + 1
    if (idx1%total_bytes .ne. idx2%total_bytes) cmp_idx = cmp_idx + 1
    if (idx1%grib_version .ne. idx2%grib_version) cmp_idx = cmp_idx + 1
    if (idx1%discipline .ne. idx2%discipline) cmp_idx = cmp_idx + 1
    if (idx1%field_number .ne. idx2%field_number) cmp_idx = cmp_idx + 1

  end function cmp_idx

end module index_rec
