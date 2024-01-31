!> @file
!> @brief Reallocate memory, preserving contents.
!> @author Stephen Gilbert @date 2000-10-01

!> @brief Reallocate memory, preserving contents.
!>
!> This module contains two subroutines to reallocate integer, and
!> character arrays in memory, preseving the existing contents of the
!> array.
!>
!> @author Stephen Gilbert @date 2000-10-01
module re_alloc

  interface realloc
     module procedure realloc_c1
     module procedure realloc_r
     module procedure realloc_i
  end interface realloc

contains

  !> This subroutine reallocates a character array, preserving its
  !> contents.
  !>
  !> @param[inout] c pointer for data in memory.
  !> @param[in] n dimension for data in memory. This is how much data
  !> is currently stored in the array.
  !> @param[in] m dimension for allocatable array. This is the size
  !> the array will be after the allocation of memory.
  !> @param[out] istat scalar INTEGER variable for allocate.
  !> - 0 No error.
  !> - 10 Incorrect dimension.
  !> - other Allocation error.
  !>
  !> @author Stephen Gilbert @date 2000-10-01
  subroutine realloc_c1(c,n,m,istat)
    character(len=1),pointer,dimension(:) :: c
    integer,intent(in) :: n,m
    integer,intent(out) :: istat
    integer :: num
    character(len=1),pointer,dimension(:) :: tmp

    istat=0
    if ( (n<0) .OR. (m<=0) ) then
       istat=10
       return
    endif

    if ( .not. associated(c) ) then
       allocate(c(m),stat=istat)   ! allocate new memory
       return
    endif

    tmp=>c                      ! save pointer to original mem
    nullify(c)
    allocate(c(m),stat=istat)   ! allocate new memory
    if ( istat /= 0 ) then
       c=>tmp
       return
    endif
    if ( n /= 0 ) then
       num=min(n,m)
       c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
    endif
    deallocate(tmp)             ! deallocate original memory
    return
  end subroutine realloc_c1

  !> This subroutine reallocates an integer array, preserving its
  !> contents.
  !>
  !> @param[inout] c pointer for data in memory.
  !> @param[in] n dimension for data in memory. This is how much data
  !> is currently stored in the array.
  !> @param[in] m dimension for allocatable array. This is the size
  !> the array will be after the allocation of memory.
  !> @param[out] istat scalar INTEGER variable for allocate.
  !> - 0 No error.
  !> - 10 Incorrect dimension.
  !> - other Allocation error.
  !>
  !> @author Stephen Gilbert @date 2000-10-01
  subroutine realloc_r(c,n,m,istat)
    real,pointer,dimension(:) :: c
    integer,intent(in) :: n,m
    integer,intent(out) :: istat
    integer :: num
    real,pointer,dimension(:) :: tmp

    istat=0
    if ( (n<0) .OR. (m<=0) ) then
       istat=10
       return
    endif

    if ( .not. associated(c) ) then
       allocate(c(m),stat=istat)   ! allocate new memory
       return
    endif

    tmp=>c                      ! save pointer to original mem
    nullify(c)
    allocate(c(m),stat=istat)   ! allocate new memory
    if ( istat /= 0 ) then
       c=>tmp
       return
    endif
    if ( n /= 0 ) then
       num=min(n,m)
       c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
    endif
    deallocate(tmp)             ! deallocate original memory
    return
  end subroutine realloc_r

  !>    This subroutine reorganize integer type data in memory into
  !>    one one dimensional array.
  !>    @param[inout] c pointer for data in memory.
  !>    @param[in] n dimension for data in memory.
  !>    @param[in] m dimension for allocatable array.
  !>    @param[out] istat scalar INTEGER variable for allocate.
  !>    @author Stephen Gilbert @date 2000-10-01
  !>

  subroutine realloc_i(c,n,m,istat)
    integer,pointer,dimension(:) :: c
    integer,intent(in) :: n,m
    integer,intent(out) :: istat
    integer :: num
    integer,pointer,dimension(:) :: tmp

    istat=0
    if ( (n<0) .OR. (m<=0) ) then
       istat=10
       return
    endif

    if ( .not. associated(c) ) then
       allocate(c(m),stat=istat)   ! allocate new memory
       return
    endif

    tmp=>c                      ! save pointer to original mem
    nullify(c)
    allocate(c(m),stat=istat)   ! allocate new memory
    if ( istat /= 0 ) then
       c=>tmp
       return
    endif
    if ( n /= 0 ) then
       num=min(n,m)
       c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
    endif
    deallocate(tmp)             ! deallocate original memory
    return
  end subroutine realloc_i

end module re_alloc
