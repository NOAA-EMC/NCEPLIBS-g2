! This program tests the complex packing and unpacking subroutines of
! the NCEPLIBS-g2 project. Link this to the _4 build of the library.
!
! Brian Curtis 11-12-2021
program test_cmplxpack
    implicit none
  
    integer, parameter :: ndpts = 4
    real(kind=8) :: fld_orig(ndpts), fld(ndpts), fld_ina(ndpts), fld_inb(ndpts)
    integer :: idrstmpl(17)
    integer :: idrsnum
    character*1, dimension(50) :: cpack
    integer :: lcpack, lensec
    integer :: i, ierr
  
    print *, 'Testing cmplxack.'
    lensec = ndpts
  
    print *, "Shape of fld_orig", shape(cmplx(22, 4))
    print *, 'Testing simple call to compack...'
!    fld_orig = (/cmplx(42, 3), cmplx(43, 2), cmplx(44, 1), cmplx(99, 0)/)
    fld_orig = (/ 42.0, 43.0, 44.0, 99.0 /)
    fld = fld_orig
    ! idrstmpl = (/42, 1, 1, 0, 0, 0/)
    idrstmpl(1) = 0
    idrstmpl(2) = 0
    idrstmpl(3) = 0
    idrstmpl(4) = 32
    idrstmpl(7) = 0
    idrstmpl(8) = 0
    idrstmpl(9) = 0
    
    idrsnum = 2
    call cmplxpack(fld, ndpts, idrsnum, idrstmpl, cpack, lcpack)
    print *, 'lcpack1: ', lcpack
  
    if (lcpack .ne. ndpts) stop 2
  
    print *, 'Testing simple call to comunpack...'
    call comunpack(cpack, lcpack, lensec, idrsnum, idrstmpl, ndpts, &
                   fld_ina ,ierr)
    if (ierr .ne. 0) then
        print *, 'ierr: ', ierr
        stop 22
    endif
    do i = 1, ndpts
        print *, 'fld_orig, fldin:', fld_orig(i), fld_ina(i)
      if (abs(fld_orig(i) - fld_ina(i)) .gt. .1) stop 10
    end do

    idrstmpl(7) = 2
    idrstmpl(8) = 99
    idrstmpl(17) = 1

    ! This calls misspack (missing values packing)
    call cmplxpack(fld, ndpts, idrsnum, idrstmpl, cpack, lcpack)
    print *, 'lcpack2: ', lcpack
  
    if (lcpack .ne. ndpts+1) stop 3

    print *, 'Testing simple call to comunpack...'
    call comunpack(cpack, lcpack, lensec, idrsnum, idrstmpl, ndpts, &
                   fld_inb ,ierr)
    if (ierr .ne. 0) then
        print *, 'ierr: ', ierr
        stop 23
    endif
    do i = 1, ndpts
        print *, 'fld_orig, fldin:', fld_orig(i), fld_inb(i)
      if (abs(fld_orig(i) - fld_inb(i)) .gt. .1) stop 11
    end do
  
    print *, 'SUCCESS!'
  
  end program test_cmplxpack
  
