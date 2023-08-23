! Test the mkieee and rdieee functions in NCEPLIBS-g2 written by ChatGPT using the prompt "write a fortran test program that tests an array of positive and negative values which includes a very large number and zero for the mkieee()/rdieee() functions of the NCEPLIBS-g2 library"
!
! Aidan Hartnett

!program ieee_array_large_conversion_test
!    #ifdef KIND_4
!    implicit none

    ! Declare constants
 !   integer, parameter :: n_values = 8

    ! Declare variables
!    real(kind=4) :: ibm_floats(n_values), ieee_floats(n_values)
!    integer :: ierr, i

    ! Initialize IBM floating-point values
!    ibm_floats = [1.0, -2.5, 3.14159, -0.123456, 10.0, 0.0, 1.0E38, -1.0E38]

    ! Convert IBM array to IEEE
!    do i = 1, n_values
!        call mkieee(ibm_floats(i), ieee_floats(i), ierr)
!    end do

    ! Display results
!    write(*,*) 'IBM Floats:'
!    do i = 1, n_values
!        write(*,*) ibm_floats(i)
!    end do
!    write(*,*)
!    write(*,*) 'IEEE Floats:'
!    do i = 1, n_values
!        write(*,*) ieee_floats(i)
!    end do
!    write(*,*)

    ! Convert IEEE array back to IBM
!    do i = 1, n_values
!        call rdieee(ieee_floats(i), ibm_floats(i), ierr)
!    end do

    ! Display results
!    write(*,*) 'Converted IBM Floats:'
!    do i = 1, n_values
!        write(*,*) ibm_floats(i)
!    end do
!    write(*,*)

    ! Verify conversion
!    write(*,*)
!    write(*,*) 'Verification:'
!    do i = 1, n_values
!        if (abs(ibm_floats(i) - ibm_floats(i)) < 1E-6) then
!            write(*,*) 'Value', i, '- Conversion successful!'
!        else
!            write(*,*) 'Value', i, '- Conversion failed!'
!        end if
!    end do
!    #endif

!end program ieee_array_large_conversion_test
