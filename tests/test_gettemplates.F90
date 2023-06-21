! This is a test program for NCEPLIBS-g2.
!
! This program tests the gettemplates function.
!
! Andrew King 6/20/23
program test_gettemplates
    implicit none

    ! Grib message from g2_decode
    fgrib(:)=(/                                                 &
       ! section 0
     & "G", "R", "I", "B", char(0), char(0),                    &
     & char(0), char(2), char(0), char(0), char(0), char(0),    &
     & char(0), char(0), char(1), char(13),                     &
       ! section 1
     & char(0), char(0), char(0), char(21), char(1), char(0),   &
     & char(8), char(0), char(0), char(1), char(0), char(1),    &
     & char(7), char(n1), char(11), char(15), char(10),         &
     & char(10), char(10), char(0), char(1),                    &
       ! section 2
     & char(0), char(0), char(0), char(11), char(2),            &
     & char(1), "H", "K", "T", "A", "R",                        &
       ! section 3
     & char(0), char(0), char(0), char(81), char(3), char(0),   &
     & char(0), char(0), char(0), char(k10), char(0), char(0),  &
     & char(0), char(30), char(1), char(0), char(k1), char(k1), &
     & char(k1), char(k1), char(0), char(0), char(0), char(0),  &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
     & char(k2), char(k2), char(k2), char(k2), char(k3),        &
     & char(k3), char(k3), char(k3), char(k4), char(k4),        &
     & char(k4), char(k4),char(k5),char(k5), char(k5),          &
     & char(k5), char(0), char(k6), char(k6), char(k6),         &
     & char(k6), char(k7), char(k7), char(k7), char(k7),        &
     & char(k8), char(k8), char(k8), char(k8), char(k8),        &
     & char(k8), char(k8), char(k8), char(0), char(80),         &
     & char(k6), char(k6), char(k6), char(k6), char(k6),        &
     & char(k6), char(k6), char(k6), char(k9), char(k9),        &
     & char(k9), char(k9), char(0), char(0), char(0), char(0),  &
       ! section 4
     & char(0), char(0), char(0), char(71), char(4), char(0),   &
     & char(0), char(0), char(9), char(1), char(8), char(2),    &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
     & char(0), char(3), char(6), char(9), char(1), char(0),    &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
     & char(0), char(0), char(0),  char(0), char(0), char(0),   &
     & char(1), char(0), char(0), char(0), char(0), char(0),    &
     & char(3), char(0), char(0), char(0), char(n2), char(7),   & 
     & char(n1), char(11), char(15), char(12), char(20),        &
     & char(10), char(1), char(0), char(0), char(0), char(0),   &
     & char(1), char(0), char(1), char(0), char(0), char(0),    &
     & char(12), char(1), char(0), char(0), char(0), char(0),   &
       ! section 5
     & char(0), char(0), char(0), char(47), char(5),  char(0),  &
     & char(0), char(0), char(k10), char(0), char(2),  char(0), &
     & char(0), char(0), char(0), char(0), char(0),  char(0),   &
     & char(1), char(8), char(1), char(1), char(1), char(Nan),  &
     & char(Nan), char(Nan), char(Nan), char(0), char(0),       &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
     & char(0), char(0), char(0), char(0), char(0), char(0),    &
       ! section 6
     & char(0), char(0), char(0), char(6), char(6),  char(n3),  &
       ! section 7
     & char(0), char(0), char(0), char(12), char(7),  char(1),  &
     & char(0), char(0), char(0), char(0), char(0),  char(0),   &
       ! section 8
     & "7", "7", "7", "7" /) 
  
    print *, 'SUCCESS!'
  end program test_gettemplates
