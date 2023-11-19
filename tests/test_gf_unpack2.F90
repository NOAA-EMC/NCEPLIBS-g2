! This is a test program for NCEPLIBS-g2.
!
! This program tests the gf_unpack2.f file.
!
! Andrew King 6/23/23
program test_gf_unpack2
  implicit none

  character, dimension(269) :: fgrib
  integer :: n1, n2, n3, fgrib_len
  integer :: iofst, lencsec2, ierr
  integer :: numlocal, numfields
  integer :: listsec0(3), listsec1(13), maxvals(7)
  character(len=1),pointer,dimension(:) :: csec2

  interface
     subroutine gf_unpack2(cgrib, lcgrib, iofst, lencsec2, csec2, ierr)
       character(len = 1), intent(in) :: cgrib(lcgrib)
       integer, intent(in) :: lcgrib
       integer, intent(inout) :: iofst
       integer, intent(out) :: lencsec2
       integer, intent(out) :: ierr
       character(len = 1), pointer, dimension(:) :: csec2
     end subroutine gf_unpack2
  end interface

  ! Large numbers in message initials
  n1 = 229
  n2 = 254
  n3 = 255

  ! The grib message for a typical grib2 file in fortran.
  fgrib(:)=(/                                                    &
                                ! section 0
       & "G", "R", "I", "B", char(0), char(0),                        &
       & char(0), char(2), char(0), char(0), char(0), char(0),    &
       & char(0), char(0), char(1), char(13),                       &
                                ! section 1
       & char(0), char(0), char(0), char(21), char(1), char(0),   &
       & char(8), char(0), char(0), char(1), char(0), char(1),    &
       & char(7), char(n1), char(11), char(15), char(10),          &
       & char(10), char(10), char(0), char(1),                      &
                                ! section 2
       & char(0), char(0), char(0), char(11), char(2),             &
       & char(1), "H", "K", "T", "A", "R",                             &
                                ! section 3
       & char(0), char(0), char(0), char(81), char(3), char(0),   &
       & char(0), char(0), char(0), char(10), char(0), char(0),  &
       & char(0), char(30), char(1), char(0), char(10), char(10), &
       & char(10), char(10), char(0), char(0), char(0), char(0),  &
       & char(0), char(0), char(0), char(0), char(0), char(0),    &
       & char(2), char(2), char(2), char(2), char(3),         &
       & char(3), char(3), char(3), char(4), char(4),         &
       & char(4), char(4),char(5),char(5), char(5),           &
       & char(5), char(0), char(6), char(6), char(6),          &
       & char(6), char(7), char(7), char(7), char(7),         &
       & char(8), char(8), char(8), char(8), char(8),         &
       & char(8), char(8), char(8), char(0), char(0),          &
       & char(6), char(6), char(6), char(6), char(6),         &
       & char(6), char(6), char(6), char(9), char(9),         &
       & char(9), char(9), char(0), char(0), char(0), char(0),  &
                                ! section 4
       & char(0), char(0), char(0), char(71), char(4), char(0),   &
       & char(0), char(0), char(9), char(1), char(8), char(2),    &
       & char(0), char(0), char(0), char(0), char(0), char(0),    &
       & char(0), char(3), char(6), char(9), char(1), char(0),    &
       & char(0), char(0), char(0), char(0), char(0), char(0),    &
       & char(0), char(0), char(0),  char(0), char(0), char(0),   &
       & char(1), char(0), char(0), char(0), char(0), char(0),    &
       & char(3), char(0), char(0), char(0), char(n2), char(7),   & 
       & char(n1), char(11), char(15), char(12), char(20),         &
       & char(10), char(1), char(0), char(0), char(0), char(0),   &
       & char(1), char(0), char(1), char(0), char(0), char(0),    &
       & char(12), char(1), char(0), char(0), char(0), char(0),   &
                                ! section 5
       & char(0), char(0), char(0), char(47), char(5),  char(0),  &
       & char(0), char(0), char(100), char(0), char(2),  char(0), &
       & char(0), char(0), char(0), char(0), char(0),  char(0),   &
       & char(1), char(8), char(1), char(1), char(1), char(0),  &
       & char(0), char(0), char(0), char(0), char(0),        &
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

  print *, ''//NEW_LINE('A')//'Calling grib_info ...'
  call gribinfo(fgrib, fgrib_len, listsec0, listsec1,  &
       numlocal, numfields, maxvals, ierr)
  print *, 'Message length: ', listsec0(3)
  print *, 'Section 2 occurence: ', numlocal
  print *, 'Section 2 length: ', maxvals(1)

  print *, ''//NEW_LINE('A')//'Testging gf_unpack2 ...'

  ! Normal test, should return no error and allocate csec2 Start byte
  ! = 37 => start bit = 296 Length of section (in octets) is 6.
  iofst = 296
  call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
  if (ierr .ne. 0 .or. iofst .ne. 384 .or. lencsec2 .ne. 6) stop 1
  deallocate(csec2)

  ! Offset not to section 2 data, should error without allocating
  ! csec2.
  iofst = 1
  call gf_unpack2(fgrib, fgrib_len, iofst, lencsec2, csec2, ierr)
  if (ierr .ne. 6) stop 2

  print *, ''//NEW_LINE('A')//'SUCCESS!'
end program test_gf_unpack2
