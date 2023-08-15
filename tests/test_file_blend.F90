! This is a test program for NCEPLIBS-g2.
!
! This program tests the contents of a file downloaded from the FTP
! site, blend.t19z.core.f001.co.grib2.
!
! Ed Hartnett 8/11/23
program test_file_blend
  use grib_mod
  implicit none

  character*(*) :: FILE_NAME
  parameter (FILE_NAME = 'data/blend.t19z.core.f001.co.grib2')
  integer :: LUGI
  parameter (LUGI = 3)
  character(len = 1), pointer, dimension(:) :: cbuf
  integer :: numfld, mlen, lgrib
  integer :: msk1, msk2, lskip
  parameter(msk1 = 32000, msk2 = 4000)  
  integer :: iseek
  character(len = 1), allocatable, dimension(:) :: cgrib  
  integer :: iret

  print *, 'Testing reading file ', FILE_NAME

  ! Open the file.
  call baopenr(LUGI, FILE_NAME, iret)
  if (iret .ne. 0) stop 3

  ! Use ixgb2 to generate an index record.
  lgrib = 2000
  call ixgb2(LUGI, 0, lgrib, cbuf, numfld, mlen, iret)
!  print *,iret, numfld, mlen
  if (iret .ne. 0) stop 101
  if (numfld .ne. 1 .or. mlen .ne. 235) stop 102
  deallocate(cbuf)

  ! Find a GRIB2 message in the file.
  ! iseek = 0
  ! call skgb(LUGI, iseek, msk1, lskip, lgrib)
  ! print *, lgrib
  ! if (lgrib .eq. 0) stop 110
  
  ! ! Read the GRIB2 message from the file.
  ! allocate(cgrib(lgrib))
  ! call baread(LUGI, lskip, lgrib, lengrib, cgrib)
  ! if (lgrib .ne. lengrib) stop 111
  ! iseek = lskip + lgrib
  ! icount = icount + 1
  ! print *,' GRIB MESSAGE  ', icount, '  starts at ', lskip + 1

     ! Get info about the message.
     ! call gb_info(cgrib, lengrib, listsec0, listsec1,  &
     !      numfields, numlocal, maxlocal, ierr)
     ! if (ierr .ne. 0) then
     !    write(6, '(A,I0)') ' ERROR extracting field = ', ierr
     !    stop 10
     ! endif
     ! itot = itot + numfields
     ! write(6, '(A,3(1x,I0))')'  SECTION 0: ', (listsec0(j), j = 1, 3)
     ! write(6, '(A,13(1x,I0))')'  SECTION 1: ', (listsec1(j), j = 1, 13)
     ! write(6, '(A,1x,I0,1x,A,I0,1x,A)') '  Contains ', numlocal,  &
     !      ' Local Sections  and  ', numfields, ' data fields.'


  ! Close the file.
  call baclose(lugi, iret)
  if (iret .ne. 0) stop 5
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_file_blend
