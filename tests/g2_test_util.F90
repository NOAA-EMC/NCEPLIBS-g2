! This is part of the test code for the NCEPLIBS-g2 project.
!
! This file provides subroutines that create a cgrib variable for other tests
! or writes a grib2 file for use in other tests.
!
! Brian Curtis 11/12/21
! Ed Hartnett

! Create a GIRB2 message.
!
! Brian Curtis
subroutine create_cgrib(cgrib, lengrib)
  implicit none

  ! Storage for the GRIB2 message we are constructing.
  integer, parameter :: lcgrib = 191
  character, intent(out), dimension(lcgrib) :: cgrib

  ! Section 0 and 1.
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_table0-0.shtml.
  integer :: listsec0(2) = (/ 0, 2 /)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect1.shtml.
  integer :: listsec1(13) = (/ 7, 4, 2, 24, 0, 2021, 11, 13, 15, 59, 59, 1, 0 /)

  ! Section 2.
  integer, parameter :: lcsec2 = 3
  character :: csec2(lcsec2) = (/ achar(1), achar(2), achar(3) /)

  ! Section 3.
  integer, parameter :: igdstmplen = 19
  integer, parameter :: idefnum = 0
  integer, parameter :: ndata = 4
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_sect3.shtml
  integer :: igds(5) = (/ 0, ndata, 0, 0, 0/)
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp3-0.shtml
  integer :: igdstmpl(igdstmplen) = (/ 0, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0, 45, 91, 0, 55, 101, 5, 5, 0 /)
  integer :: ideflist(idefnum)

  ! Sections 4-7.
  integer :: ipdsnum = 0
  integer, parameter :: ipdstmplen = 15, numcoord = 0
  ! See https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/grib2_temp4-0.shtml
  integer :: ipdstmpl(ipdstmplen) = (/ 0, 0, 0, 0, 0, 12, 59, 0, 0, 1, 1, 1, 2, 1, 1 /)
  integer :: coordlist(1)
  integer :: idrsnum = 0
  integer, parameter :: idrstmplen = 5
  integer :: idrstmpl(idrstmplen) = (/ 0, 1, 1, 8, 0 /)
  integer, parameter :: ngrdpts = 4, ibmap = 255
  logical :: bmap(1) = .false.
#ifdef KIND_4  
  real(4) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)
#else
  real(8) :: fld(ngrdpts) = (/ 1.1, 1.2, 1.3, 1.4 /)
#endif

  ! Section 8
  integer, intent(out) :: lengrib
  integer :: ierr

  ! Create the GRIB2 message, with sections 0 and 1.
  call gribcreate(cgrib, lcgrib, listsec0, listsec1, ierr)
  if (ierr .ne. 0) stop 20
  print *, 'fld(1) ', fld(1)

  ! Add a local section.
  call addlocal(cgrib, lcgrib, csec2, lcsec2, ierr)
  if (ierr .ne. 0) stop 40
  print *, 'fld(1) ', fld(1)

  ! Add a grid section.
  call addgrid(cgrib, lcgrib, igds, igdstmpl, igdstmplen, &
       ideflist, idefnum, ierr)
  if (ierr .ne. 0) stop 80
  print *, 'fld(1) ', fld(1)

  ! Add a field.
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, &
       & coordlist, numcoord, idrsnum, idrstmpl, idrstmplen, fld, &
       & ngrdpts, ibmap, bmap, ierr)
  if (ierr .ne. 0) stop 140
  print *, 'fld(1) ', fld(1)

  ! End the grib message by adding section 8.w
  call gribend(cgrib, lcgrib, lengrib, ierr)
  if (ierr .ne. 0) stop 150

  return
end subroutine create_cgrib

! Write a GRIB2 message to a file.
!
! Brian Curtis
subroutine write_grib2_file(filename)
  use bacio_module
  implicit none

  character(*), intent(in) :: filename
  integer :: lengrib
  character :: cgrib(200)
  integer :: ierr

  ! Create a GRIB2 message.
  call create_cgrib(cgrib, lengrib)

  ! Open the file for writing.
  call baopenw(1, filename, ierr)

  ! Write the GRIB2 message.
  call wryte(1, lengrib, cgrib)

  ! Close the file.
  call baclose(1, ierr)

end subroutine write_grib2_file

! Pull the values out of an index record.
!
! Edward Hartnett, 5/11/24
subroutine read_index(cbuf, idxver, index_rec_len, b2s_message8, b2s_lus8, &
     b2s_gds8, b2s_pds8, b2s_drs8, b2s_bms8, b2s_data8, total_bytes8, &
     grib_version, discipline, field_number, sec1, lengds, gds, lenpds, pds, &
     lendrs, drs, bms, iret)
  implicit none

  character(len=1), pointer, dimension(:), intent(in) :: cbuf(:)
  integer, intent(in) :: idxver
  integer, intent(out) :: index_rec_len
  integer (kind = 8), intent(out) :: b2s_message8, b2s_lus8, b2s_gds8, b2s_pds8, b2s_drs8, b2s_bms8, b2s_data8
  integer (kind = 8), intent(out) :: total_bytes8
  integer, intent(out) :: grib_version, discipline, field_number
  character, intent(out) :: sec1(21)
  integer, intent(inout) :: lengds
  character, intent(out) :: gds(:)
  integer, intent(inout) :: lenpds
  character, intent(out) :: pds(:)
  integer, intent(inout) :: lendrs
  character, intent(out) :: drs(:)
  character, intent(out) :: bms(:)
  integer, intent(out) :: iret
  
  integer :: lensec1

  integer :: b2s_message, b2s_lus, b2s_gds, b2s_pds, b2s_drs, b2s_bms, b2s_data
  integer :: inc, mypos = 0
  integer :: i
  integer :: INT1_BITS, INT2_BITS, INT4_BITS, INT8_BITS
  parameter(INT1_BITS = 8, INT2_BITS = 16, INT4_BITS = 32, INT8_BITS = 64)
  integer (kind = 8) :: INT8_BITS8
  parameter(INT8_BITS8 = 64_8)
  integer :: BMS_LEN
  parameter (BMS_LEN = 6)

  interface
     subroutine g2_gbytec1(in, siout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: siout
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec1
  end interface
  interface
     subroutine g2_gbytec(in, iout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer, intent(inout) :: iout(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec
  end interface
  interface
     subroutine g2_gbytesc(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer, intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc
  end interface
  interface
     subroutine g2_gbytec8(in, iout, iskip, nbits)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(inout) :: iout(*)
       integer, intent(in) :: iskip, nbits
     end subroutine g2_gbytec8
  end interface

  ! Get the index record len (4 byte int).
  call g2_gbytec1(cbuf, index_rec_len, 0, INT4_BITS)
  !print *, 'read_index(): index_rec_len', index_rec_len
  mypos = INT4_BITS

  if (idxver .eq. 1) then
     inc = 0
     call g2_gbytec1(cbuf, b2s_message, mypos, INT4_BITS)
     !print '(i3, a12, z4)', mypos/8, ' b2s_message', b2s_message
     mypos = mypos + INT4_BITS
     b2s_message8 = b2s_message
     call g2_gbytec1(cbuf, b2s_lus, mypos, INT4_BITS)
     !print '(i3, a8, z4)', mypos/8, ' b2s_lus', b2s_lus
     mypos = mypos + INT4_BITS
     b2s_lus8 = b2s_lus
     call g2_gbytec1(cbuf, b2s_gds, mypos, INT4_BITS)
     !print '(i3, a8, z4)', mypos/8, ' b2s_gds', b2s_gds
     mypos = mypos + INT4_BITS
     b2s_gds8 = b2s_gds
  else
     inc = 12
     call g2_gbytec81(cbuf, b2s_message8, 8 * 4, INT8_BITS)
     mypos = mypos + INT8_BITS
     call g2_gbytec81(cbuf, b2s_lus8, 8 * 12, INT8_BITS)
     mypos = mypos + INT8_BITS
     call g2_gbytec81(cbuf, b2s_gds8, 8 * 20, INT8_BITS)
     mypos = mypos + INT8_BITS
     ! call g2_gbytec(cbuf, b2s_pds, 8 * 16, INT8_BITS)
  endif
  call g2_gbytec1(cbuf, b2s_pds, mypos, INT4_BITS)
  mypos = mypos + INT4_BITS
  b2s_pds8 = b2s_pds
  call g2_gbytec1(cbuf, b2s_drs, mypos, INT4_BITS)
  mypos = mypos + INT4_BITS  
  b2s_drs8 = b2s_drs
  call g2_gbytec1(cbuf, b2s_bms, mypos, INT4_BITS)
  mypos = mypos + INT4_BITS  
  b2s_bms8 = b2s_bms
  call g2_gbytec1(cbuf, b2s_data, mypos, INT4_BITS)
  mypos = mypos + INT4_BITS  
  b2s_data8 = b2s_data
  call g2_gbytec81(cbuf, total_bytes8, mypos, INT8_BITS)
  mypos = mypos + INT8_BITS
  call g2_gbytec1(cbuf, grib_version, mypos, INT1_BITS)
  mypos = mypos + INT1_BITS  
  call g2_gbytec1(cbuf, discipline, mypos, INT1_BITS)
  mypos = mypos + INT1_BITS  
  call g2_gbytec1(cbuf, field_number, mypos, INT2_BITS)
  mypos = mypos + INT2_BITS

  ! Find the length of sec1. It should be 21.
  call g2_gbytec1(cbuf, lensec1, mypos, INT4_BITS)
  !mypos = mypos + INT4_BITS
  
  ! Copy section 1 from the index record to output parameter. (mypos
  ! is in bits, but i is in bytes.)
  !print *, 'copying sec1', mypos/8
  do i = 1, lensec1
     sec1(i) = cbuf(mypos/8 + 1)
     mypos = mypos + INT1_BITS
  end do

  ! Find the length of gds. It should be 72.
  call g2_gbytec1(cbuf, lengds, mypos, INT4_BITS)
  
  ! Copy GDS from the index record to output parameter. (mypos
  ! is in bits, but i is in bytes.)
  !print *, 'copying gds', lengds, mypos/8
  do i = 1, lengds
     gds(i) = cbuf(mypos/8 + 1)
     mypos = mypos + INT1_BITS
  end do

  ! Find the length of pds. It should be 72.
  call g2_gbytec1(cbuf, lenpds, mypos, INT4_BITS)
  
  ! Copy PDS from the index record to output parameter. (mypos
  ! is in bits, but i is in bytes.)
  !print *, 'copying pds', lenpds, mypos/8
  do i = 1, lenpds
     pds(i) = cbuf(mypos/8 + 1)
     !print *, ichar(pds(i)), ','
     mypos = mypos + INT1_BITS
  end do

  ! Find the length of drs. It should be 72.
  call g2_gbytec1(cbuf, lendrs, mypos, INT4_BITS)
  
  ! Copy DRS from the index record to output parameter. (mypos
  ! is in bits, but i is in bytes.)
  !print *, 'copying drs', lendrs, mypos/8
  do i = 1, lendrs
     drs(i) = cbuf(mypos/8 + 1)
     !print *, ichar(drs(i)), ','
     mypos = mypos + INT1_BITS
  end do

  ! Copy the 6 bytes of bms from the index record to output
  ! parameter. (mypos is in bits, but i is in bytes.)
  !print *, 'copying bms', mypos/8
  do i = 1, BMS_LEN
     bms(i) = cbuf(mypos/8 + 1)
     !print *, ichar(bms(i)), ','
     mypos = mypos + INT1_BITS
  end do

  ! Return success.
  iret = 0
end subroutine read_index

