!> @file
!> @brief Extract or store arbitrary size values between packed bit
!> string and unpacked array.
!> @author Stephen Gilbert @date 2004-04-27

!> Extract one arbitrary size value (up to 32 bits) from a packed bit
!> string, right justifying each value in the unpacked array.
!>
!> This should be used when input array IN has only one element. If in
!> has more elements, use g2_sbytesc().
!>
!> @param[in] in Array input.
!> @param[inout] iout Unpacked array output.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in IN to take. Must
!> be 32 or less.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_gbytec(in, iout, iskip, nbits)
  implicit none

  character*1, intent(in) :: in(*)
  integer, intent(inout) :: iout(*)
  integer, intent(in) :: iskip, nbits
  call g2_gbytesc(in, iout, iskip, nbits, 0, 1)
end subroutine g2_gbytec

!> Extract arbitrary size values (up to 32 bits each) from a packed
!> bit string, right justifying each value in the unpacked array.
!>
!> @param[in] in array input
!> @param[out] iout unpacked array output
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in IN to take. Must
!> be 32 or less.
!> @param[in] nskip Additional number of bits to skip on each iteration.
!> @param[in] n Number of integers to extract.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_gbytesc(in, iout, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(in) :: in(*)
  integer, intent(out) :: iout(*)
  integer, intent(in) :: iskip, nbits, nskip, n
  integer :: tbit, bitcnt
  integer, parameter :: ones(8) = (/ 1, 3, 7, 15, 31, 63, 127, 255 /)

  integer :: nbit, i, index, ibit, itmp
  integer, external :: mova2i

  !     nbit is the start position of the field in bits
  nbit = iskip
  do i = 1, n
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     !        first byte
     tbit = min(bitcnt, 8 - ibit)
     itmp = iand(mova2i(in(index)), ones(8 - ibit))
     if (tbit .ne. 8 - ibit) itmp = ishft(itmp, tbit - 8 + ibit)
     index = index + 1
     bitcnt = bitcnt - tbit

     !        now transfer whole bytes
     do while (bitcnt .ge. 8)
        itmp = ior(ishft(itmp,8), mova2i(in(index)))
        bitcnt = bitcnt - 8
        index = index + 1
     enddo

     !        get data from last byte
     if (bitcnt .gt. 0) then
        itmp = ior(ishft(itmp, bitcnt), iand(ishft(mova2i(in(index)), &
             - (8 - bitcnt)), ones(bitcnt)))
     endif

     iout(i) = itmp
  enddo

end subroutine g2_gbytesc

!> Extract one arbitrary sized (up to 64-bits) values from a packed bit
!> string, right justifying each value in the unpacked array.
!>
!> This should be used when input array in has only one element. If in
!> has more elements, use g2_sbytesc().
!>
!> @param[in] in Array input.
!> @param[inout] iout Unpacked array output.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in IN to take. Must
!> be 64 or less.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_gbytec8(in, iout, iskip, nbits)
  implicit none

  character*1, intent(in) :: in(*)
  integer (kind = 8), intent(inout) :: iout(*)
  integer, intent(in) :: iskip, nbits
  call g2_gbytesc(in, iout, iskip, nbits, 0, 1)
end subroutine g2_gbytec8

!> Extract arbitrary sized (up to 64-bits) values from a packed bit
!> string, right justifying each value in the unpacked array.
!>
!> @param[in] in array input
!> @param[out] iout unpacked array output
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in IN to take. Must
!> be 64 or less.
!> @param[in] nskip Additional number of bits to skip on each iteration.
!> @param[in] n Number of integers to extract.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_gbytesc8(in, iout, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(in) :: in(*)
  integer (kind = 8), intent(out) :: iout(*)
  integer, intent(in) :: iskip, nbits, nskip, n
  integer :: tbit, bitcnt
  integer, parameter :: ones(8) = (/ 1, 3, 7, 15, 31, 63, 127, 255 /)

  integer :: nbit, i, index, ibit, itmp
  integer, external :: mova2i

  !     nbit is the start position of the field in bits
  nbit = iskip
  do i = 1, n
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     !        first byte
     tbit = min(bitcnt, 8 - ibit)
     itmp = iand(mova2i(in(index)), ones(8 - ibit))
     if (tbit .ne. 8 - ibit) itmp = ishft(itmp, tbit - 8 + ibit)
     index = index + 1
     bitcnt = bitcnt - tbit

     !        now transfer whole bytes
     do while (bitcnt .ge. 8)
        itmp = ior(ishft(itmp,8), mova2i(in(index)))
        bitcnt = bitcnt - 8
        index = index + 1
     enddo

     !        get data from last byte
     if (bitcnt .gt. 0) then
        itmp = ior(ishft(itmp, bitcnt), iand(ishft(mova2i(in(index)), &
             - (8 - bitcnt)), ones(bitcnt)))
     endif

     iout(i) = itmp
  enddo

end subroutine g2_gbytesc8

!> Put one arbitrary sized (up to 32 bits) values into a packed bit
!> string, taking the low order bits from the value in the unpacked
!> array.
!>
!> This should be used when input array IN has only one element. If IN
!> has more elements, use g2_sbytesc().
!>
!> @param[inout] out packed array output
!> @param[in] in unpacked array input
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in OUT to fill.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_sbytec(out, in, iskip, nbits)
  implicit none

  character*1, intent(inout) :: out(*)
  integer, intent(in) :: in(*)
  integer, intent(in) :: iskip, nbits
  call g2_sbytesc(out, in, iskip, nbits, 0, 1)
end subroutine g2_sbytec

!> Put arbitrary size (up to 32 bits each) values into a packed bit
!> string, taking the low order bits from each value in the unpacked
!> array.
!>
!> @param[out] out Packed array output.
!> @param[in] in Unpacked array input.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 32 or less.
!> @param[in] nskip Additional number of bits to skip on each iteration.
!> @param[in] n Number of iterations.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_sbytesc(out, in, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(out) :: out(*)
  integer, intent(in) :: in(n)
  integer, intent(in) :: iskip, nbits, nskip, n
  
  integer :: bitcnt, tbit
  integer, parameter :: ones(8)=(/ 1,  3,  7, 15, 31, 63, 127, 255/)
  integer :: nbit, i, itmp, index, ibit, imask, itmp2, itmp3
  integer, external :: mova2i

  ! number bits from zero to ...
  ! nbit is the last bit of the field to be filled
  nbit = iskip + nbits - 1
  do i = 1, n
     itmp = in(i)
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     ! make byte aligned
     if (ibit .ne. 7) then
        tbit = min(bitcnt, ibit + 1)
        imask = ishft(ones(tbit), 7 - ibit)
        itmp2 = iand(ishft(itmp, 7 - ibit),imask)
        itmp3 = iand(mova2i(out(index)), 255 - imask)
        out(index) = char(ior(itmp2, itmp3))
        bitcnt = bitcnt - tbit
        itmp = ishft(itmp, -tbit)
        index = index - 1
     endif

     ! now byte aligned

     ! do by bytes
     do while (bitcnt .ge. 8)
        out(index) = char(iand(itmp, 255))
        itmp = ishft(itmp, -8)
        bitcnt = bitcnt - 8
        index = index - 1
     enddo

     ! do last byte
     if (bitcnt .gt. 0) then
        itmp2 = iand(itmp, ones(bitcnt))
        itmp3 = iand(mova2i(out(index)), 255 - ones(bitcnt))
        out(index) = char(ior(itmp2, itmp3))
     endif
  enddo

end subroutine g2_sbytesc

!> Put one arbitrary sized (up to 64 bits) values into a packed bit
!> string, taking the low order bits from each value in the unpacked
!> array.
!>
!> This should be used when input array IN has only one element. If IN
!> has more elements, use g2_sbytesc().
!>
!> @param[inout] out packed array output
!> @param[in] in unpacked array input
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 64 or less.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_sbytec8(out, in, iskip, nbits)
  implicit none

  character*1, intent(inout) :: out(*)
  integer (kind = 8), intent(in) :: in(*)
  integer, intent(in) :: iskip, nbits
  call g2_sbytesc8(out, in, iskip, nbits, 0, 1)
end subroutine g2_sbytec8

!> Put arbitrary sized (up to 64 bits each) values into a packed bit
!> string, taking the low order bits from each value in the unpacked
!> array.
!>
!> @param[out] out Packed array output.
!> @param[in] in Unpacked array input.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 64 or less.
!> @param[in] nskip Additional number of bits to skip on each iteration.
!> @param[in] n Number of iterations.
!>
!> @author Stephen Gilbert @date 2004-04-27
subroutine g2_sbytesc8(out, in, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(out) :: out(*)
  integer (kind = 8), intent(in) :: in(n)
  integer, intent(in) :: iskip, nbits, nskip, n
  
  integer :: bitcnt, tbit
  integer, parameter :: ones(8)=(/ 1,  3,  7, 15, 31, 63, 127, 255/)
  integer :: nbit, i, itmp, index, ibit, imask, itmp2, itmp3
  integer, external :: mova2i

  ! number bits from zero to ...
  ! nbit is the last bit of the field to be filled
  nbit = iskip + nbits - 1
  do i = 1, n
     itmp = int(in(i), kind(8))
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     ! make byte aligned
     if (ibit .ne. 7) then
        tbit = min(bitcnt, ibit + 1)
        imask = ishft(ones(tbit), 7 - ibit)
        itmp2 = iand(ishft(itmp, 7 - ibit),imask)
        itmp3 = iand(mova2i(out(index)), 255 - imask)
        out(index) = char(ior(itmp2, itmp3))
        bitcnt = bitcnt - tbit
        itmp = ishft(itmp, -tbit)
        index = index - 1
     endif

     ! now byte aligned

     ! do by bytes
     do while (bitcnt .ge. 8)
        out(index) = char(iand(itmp, 255))
        itmp = ishft(itmp, -8)
        bitcnt = bitcnt - 8
        index = index - 1
     enddo

     ! do last byte
     if (bitcnt .gt. 0) then
        itmp2 = iand(itmp, ones(bitcnt))
        itmp3 = iand(mova2i(out(index)), 255 - ones(bitcnt))
        out(index) = char(ior(itmp2, itmp3))
     endif
  enddo
end subroutine g2_sbytesc8
