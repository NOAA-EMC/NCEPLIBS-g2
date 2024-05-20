!> @file
!> @brief Extract or store arbitrary size values between packed bit
!> string and integer or real scalars, or integer arrays.
!> @author Stephen Gilbert @date 2004-04-27

!> Extract one arbitrary size big-endian value (up to 32 bits) from a
!> packed bit string into one element of an integer array.
!>
!> This should be used converting one integer*4 value into an array
!> element. If more values need to be converted, use g2_sbytesc(). To
!> convert into a scalar integer, use g2_gbytec1().
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

  interface
     subroutine g2_gbytesc(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer, intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc
  end interface
     
  call g2_gbytesc(in, iout, iskip, nbits, 0, 1)
end subroutine g2_gbytec

!> Extract one arbitrary size big-endian integer value (up to 32 bits)
!> from a packed bit string into a scalar integer.
!>
!> This should be used converting one integer*4 value. If more values
!> need to be converted, use g2_sbytesc().
!>
!> @param[in] in Character array input.
!> @param[inout] siout Unpacked scalar integer output.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer to take. Must
!> be 32 or less.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_gbytec1(in, siout, iskip, nbits)
  implicit none

  character*1, intent(in) :: in(*)
  integer, intent(inout) :: siout
  integer, intent(in) :: iskip, nbits

  integer (kind = 4) :: iout(1)

  interface
     subroutine g2_gbytesc(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer, intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc
  end interface
  
  call g2_gbytesc(in, iout, iskip, nbits, 0, 1)
  siout = iout(1)
end subroutine g2_gbytec1

!> Extract big-endian floating-point values (32 bits each) from a
!> packed bit string.
!>
!> @param[in] in array input
!> @param[out] rout unpacked array output
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each real in IN to take. Must
!> be 32.
!> @param[in] nskip Additional number of bits to skip on each
!iteration.
!> @param[in] n Number of floats to extract.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_gbytescr(in, rout, iskip, nbits, nskip, n)
  implicit none
  character*1, intent(in) :: in(*)
  real (kind = 4), intent(out) :: rout(*)
  integer, intent(in) :: iskip, nbits, nskip, n
  integer (kind = 4) :: iout(n)

  interface
     subroutine g2_gbytesc(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer, intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc
  end interface

  ! Unpack into integer array.
  call g2_gbytesc(in, iout, iskip, nbits, nskip, n)

  ! Transfer to real array.
  rout(1:n) = transfer(iout, rout(1:n), n)
end subroutine g2_gbytescr

!> Extract arbitrary size big-endian integer values (up to 32 bits
!> each) from a packed bit string.
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
  integer, external :: g2_mova2i

  !     nbit is the start position of the field in bits
  nbit = iskip
  do i = 1, n
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     !        first byte
     tbit = min(bitcnt, 8 - ibit)
     itmp = iand(g2_mova2i(in(index)), ones(8 - ibit))
     if (tbit .ne. 8 - ibit) itmp = ishft(itmp, tbit - 8 + ibit)
     index = index + 1
     bitcnt = bitcnt - tbit

     !        now transfer whole bytes
     do while (bitcnt .ge. 8)
        itmp = ior(ishft(itmp,8), g2_mova2i(in(index)))
        bitcnt = bitcnt - 8
        index = index + 1
     enddo

     !        get data from last byte
     if (bitcnt .gt. 0) then
        itmp = ior(ishft(itmp, bitcnt), iand(ishft(g2_mova2i(in(index)), &
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
!> @author Edward Hartnett @date 2024
subroutine g2_gbytec8(in, iout, iskip, nbits)
  implicit none

  character*1, intent(in) :: in(*)
  integer (kind = 8), intent(inout) :: iout(*)
  integer, intent(in) :: iskip, nbits

  interface
     subroutine g2_gbytesc8(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc8
  end interface
     
  call g2_gbytesc8(in, iout, iskip, nbits, 0, 1)
end subroutine g2_gbytec8

!> Extract one arbitrary size big-endian integer value (up to 64 bits)
!> from a packed bit string into a scalar integer.
!>
!> This should be used converting one integer*4 value. If more values
!> need to be converted, use g2_sbytesc().
!>
!> @param[in] in Character array input.
!> @param[inout] siout Unpacked scalar integer output.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer to take. Must
!> be 32 or less.
!>
!> @author Edward Hartnett @date 2024-05-11
subroutine g2_gbytec81(in, siout, iskip, nbits)
  implicit none

  character*1, intent(in) :: in(*)
  integer (kind = 8), intent(inout) :: siout
  integer, intent(in) :: iskip, nbits
  integer (kind = 8) :: iout(1)

  interface
     subroutine g2_gbytesc8(in, iout, iskip, nbits, nskip, n)
       character*1, intent(in) :: in(*)
       integer (kind = 8), intent(out) :: iout(*)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_gbytesc8
  end interface
  
  call g2_gbytesc8(in, iout, iskip, nbits, 0, 1)
  siout = iout(1)
end subroutine g2_gbytec81

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
!> @author Edward Hartnett, Stephen Gilbert @date 2024
subroutine g2_gbytesc8(in, iout, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(in) :: in(*)
  integer (kind = 8), intent(out) :: iout(*)
  integer, intent(in) :: iskip, nbits, nskip, n
  
  integer :: tbit, bitcnt
  integer, parameter :: ones(8) = (/ 1, 3, 7, 15, 31, 63, 127, 255 /)

  integer :: nbit, i, index, ibit, itmp
  integer (kind = 8) :: itmp8, itmp8_2, itmp8_3
  integer, external :: g2_mova2i
  integer (kind = 8), external :: g2_mova2i8

  ! Nbit is the start position of the field in bits.
  nbit = iskip
  do i = 1, n
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip

     ! first byte
     tbit = min(bitcnt, 8 - ibit)
     itmp8 = iand(g2_mova2i8(in(index)), int(ones(8 - ibit), kind = 8))
     itmp = iand(g2_mova2i(in(index)), ones(8 - ibit))
     if (tbit .ne. 8 - ibit) itmp = ishft(itmp, tbit - 8 + ibit)
     if (tbit .ne. 8 - ibit) itmp8 = ishft(itmp8, tbit - 8 + ibit)
     index = index + 1
     bitcnt = bitcnt - tbit

     ! now transfer whole bytes
     do while (bitcnt .ge. 8)
        itmp = ior(ishft(itmp,8), g2_mova2i(in(index)))
        itmp8 = ior(ishft(itmp8,8), g2_mova2i8(in(index)))
        bitcnt = bitcnt - 8
        index = index + 1
     enddo

     ! get data from last byte
     if (bitcnt .gt. 0) then
        itmp = ior(ishft(itmp, bitcnt), iand(ishft(g2_mova2i(in(index)), - (8 - bitcnt)), ones(bitcnt)))
        itmp8_2 = ishft(g2_mova2i8(in(index)), int(-(8 - bitcnt), kind(8)))
        itmp8_3 = int(ones(bitcnt), kind(8))
        itmp8 = ior(ishft(itmp8, bitcnt), iand(itmp8_2, itmp8_3))
     endif

     iout(i) = itmp8
  enddo
end subroutine g2_gbytesc8

!> Put one arbitrary sized (up to 32 bits) value from an integer
!> array, into a packed bit string, in big-endian format.
!>
!> This should be used when input is an array and one value is to be
!> packed. If more values are to be packed, use g2_sbytesc(). If
!> packing a scalar integer, use g2_sytec1().
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

  interface
     subroutine g2_sbytesc(out, in, iskip, nbits, nskip, n)
       character*1, intent(out) :: out(*)
       integer, intent(in) :: in(n)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_sbytesc
  end interface
     
  call g2_sbytesc(out, in, iskip, nbits, 0, 1)
end subroutine g2_sbytec

!> Put one arbitrary sized (up to 32 bits) values from an integer
!> scalar into a packed bit string, in big-endian format.
!>
!> This should be used when input array in is a scalar. If an array
!> element is to be packed, use g1_sbytec(). If more than one integer
!> is to be packed, use g2_sbytesc().
!>
!> @param[inout] out packed characeter array output.
!> @param[in] in unpacked scalar integer input.
!> @param[in] iskip initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in OUT to fill.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_sbytec1(out, in, iskip, nbits)
  implicit none

  character*1, intent(inout) :: out(*)
  integer, intent(in) :: in
  integer, intent(in) :: iskip, nbits
  integer :: ain(1)

  interface
     subroutine g2_sbytesc(out, in, iskip, nbits, nskip, n)
       character*1, intent(out) :: out(*)
       integer, intent(in) :: in(n)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_sbytesc
  end interface

  ain(1) = in
  call g2_sbytesc(out, ain, iskip, nbits, 0, 1)
end subroutine g2_sbytec1

!> Put real values into a packed bit string in big-endian order.
!>
!> @param[out] out Packed character array output.
!> @param[in] rin real array input.
!> @param[in] iskip Initial number of bits to skip.
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 32.
!> @param[in] nskip Additional number of bits to skip on each iteration.
!> @param[in] n Number of iterations.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_sbytescr(out, rin, iskip, nbits, nskip, n)
  implicit none
  character*1, intent(out) :: out(*)
  real, intent(in) :: rin(n)
  integer, intent(in) :: iskip, nbits, nskip, n
  integer :: in(n)

  interface
     subroutine g2_sbytesc(out, in, iskip, nbits, nskip, n)
       character*1, intent(out) :: out(*)
       integer, intent(in) :: in(n)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_sbytesc
  end interface

  ! Transfer real array to integer array.
  in(1:n) = transfer(rin, in(1:n), n)

  ! Pack into character array.
  call g2_sbytesc(out, in, iskip, nbits, nskip, n)
end subroutine g2_sbytescr

!> Put arbitrary size (up to 32 bits each) integer values
!> into a packed bit string in big-endian order.
!>
!> @param[out] out Packed character array output.
!> @param[in] in Integer array input.
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
  integer, external :: g2_mova2i

  ! number bits from zero to ...
  ! nbit is the last bit of the field to be filled
  nbit = iskip + nbits - 1
  !print *, 'nbit', nbit, 'nbits ', nbits, 'nskip', nskip, 'n', n
  do i = 1, n
     itmp = in(i)
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip
     !print *, 'i', i, 'itmp', itmp, 'bitcnt', bitcnt, 'index', index, 'ibit', ibit, 'nbit', nbit
     
     ! make byte aligned
     if (ibit .ne. 7) then
        tbit = min(bitcnt, ibit + 1)
        imask = ishft(ones(tbit), 7 - ibit)
        itmp2 = iand(ishft(itmp, 7 - ibit),imask)
        itmp3 = iand(g2_mova2i(out(index)), 255 - imask)
        out(index) = char(ior(itmp2, itmp3))
        bitcnt = bitcnt - tbit
        itmp = ishft(itmp, -tbit)
        index = index - 1
     endif

     ! now byte aligned

     ! do by bytes
     do while (bitcnt .ge. 8)
        out(index) = char(iand(itmp, 255))
        !print '(z2.2, x, z2.2, x, z2.2)', out(index), itmp, iand(itmp, 255)
        itmp = ishft(itmp, -8)
        bitcnt = bitcnt - 8
        index = index - 1
     enddo

     ! Do left over bits.
     if (bitcnt .gt. 0) then
        itmp2 = iand(itmp, ones(bitcnt))
        !print '(z2.2, x, z2.2)', ones(bitcnt), itmp2
        itmp3 = iand(g2_mova2i(out(index)), 255 - ones(bitcnt))
        out(index) = char(ior(itmp2, itmp3))
     endif
  enddo
end subroutine g2_sbytesc

!> Put one arbitrary sized (up to 64 bits) values into a packed bit
!> string, taking the low order bits from each value in the unpacked
!> array.
!>
!> This should be used when input array IN has only one element. If IN
!> has more elements, use g2_sbytesc8().
!>
!> @param[inout] out packed array output
!> @param[in] in unpacked array input
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 64 or less.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_sbytec8(out, in, iskip, nbits)
  implicit none

  character*1, intent(inout) :: out(*)
  integer (kind = 8), intent(in) :: in(*)
  integer, intent(in) :: iskip, nbits

  interface
     subroutine g2_sbytesc8(out, in, iskip, nbits, nskip, n)
       character*1, intent(out) :: out(*)
       integer (kind = 8), intent(in) :: in(n)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_sbytesc8
  end interface
  
  call g2_sbytesc8(out, in, iskip, nbits, 0, 1)
end subroutine g2_sbytec8

!> Put one arbitrary sized (up to 64 bits) scalar into a packed bit
!> string, taking the low order bits from each value in the unpacked
!> array.
!>
!> This should be used when input is a scalar. If the input is an
!> array, use sbytec8() or g2_sbytesc8().
!>
!> @param[inout] out packed array output
!> @param[in] sin unpacked scalar input
!> @param[in] iskip initial number of bits to skip
!> @param[in] nbits Number of bits of each integer in OUT to
!> fill. Must be 64 or less.
!>
!> @author Edward Hartnett @date 2024
subroutine g2_sbytec81(out, sin, iskip, nbits)
  implicit none

  character*1, intent(inout) :: out(*)
  integer (kind = 8), intent(in) :: sin
  integer, intent(in) :: iskip, nbits

  integer (kind = 8) :: in(1)
  
  interface
     subroutine g2_sbytesc8(out, in, iskip, nbits, nskip, n)
       character*1, intent(out) :: out(*)
       integer (kind = 8), intent(in) :: in(n)
       integer, intent(in) :: iskip, nbits, nskip, n
     end subroutine g2_sbytesc8
  end interface
  
  in(1) = sin
  call g2_sbytesc8(out, in, iskip, nbits, 0, 1)
end subroutine g2_sbytec81

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
!> @author Edward Hartnett, Stephen Gilbert @date 2024
subroutine g2_sbytesc8(out, in, iskip, nbits, nskip, n)
  implicit none

  character*1, intent(out) :: out(*)
  integer (kind = 8), intent(in) :: in(n)
  integer, intent(in) :: iskip, nbits, nskip, n
  
  integer :: bitcnt, tbit
  integer, parameter :: ones(8)=(/ 1,  3,  7, 15, 31, 63, 127, 255/)
  integer :: nbit, i, index, ibit, imask, itmp1, itmp2, itmp3
  integer (kind = 8) :: itmp8, itmp8_2
  integer, external :: g2_mova2i

  ! number bits from zero to ...
  ! nbit is the last bit of the field to be filled
  nbit = iskip + nbits - 1
  !print *, 'nbit', nbit, 'nbits ', nbits, 'nskip', nskip, 'n', n
  do i = 1, n
     itmp8 = in(i)
     bitcnt = nbits
     index = nbit / 8 + 1
     ibit = mod(nbit, 8)
     nbit = nbit + nbits + nskip
     !print *, 'i', i, 'itmp8', itmp8, 'bitcnt', bitcnt, 'index', index, 'ibit', ibit, 'nbit', nbit 

     ! make byte aligned
     if (ibit .ne. 7) then
        tbit = min(bitcnt, ibit + 1)
        imask = ishft(ones(tbit), 7 - ibit)
        itmp1 = int(ishft(itmp8, int(7 - ibit, kind(8))), kind(4))
        itmp2 = iand(itmp1, imask)
        itmp3 = iand(g2_mova2i(out(index)), 255 - imask)
        out(index) = char(ior(itmp2, itmp3))
        bitcnt = bitcnt - tbit
        itmp8 = ishft(itmp8, -tbit)
        index = index - 1
     endif

     ! now byte aligned

     ! Process a byte at a time.
     do while (bitcnt .ge. 8)
        !print *, bitcnt, iand(itmp8, 255_8)
        out(index) = char(iand(itmp8, 255_8))
        itmp8 = ishft(itmp8, -8)
        bitcnt = bitcnt - 8
        index = index - 1
     enddo

     ! Take care of left over bits.
     if (bitcnt .gt. 0) then
        itmp8_2 = int(ones(bitcnt), kind(8))
        itmp2 = int(iand(itmp8, itmp8_2), kind(4))
        itmp3 = iand(g2_mova2i(out(index)), 255 - ones(bitcnt))
        out(index) = char(ior(itmp2, itmp3))
     endif
  enddo
end subroutine g2_sbytesc8

!> Copy array of 32-bit IEEE floating point values stored in char
!> array to local floating point representation.
!>
!> @param[in] cieee Input char array containing floating point values
!> in 32-bit IEEE format.
!> @param[out] a Output array of real values.
!> @param[in] num Number of floating point values to convert.
!>
!> @author Edward Hartnett @date Mar 12, 2024
subroutine rdieeec(cieee, a, num)
  implicit none
  
  character(len = 1), intent(in) :: cieee(*)
  real, intent(out) :: a(num)
  integer, intent(in) :: num
  real (kind = 4) :: rieee(num)

  interface
     subroutine rdieee(rieee, a, num)
       real(4), intent(in) :: rieee(num)
       real, intent(out) :: a(num)
       integer, intent(in) :: num
     end subroutine rdieee
  end interface

  rieee(1:num) = transfer(cieee(1:num * 4), rieee, num)
  call rdieee(rieee, a, num)
end subroutine rdieeec

!> Copy array of 32-bit IEEE floating point values to local
!> floating point representation.
!>
!> @param[in] rieee Input array of floating point values in 32-bit
!> IEEE format. Note that this array is always 32-bit floats, even
!> when compiled with -d.
!> @param[out] a Output array of real values.
!> @param[in] num Number of floating point values to convert.
!>
!> @author Stephen Gilbert @date 2000-05-09
subroutine rdieee(rieee, a, num)
  implicit none
  
  real(4), intent(in) :: rieee(num)
  real, intent(out) :: a(num)
  integer, intent(in) :: num

  integer(4) :: ieee
  real, parameter :: two23 = scale(1.0, -23)
  real, parameter :: two126 = scale(1.0, -126)
  integer :: iexp, imant, isign, j
  real :: sign, temp

  do j = 1, num
     ! Transfer IEEE bit string to integer variable.
     ieee = transfer(rieee(j), ieee)

     ! Extract sign bit, exponent, and mantissa.
     isign = ibits(ieee, 31, 1)
     iexp = ibits(ieee, 23, 8)
     imant = ibits(ieee, 0, 23)
     sign = 1.0
     if (isign .eq. 1) sign = -1.0

     if (iexp .gt. 0 .and. iexp .lt. 255) then
        temp = 2.0**(iexp - 127)
        a(j) = sign * temp * (1.0 + (two23 * real(imant)))
     elseif (iexp .eq. 0) then
        if (imant .ne. 0) then
           a(j) = sign * two126 * two23 * real(imant)
        else
           a(j) = sign * 0.0
        endif
     elseif (iexp .eq. 255) then
        a(j) = sign * huge(a(j))
     endif
  enddo
end subroutine rdieee

!> Copy an array of real to an array of 32-bit IEEE floating points.
!>
!> @param[in] a Input array of floating point values.
!> @param[out] rieee Output array of floating point values in 32-bit
!> IEEE format. Note that this array is always 32-bit floats, even
!> when compiled with -d.
!> @param[in] num Number of floating point values to convert.
!>
!> @author Stephen Gilbert @date 2000-05-09
subroutine mkieee(a, rieee, num)
  implicit none
  
  real(4), intent(in) :: a(num)
  real(4), intent(out) :: rieee(num)
  integer, intent(in) :: num

  integer(4) :: ieee
  real, parameter :: two23 = scale(1.0,23)
  real, parameter :: two126 = scale(1.0,126)
  real :: alog2, atemp
  integer :: iexp, imant, j, n

  alog2 = alog(2.0)

  do j = 1, num
     ieee = 0
     if (a(j) .eq. 0.) then
        ieee = 0
        rieee(j) = transfer(ieee, rieee(j))
        cycle
     endif

     ! Set Sign bit (bit 31 - leftmost bit).
     if (a(j) .lt. 0.0) then
        ieee = ibset(ieee, 31)
        atemp = abs(a(j))
     else
        ieee = ibclr(ieee, 31)
        atemp = a(j)
     endif

     ! Determine exponent n with base 2.
     if (atemp .ge. 1.0) then
        n = 0
        do while (2.0**(n+1) .le. atemp)
           n = n + 1
        enddo
     else
        n = -1
        do while (2.0**n .gt. atemp )
           n = n - 1
        enddo
     endif
     iexp = n + 127
     if (n .gt. 127) iexp = 255 ! overflow
     if (n .lt. -127) iexp = 0
     call mvbits(iexp, 0, 8, ieee, 23)

     ! Determine Mantissa.
     if (iexp .ne. 255) then
        if (iexp .ne. 0) then
           atemp = (atemp / (2.0**n)) - 1.0
        else
           atemp = atemp * two126
        endif
        imant = nint(atemp * two23)
     else
        imant = 0
     endif
     ! set mantissa bits (bits 22-0).
     call mvbits(imant, 0, 23, ieee, 0)

     ! Transfer IEEE bit string to real variable.
     rieee(j) = transfer(ieee, rieee(j))
  enddo
end subroutine mkieee

