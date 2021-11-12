C>    @file
C>    @brief This Fortran module extract or store arbitrary size values
C>    between packed bit string and unpacked array.
C>    @author Stephen Gilbert @date 2004-04-27
C>

C>    This subrountine is to extract arbitrary size values from a
C>    packed bit string, right justifying each value in the unpacked
C>    array without skip and interations.
C>
C>    This should be used when input array IN has only one element. If IN
C>    has more elements, use G2_SBYTESC().
C>
C>    @param[in] IN Array input.
C>    @param[out] IOUT Unpacked array output.
C>    @param[in] ISKIP Initial number of bits to skip.
C>    @param[in] NBYTE Number of bits of each integer in IN to take.
C>
C>    @author Stephen Gilbert @date 2004-04-27
C>

      SUBROUTINE G2_GBYTEC(IN,IOUT,ISKIP,NBYTE)
      character*1 in(*)
      integer iout(*)
      CALL G2_GBYTESC(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END

C>    This subrountine is to put arbitrary size values into a packed bit
C>    string, taking the low order bits from each value in the unpacked
C>    array without skip and interation.
C>
C>    This should be used when input array IN has only one element. If IN
C>    has more elements, use G2_SBYTESC().
C>
C>    @param[out] OUT packed array output
C>    @param[in] IN unpacked array input
C>    @param[in] ISKIP initial number of bits to skip
C>    @param[in] NBYTE Number of bits of each integer in OUT to fill.
C>
C>    @author Stephen Gilbert @date 2004-04-27
C>

      SUBROUTINE G2_SBYTEC(OUT,IN,ISKIP,NBYTE)
      character*1 out(*)
      integer in(*)
      CALL G2_SBYTESC(OUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END

C>    This subrountine is to extract arbitrary size values from a
C>    packed bit string, right justifying each value in the unpacked
C>    array with skip and interation options.
C>
C>    @param[in] IN array input
C>    @param[out] IOUT unpacked array output
C>    @param[in] ISKIP initial number of bits to skip
C>    @param[in] NBYTE Number of bits of each integer in IN to take.
C>    @param[in] NSKIP Additional number of bits to skip on each iteration.
C>    @param[in] N Number of integers to extract.
C>
C>    @author Stephen Gilbert @date 2004-04-27
C>

      SUBROUTINE G2_GBYTESC(IN,IOUT,ISKIP,NBYTE,NSKIP,N)

      character*1 in(*)
      integer iout(*)
      integer tbit, bitcnt
      integer, parameter :: ones(8) = (/ 1,3,7,15,31,63,127,255 /)

c     nbit is the start position of the field in bits
      nbit = iskip
      do i = 1, n
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        first byte
         tbit = min(bitcnt,8-ibit)
         itmp = iand(mova2i(in(index)),ones(8-ibit))
         if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
         index = index + 1
         bitcnt = bitcnt - tbit

c        now transfer whole bytes
         do while (bitcnt.ge.8)
             itmp = ior(ishft(itmp,8),mova2i(in(index)))
             bitcnt = bitcnt - 8
             index = index + 1
         enddo

c        get data from last byte
         if (bitcnt.gt.0) then
             itmp = ior(ishft(itmp,bitcnt),iand(ishft(mova2i(in(index)),
     1          -(8-bitcnt)),ones(bitcnt)))
         endif

         iout(i) = itmp
      enddo

      RETURN
      END                                                                  

C>    This subrountine is to put arbitrary size values into a packed bit
C>    string, taking the low order bits from each value in the unpacked
C>    array with skip and interation options.
C>
C>    @param[out] OUT Packed array output.
C>    @param[in] IN Unpacked array input.
C>    @param[in] ISKIP Initial number of bits to skip.
C>    @param[in] NBYTE Number of bits of each integer in OUT to fill.
C>    @param[in] NSKIP Additional number of bits to skip on each iteration.
C>    @param[in] N Number of iterations.
C>
C>    @author Stephen Gilbert @date 2004-04-27
C>

      SUBROUTINE G2_SBYTESC(OUT,IN,ISKIP,NBYTE,NSKIP,N)

      character*1 out(*)
      integer in(N), bitcnt, tbit
      integer, parameter :: ones(8)=(/ 1,  3,  7, 15, 31, 63,127,255/)

c     number bits from zero to ...
c     nbit is the last bit of the field to be filled

      nbit = iskip + nbyte - 1
      do i = 1, n
         itmp = in(i)
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        make byte aligned 
         if (ibit.ne.7) then
             tbit = min(bitcnt,ibit+1)
             imask = ishft(ones(tbit),7-ibit)
             itmp2 = iand(ishft(itmp,7-ibit),imask)
             itmp3 = iand(mova2i(out(index)), 255-imask)
             out(index) = char(ior(itmp2,itmp3))
             bitcnt = bitcnt - tbit
             itmp = ishft(itmp, -tbit)
             index = index - 1
         endif

c        now byte aligned

c        do by bytes
         do while (bitcnt.ge.8)
             out(index) = char(iand(itmp,255))
             itmp = ishft(itmp,-8)
             bitcnt = bitcnt - 8
             index = index - 1
         enddo

c        do last byte

         if (bitcnt.gt.0) then
             itmp2 = iand(itmp,ones(bitcnt))
             itmp3 = iand(mova2i(out(index)), 255-ones(bitcnt))
             out(index) = char(ior(itmp2,itmp3))
         endif
      enddo

      return
      end
