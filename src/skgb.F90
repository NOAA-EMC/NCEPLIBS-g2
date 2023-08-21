!> @file
!> @brief Search a file for the next GRIB1 or GRIB2 message.
!> @author Mark Iredell @date 1995-10-31

!> Search a file for the next GRIB1 or GRIB2 message.
!>
!> A grib message is identified by its indicator section, which is an
!> 8-byte sequence with 'GRIB' in bytes 1-4 and a '1' or '2' in byte
!> 8. If found, the length of the message is decoded from bytes
!> 5-7. The search is done over a given section of the file. The
!> search is terminated if an eof or i/o error is encountered.
!>
!> @param[in] lugb Unit of the unblocked grib file.
!> @param[in] iseek Number of bytes to skip before search.
!> @param[in] mseek Maximum number of bytes to search.
!> @param[out] lskip Number of bytes to skip before message.
!> @param[out] lgrib Number of bytes in message (0 if not found).
!>
!> @author Mark Iredell @date 1995-10-31
subroutine skgb(lugb, iseek, mseek, lskip, lgrib)
  implicit none

  integer lseek, lugb, iseek, mseek, lskip, lgrib, i1, i4, k, k4, kg, km, ks, kz, kn
  parameter(lseek = 512)
  character z(lseek)
  character z4(4)

!  print *,'iseek ', iseek, ' mseek ', mseek, ' lskip ', lskip
  lgrib = 0
  ks = iseek
  kn = min(lseek, mseek)
  kz = lseek

  !  loop until grib message is found
  do while (lgrib .eq. 0 .and. kn .ge. 8 .and. kz .eq. lseek)
     !  read partial section
     call baread(lugb, ks, kn, kz, z)
     km = kz - 8 + 1
     k = 0
     !  look for 'grib...1' in partial section
     do while (lgrib .eq. 0 .and. k .lt. km)
        call g2_gbytec(z, i4, (k + 0) * 8, 4 * 8)
        call g2_gbytec(z, i1, (k + 7) * 8, 1 * 8)
        if (i4 .eq. 1196575042 .and. (i1 .eq. 1 .or. i1 .eq. 2)) then
           !  look for '7777' at end of grib message
           if (i1 .eq. 1) call g2_gbytec(z, kg, (k + 4) * 8, 3 * 8)
           if (i1 .eq. 2) call g2_gbytec(z, kg, (k + 12) * 8, 4 * 8)
           call baread(lugb, ks + k + kg-4, 4, k4, z4)
           if (k4 .eq. 4) then
              call g2_gbytec(z4, i4, 0, 4 * 8)
              if (i4 .eq. 926365495) then
                 !  grib message found
                 lskip = ks + k
                 lgrib = kg
              endif
           endif
        endif
        k = k + 1
     enddo
     ks = ks + km
     kn = min(lseek, iseek + mseek - ks)
  enddo
end subroutine skgb

!> Search a file for the next GRIB1 or GRIB2 message.
!>
!> A grib message is identified by its indicator section, which is an
!> 8-byte sequence with 'GRIB' in bytes 1-4 and a '1' or '2' in byte
!> 8. If found, the length of the message is decoded from bytes
!> 5-7. The search is done over a given section of the file. The
!> search is terminated if an eof or i/o error is encountered.
!>
!> @param[in] lugb Unit of the unblocked grib file.
!> @param[in] iseek8 Number of bytes to skip before search.
!> @param[in] mseek8 Maximum number of bytes to search.
!> @param[out] lskip8 Number of bytes to skip before message.
!> @param[out] lgrib8 Number of bytes in message (0 if not found).
!>
!> @author Edward Hartnett @date 2023-08-21
subroutine skgb8(lugb, iseek8, mseek8, lskip8, lgrib8)
  implicit none

  integer*8 iseek8, mseek8, lskip8, lgrib8
  integer*8 ks8, kn8, kz8, k8, kg8, k48, km8
  integer lseek, lugb, i1, i4, k, kg
  parameter(lseek = 512)
  character z(lseek)
  character z4(4)

  lgrib8 = 0
  ks8 = iseek8
  kn8 = min(int(lseek, kind = 8), mseek8)
  kz8 = lseek

  !  loop until grib message is found
  do while (lgrib8 .eq. 0 .and. kn8 .ge. 8 .and. kz8 .eq. lseek)
     !  read partial section
     print *, 'ks8 ', ks8, ' kn8 ', kn8, ' kz8 ', kz8
     call bareadl(lugb, ks8, kn8, kz8, z)
     
     km8 = kz8 - 8 + 1
     k = 0
     !  look for 'grib...1' in partial section
     do while (lgrib8 .eq. 0 .and. k .lt. km8)
        call g2_gbytec(z, i4, (k + 0) * 8, 4 * 8)
        call g2_gbytec(z, i1, (k + 7) * 8, 1 * 8)
        if (i4 .eq. 1196575042 .and. (i1 .eq. 1 .or. i1 .eq. 2)) then
           !  look for '7777' at end of grib message
           if (i1 .eq. 1) call g2_gbytec(z, kg, (k + 4) * 8, 3 * 8)
           if (i1 .eq. 2) call g2_gbytec(z, kg, (k + 12) * 8, 4 * 8)
           
           k8 = k
           kg8 = kg
           call bareadl(lugb, ks8 + k8 + kg8 - 4, 4_8, k48, z4)
           if (k48 .eq. 4) then
              call g2_gbytec(z4, i4, 0, 4 * 8)
              if (i4 .eq. 926365495) then
                 !  grib message found
                 lskip8 = ks8 + k
                 lgrib8 = kg8
              endif
           endif
        endif
        k = k + 1
     enddo
     ks8 = ks8 + km8
     kn8 = min(int(lseek, kind = 8), iseek8 + mseek8 - ks8)
  enddo
end subroutine skgb8
