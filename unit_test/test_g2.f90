program test_g2
  implicit none
  real, allocatable :: infield(:,:,:), outfield(:,:,:)
  logical(kind=1), allocatable :: inmask(:,:,:), outmask(:,:,:)
  character(len=:), allocatable :: records(:)
  integer, allocatable :: recsizes(:)
  integer, parameter :: MAX_PRINT=10
  integer, parameter :: JPEG_MAX_MASK=16383

  ! nx, ny = grid size
  ! nr = number of records
  ! nb = maximum number of bytes possible in encoded record
  ! i, j = grid loop indices
  ! r = record loop index

  ! records = GRIB record buffer
  ! recsizes = encoded GRIB message sizes
  ! infield = input data to GRIB encoder
  ! outfield = output data from GRIB decoder

  ! isuite = loop index over entire test suite
  ! test_name = human-readable name of the test being run

  integer :: i,j,nx,ny,nr,nb,r,isuite

  integer, parameter :: verbose=0 ! verbosity level
  integer,parameter :: npar=3 ! How many copies in parallel?
  integer,parameter :: nser=1 ! How many times to repeat the suite?

  real, parameter :: ep14bit = 6.103515625e-05 ! 2**-14 value for epsilon
  real, parameter :: ep8bit = 0.00390625 ! 2**-8 value for epsilon
  logical, parameter :: keep_going=.false. ! keep running after failed test
  integer, parameter :: nencodings=7
  integer, parameter :: encoding(nencodings)=(/ 0, 20, 30, 31, 32, 40, 41 /)
  character(len=40), parameter :: encdesc(nencodings)=(/ &
       'simple packing                          ', &
       'complex packing option 20               ',&
       'complex packing with no spatial diff    ',&
       'complex packing and 1st order diff      ',&
       'complex packing and 2nd order diff      ',&
       '"lossless" JPEG 2000 packing            ',&
       'Portable Network Graphics (PNG) packing '/)
  character(len=10), parameter :: encname(nencodings)=(/ &
       'simplepack', &
       'complex20 ', &
       'complex30 ', &
       'complex1st', &
       'complex2nd', &
       'jpeg2000  ', &
       'png       '/)

  character(len=:), allocatable :: test_name
  logical :: success
  character*255 :: cname,cdesc
  integer :: iencode,npts
  integer :: target_nx,target_ny,isize
  real :: epsilon

  integer, parameter :: nsizes=11
  integer, parameter :: nx_target(nsizes) = (/ 3, 10, 30, 100, 200, 100, 181, 200, 300, 1000, 3000 /)
  integer, parameter :: ny_target(nsizes) = (/ 3, 10, 30, 100, 100, 200, 181, 200, 300, 1000, 3000 /)

  nx=-1
  ny=-1
  nr=-1
  success=.true.

9 format(A)
8 format(A,' with ',A,' (encoding ',I0,')')
7 format(A,'_',A,'_',I0,'x',I0)
6 format(I0,' points in mask test with ',A,' (encoding ',I0,')')
5 format('mask',I0,'pts_',A,'_',I0,'x',I0)

  ! ********************************************************************

  suiteloop: do isuite=1,nser
     sizeloop: do isize=1,10
        target_nx=nx_target(isize)
        target_ny=ny_target(isize)
        encodeloop: do iencode=1,7

           ! ----------------------------------------
           
           write(cdesc,8) 'constant value test',trim(encdesc(iencode)),&
                encoding(iencode)
           write(cname,7) 'const',trim(encname(iencode)),target_nx,target_ny
           call start_test(cname,target_nx,target_ny,npar,cdesc)
           !$omp parallel do privete(i,j,r)
           do r=1,nr
              do j=1,ny
                 do i=1,nx
                    infield(i,j,r)=1.0
                    inmask(i,j,r)=.true.
                 enddo
              enddo
           enddo
           !$omp end parallel do
           call run_test(encoding(iencode),1.5,3,ep14bit)

           ! ----------------------------------------

           write(cdesc,8) 'almost constant value test',trim(encdesc(iencode)),&
                encoding(iencode)
           write(cname,7) 'almostconst',trim(encname(iencode)),target_nx,target_ny
           call start_test(cname,target_nx,target_ny,npar,cdesc)
           !$omp parallel do privete(i,j,r)
           do r=1,nr
              do j=1,ny
                 do i=1,nx
                    infield(i,j,r)=1.0
                    inmask(i,j,r)=.true.
                 enddo
              enddo
              infield(ny/2,nx/2,r)=1.0+1e-3
           enddo
           !$omp end parallel do
           call run_test(encoding(iencode),1.5,3,ep8bit)

           ! ----------------------------------------

           write(cdesc,8) 'single value box test',trim(encdesc(iencode)),&
                encoding(iencode)
           write(cname,7) 'boxval',trim(encname(iencode)),target_nx,target_ny
           call start_test(cname,target_nx,target_ny,npar,cdesc)
           !$omp parallel do privete(i,j,r)
           do r=1,nr
              do j=1,ny
                 do i=1,nx
                    infield(i,j,r)=1.0
                    inmask(i,j,r)=.true.
                 enddo
              enddo
              do j=ny/3,2*ny/3
                 do i=nx/3,2*nx/3
                    infield(i,j,r)=1.01
                 enddo
              enddo
              infield(ny/2,nx/2,r)=1.01+1e-3
           enddo
           !$omp end parallel do
           call run_test(encoding(iencode),1.5,3,ep8bit)

           ! ----------------------------------------

           ! ONLY MASKED TESTS AFTER THIS POINT
300        format(A,': ',A)
           if(encoding(iencode)==40 .or. encoding(iencode)==40000) then
print 300,trim(encname(iencode)),'                               '
print 300,trim(encname(iencode)),'      #############            '
print 300,trim(encname(iencode)),'     #             #           '
print 300,trim(encname(iencode)),'     #   O     O   #           '
print 300,trim(encname(iencode)),'     #      ^      #    ---------------------------------------- '
print 300,trim(encname(iencode)),'     #     ___     #   | Skipping masked tests for JPEG.  :-(   |'
print 300,trim(encname(iencode)),'     #    /   \    #---| JPEG masking does not work because of  |'
print 300,trim(encname(iencode)),'     #   /     \   #   | Jasper bugs and JPEG2000 limitations.  |'
print 300,trim(encname(iencode)),'      #############    | The WMO GRIB2 manual says you "should  |'
print 300,trim(encname(iencode)),'                       | not" use JPEG for masked fields anyway |'
print 300,trim(encname(iencode)),'                        ---------------------------------------- '
print 300,trim(encname(iencode)),'                               '
              cycle
           endif

           ! Epsilon value for tests that use (j-1)*nx+i-1 for data:
           epsilon=nx*ny/100.0

           ! ----------------------------------------

           do npts=0,4
              write(cdesc,6) npts,trim(encdesc(iencode)),encoding(iencode)
              write(cname,5) npts,trim(encname(iencode)),target_nx,target_ny
              call start_test(cname,target_nx,target_ny,npar,cdesc)
              !$omp parallel do privete(i,j,r)
              do r=1,nr
                 do j=1,ny
                    do i=1,nx
                       infield(i,j,r)=(j-1)*nx+i-1
                       inmask(i,j,r)=.false.
                    enddo
                 enddo
                 if(npts>=1) inmask(nx/3,ny/3,r)=.true.
                 if(npts>=2) inmask(2*nx/3,ny/3,r)=.true.
                 if(npts>=3) inmask(nx/3,2*ny/3,r)=.true.
                 if(npts>=4) inmask(2*nx/3,2*ny/3,r)=.true.
              enddo
              !$omp end parallel do
              call run_test(encoding(iencode),1.5,3,epsilon)
           enddo

           ! ----------------------------------------

           write(cdesc,8) 'triangle mask test',trim(encdesc(iencode)),&
                encoding(iencode)
           write(cname,7) 'triangle_mask',trim(encname(iencode)),target_nx,target_ny
           call start_test(cname,target_nx,target_ny,npar,cdesc)
           !$omp parallel do privete(i,j,r)
           do r=1,nr
              do j=1,ny
                 do i=1,nx
                    inmask(i,j,r)=(i>j)
                    if(inmask(i,j,r)) then
                       infield(i,j,r)=(j-1)*nx+i-1
                    else
                       infield(i,j,r)=-((j-1)*nx+i-1)
                    endif
                 enddo
              enddo
           enddo
           !$omp end parallel do
           call run_test(encoding(iencode),1.5,3,epsilon)

           ! ----------------------------------------

           write(cdesc,8) 'box mask test',trim(encdesc(iencode)),&
                encoding(iencode)
           write(cname,7) 'box_mask',trim(encname(iencode)),target_nx,target_ny
           call start_test(cname,target_nx,target_ny,npar,cdesc)
           !$omp parallel do privete(i,j,r)
           do r=1,nr
              do j=1,ny
                 do i=1,nx
                    infield(i,j,r)=(j-1)*nx+i-1
                    inmask(i,j,r)=.false.
                 enddo
              enddo
              do j=ny/3,2*ny/3
                 do i=nx/3,2*nx/3
                    inmask(i,j,r)=.true.
                 enddo
              enddo
           enddo
           !$omp end parallel do
           call run_test(encoding(iencode),1.5,3,epsilon)
        enddo encodeloop
     enddo sizeloop
  enddo suiteloop
  
  if(.not.success) stop 1 ! Exit with non-zero status on failure.
  ! Hit end of program block on success, which should exit with status 0.

  ! ********************************************************************

contains

  ! ********************************************************************

  subroutine test_failed
    if(.not.keep_going) then
       stop 1
    endif
  end subroutine test_failed

  ! ********************************************************************

  subroutine start_test(name,inx,iny,inr,description)
    implicit none
    integer, intent(in) :: inx,iny,inr
    character(len=*), intent(in) :: name,description

    if(allocated(test_name)) deallocate(test_name)
    allocate(character(len=len_trim(name)) :: test_name)
    test_name=trim(name)

    call remake_arrays(inx,iny,inr)

8   format("          ",A,"          ")
    print 8,'----------------------------------------'
10  format(A,': ',A)
    print 10,test_name,trim(description)

  end subroutine start_test

  ! ********************************************************************

  subroutine remake_arrays(inx,iny,inr)
    integer, intent(in) :: inx,iny,inr
    integer :: i,j,r
    if(nx/=inx .or. ny/=iny .or. nr/=inr) then
       if(allocated(infield)) then
          deallocate(infield,outfield,records,recsizes,inmask,outmask)
       endif
       nx=inx
       ny=iny
       nr=inr
       nb=nx*ny*4+20000
       allocate(infield(nx,ny,nr),outfield(nx,ny,nr))
       allocate(inmask(nx,ny,nr),outmask(nx,ny,nr))
       allocate(character(len=nb) :: records(nr))
       allocate(recsizes(nr))
    endif
    !$omp parallel do private(r)
    do r=1,nr
       records(r)=' '
       do j=1,ny
          do i=1,nx
             infield(i,j,r)=-9999
             outfield(i,j,r)=9999
             inmask(i,j,r)=(mod(i+j,2)==0)
             outmask(i,j,r)=.not.inmask(i,j,r)
          enddo
       enddo
    enddo
    !$omp end parallel do
  end subroutine remake_arrays

  ! ********************************************************************

  subroutine run_test(encoding,rmiss,dscale,epsilon)
    implicit none
    integer, intent(in) :: encoding,dscale
    real, intent(in) :: epsilon,rmiss
    integer :: ierr,localierr, localinmask, maxinmask, i,j,r

!     if(encoding==40 .or. encoding==40000) then
!        maxinmask=0
!        !$omp parallel do private(i,j,r,localinmask) reduction(max:maxinmask)
!        do r=1,nr
!           localinmask=0
!           do j=1,ny
!              do i=1,nx
!                 if(inmask(i,j,r)) localinmask=localinmask+1
!              enddo
!           enddo
!           maxinmask=max(maxinmask,localinmask)
!        enddo
!        !$omp end parallel do
!        if(maxinmask>JPEG_MAX_MASK) then
! 100       format(A,': SKIP TEST; ',I0,' points in mask too large for JPEG limit of ',I0)
!           print 100,test_name,maxinmask,JPEG_MAX_MASK
!           return
!        endif
!     endif

    ierr=0
    !$omp parallel do private(r,localierr) reduction(ior:localierr)
    do r=1,nr
       localierr=-999
       call encode_record(r,encoding,rmiss,dscale,localierr)
       ierr=ior(ierr,localierr)
    enddo
    !$omp end parallel do
    if(ierr/=0) then
       success=.false.
       print '(A,": test FAILED; cannot encode records")',test_name
       call test_failed()
       return
    endif

    ierr=0
    !$omp parallel do private(r,localierr) reduction(ior:localierr)
    do r=1,nr
       localierr=-999
       call decode_record(r,localierr)
       ierr=ior(ierr,localierr)
    enddo
    !$omp end parallel do
    if(ierr/=0) then
       success=.false.
       print '(A,": test FAILED; cannot decode records")',test_name
       call test_failed()
    endif

! Test code to test mismatch detection:
!    outfield(10,10,1)=1+10*infield(10,10,1)
!    outmask(12,12,1)=.not.inmask(12,12,1)

    call verify_test(epsilon)
  end subroutine run_test

  ! ********************************************************************

  subroutine encode_record(r,encoding,rmiss,dscale,ierr)
    implicit none
    interface
      subroutine addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,ipdstmplen, &
                          coordlist,numcoord,idrsnum,idrstmpl,      &
                          idrstmplen,fld,ngrdpts,ibmap,bmap,ierr)
        implicit none
        character(len=1),intent(inout) :: cgrib(lcgrib)
        integer,intent(in) :: ipdsnum,ipdstmpl(*)
        integer,intent(in) :: idrsnum,numcoord,ipdstmplen,idrstmplen
        integer,intent(in) :: lcgrib,ngrdpts,ibmap
        real,intent(in) :: coordlist(numcoord)
        real,target,intent(in) :: fld(ngrdpts)
        integer,intent(out) :: ierr
        integer,intent(inout) :: idrstmpl(*)
        logical*1,intent(in) :: bmap(ngrdpts)
      end subroutine addfield
    end interface

    integer, intent(inout) :: ierr
    real, intent(in) :: rmiss 
    integer, intent(in) :: encoding,dscale

    real :: rdummy(2)
    integer :: listsec0(3),listsec1(20),ipdst(200),ipdsnum
    integer :: i,j,igds(200),igdst(200),idrsnum,idrs(200)
    integer :: idefnum,ibmap,nmask,localierr,anyierr,lengrib

    integer :: r, ideflist(2000)

    anyierr=0
    ! !$omp parallel do private(listsec0,listsec1,ipdst,ipdsnum,rdummy,&
    ! !$omp&                    igds,igdst,idrsnum,idrs,idefnum,ibmap,nmask,&
    ! !$omp&                    r,localierr,ideflist,idefnum,lengrib)
    ! !$omp&            reduction(ior:anyierr)
    ! do r=1,nr
30  format(A,': encode record ',I0)
    if(verbose>0) print 30,test_name,r

       listsec0=(/0,2,0/)
       listsec1=(/7,0,8,1,1,2016,12,31,18,59,59,0,1,0,0,0,0,0,0,0/)
       ipdst=0
       ipdsnum=8 ! Statistical processing (template 4.8)
       ipdst(3)=2 ! forecast
       ipdst(4)=255 ! unknown/missing generating process
       ipdst(8)=0 ! minutes
       ipdst(1:2)=(/2,9/) ! vertical velocity
       ipdst(10:15)=(/108,-4,0,108,-4,4/) ! 0-400 mbar above ground
       ipdst(16:21)=(/2017,01,12,18,59,59/) ! forecast time
       ipdst(22)=1 ! Number of forecast times
       ipdst(24)=2 ! statistical method: maximum
       ipdst(25)=2            ! Successive times processed have same
                              ! start time of forecast, forecast time is
                              ! incremented.
       ipdst(26)=0            ! Time range units (minutes)
       ipdst(27)=60           ! 60 minutes
       
       igds=0
       igds(2)=nx*ny
       igdst=0
       igdst(8:9)=(/ nx, ny /)
       igdst(12:19)=(/ 20,200,48,-20,240,nint(40.0/(ny-1)),nint(40.0/(nx-1)),128 /)
       
       idrs=0
       idrsnum=encoding/10
       if(idrsnum==3) then
          idrs(6)=1             ! General group split
          idrs(7)=1             ! Missing value management
          call mkieee(rmiss,idrs(8),1) ! Missing value
          idrs(17)=mod(encoding,10) ! Order of secondary differences (1 or 2)
       elseif(idrsnum==4 .or. idrsnum==4000) then
          idrsnum=encoding
          idrs(6)=0
          idrs(7)=255
       endif
       idrs(3)=dscale
       
       recsizes(r)=nb
       call gribcreate(records(r),recsizes(r),listsec0,listsec1,localierr)
       if(localierr/=0) then
          print 300,test_name,'error creating new GRIB2 record',localierr
          anyierr=ior(anyierr,localierr)
          call test_failed
          return
       endif
       
       call addgrid(records(r),recsizes(r),igds,igdst,200,ideflist,&
                    idefnum,localierr)
       if(localierr/=0) then
          print 300,test_name,'error adding grid to GRIB2 record',localierr
          anyierr=ior(anyierr,localierr)
          call test_failed
          return
       endif
       
       ibmap=0
       call addfield(records(r),recsizes(r),ipdsnum,ipdst,200,&
            rdummy,0,idrsnum,idrs,200, &
            infield(:,:,r),nx*ny,ibmap,inmask(:,:,r),localierr)
       if(localierr/=0) then
          print 300,test_name,'error adding field to GRIB2 record',localierr
          anyierr=ior(anyierr,localierr)
          call test_failed
          return
       endif
       
       call gribend(records(r),recsizes(r),lengrib,localierr)
       if(localierr/=0) then
          print 300, test_name,'error ending GRIB2 record',localierr
          anyierr=ior(anyierr,localierr)
          call test_failed
          return
       endif
       recsizes(r)=lengrib
!    enddo

    ierr=anyierr

50  format(A,': encoding succeeded for record ',I0)
    if(verbose>0) print 50,test_name,r

300 format(A,': FAILED: ',A,' ierr=',I0)
  end subroutine encode_record
 
  ! ********************************************************************

  subroutine decode_record(r,ierr)
    implicit none

    interface
       subroutine gribinfo(cgrib,lcgrib,listsec0,listsec1,&
            numlocal,numfields,maxvals,ierr)
         character(len=*),intent(in) :: cgrib
         integer,intent(in) :: lcgrib
         integer,intent(out) :: listsec0(*),listsec1(*),maxvals(7)
         integer,intent(out) :: numlocal,numfields,ierr
       end subroutine gribinfo
    end interface
    interface
      subroutine gettemplates(cgrib,lcgrib,ifldnum,igds,igdstmpl,&
        igdslen,ideflist,idefnum,ipdsnum,ipdstmpl,&
        ipdslen,coordlist,numcoord,ierr)
        character(len=*),intent(in) :: cgrib
        integer,intent(in) :: lcgrib,ifldnum
        integer :: igdslen,ipdslen
        integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
        integer,intent(out) :: ipdsnum,ipdstmpl(*)
        integer,intent(out) :: idefnum,numcoord
        integer,intent(out) :: ierr
        real,intent(out) :: coordlist(*)
      end subroutine gettemplates
    end interface

    interface
      subroutine getfield(cgrib,lcgrib,ifldnum,igds,igdstmpl,igdslen,&
                          ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,&
                          coordlist,numcoord,ndpts,idrsnum,idrstmpl,&
                          idrslen,ibmap,bmap,fld,ierr)
        character(len=*),intent(in) :: cgrib
        integer :: igdslen,ipdslen,idrslen
        integer,intent(in) :: lcgrib,ifldnum
        integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
        integer,intent(out) :: ipdsnum,ipdstmpl(*)
        integer,intent(out) :: idrsnum,idrstmpl(*)
        integer,intent(out) :: ndpts,ibmap,idefnum,numcoord
        integer,intent(out) :: ierr
        logical(kind=1),intent(out) :: bmap(*)
        real,intent(out) :: fld(*),coordlist(*)
      end subroutine getfield
    end interface

    integer,intent(in) :: r

    integer :: listsec0(3), listsec1(20), numlocal, numfields, maxvals(7), ierr

    integer :: igds(20), igdslen,ipdslen,ipdsnum,numcoord
    integer, allocatable :: igdstmpl(:), ipdstmpl(:), ideflist(:), idrstmpl(:)
    real, allocatable :: coordlist(:)
    real, allocatable :: tempbuf(:)
    integer :: ifldnum,ndpts,ibmap,idefnum,idrsnum,idrslen,i,j,o

30  format(A,': decode record ',I0)
    if(verbose>0) print 30,test_name,r

    ifldnum=1

    ierr=-999
    call gribinfo(records(r),recsizes(r),listsec0,listsec1,numlocal,&
         numfields,maxvals,ierr)
    if(ierr/=0) then
       print 40,test_name,'gribinfo',ierr
       success=.false.
       call test_failed
       goto 2000
    endif

    allocate(igdstmpl(maxvals(2)))
    allocate(ideflist(maxvals(3)))
    allocate(ipdstmpl(maxvals(4)))
    allocate(coordlist(maxvals(5)))
    allocate(idrstmpl(maxvals(6)))
    allocate(tempbuf(maxvals(7)))

    ierr=-999
    call gettemplates(records(r),recsizes(r),ifldnum,igds,igdstmpl,&
         igdslen,ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,coordlist,&
         numcoord,ierr)
    if(ierr/=0) then
       print 40,test_name,'gettemplates',ierr
       success=.false.
       call test_failed
       goto 2000
    endif

    ierr=-999
    call getfield(records(r),recsizes(r),ifldnum,igds,igdstmpl,igdslen, &
         ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,coordlist,numcoord, &
         ndpts,idrsnum,idrstmpl,idrslen,ibmap,outmask(:,:,r),tempbuf,ierr)
    if(ierr/=0) then
       print 40,test_name,'getfield',ierr
       success=.false.
       call test_failed
       goto 2000
    endif

    o=0
    do j=1,ny
       do i=1,nx
          if(outmask(i,j,r)) then
             o=o+1
             outfield(i,j,r)=tempbuf(o)
          endif
       enddo
    enddo

50  format(A,': decoding succeeded for record ',I0)
    if(verbose>0) print 50,test_name,r

2000 continue
    deallocate(igdstmpl,ideflist,ipdstmpl,coordlist,idrstmpl,tempbuf)

40  format(A,": FAILED in decode: ",A," status ierr=",I0)
  end subroutine decode_record
 
  ! ********************************************************************

  subroutine verify_test(epsilon)
    integer :: i,j,r,mismatches,mismasks
    real, intent(in) :: epsilon
    mismasks=0 ! mask does not match
    mismatches=0 ! value differs by more than epsilon
10  format(A,': mask mismatch at i=',I0,' j=',I0,' record ',I0,&
           ' in=',L1,' out=',L1)
20  format(A,': at i=',I0,' j=',I0,' record ',I0,', |in-out| = | ',&
           G0.7,' - ',G0.7,' | > epsilon=',G0.7)
30  format(A,': test ',A,'.')
40  format(A,': found ',I0,' mask differences')
50  format(A,': found ',I0,' value differences > ',G0.7)
    !$omp parallel do private(i,j,r)
    !$omp&            reduction(+:mismatches)
    !$omp&            reduction(+:mismasks)
    do r=1,nr
       do j=1,ny
          do i=1,nx
             if(inmask(i,j,r).neqv.outmask(i,j,r)) then
                mismasks=mismasks+1
                if(mismasks<MAX_PRINT) then
                   !$omp critical
                   print 10,test_name,i,j,r,inmask(i,j,r),outmask(i,j,r)
                   !$omp end critical
                endif
             endif
             if(outmask(i,j,r) .and. inmask(i,j,r)) then
                if(abs(infield(i,j,r)-outfield(i,j,r))>epsilon) then
                   mismatches=mismatches+1
                   if(mismatches<MAX_PRINT) then
                      !$omp critical
                      print 20,test_name,i,j,r,infield(i,j,r),outfield(i,j,r),epsilon
                      !$omp end critical
                   endif
                endif
             endif
          enddo
       enddo
    enddo
    !$omp end parallel do
    if(mismatches/=0) then
       print 50,test_name,mismatches,epsilon
       success=.false.
    endif
    if(mismasks/=0) then
       print 40,test_name,mismasks
       success=.false.
    endif
    if(mismasks==0 .and. mismatches==0) then
       print 30,test_name,'SUCCEEDED'
    else
       print 30,test_name,'FAILED'
       call test_failed
    endif
  end subroutine verify_test

  ! ********************************************************************

end program test_g2
