program dump_values
  implicit none
  integer :: iarg,nargs,arglen,unit
  character(len=:), allocatable :: carg
  character(len=:), allocatable :: cgrib
  integer :: lcgrib,ierr 

  double precision :: dsum
  real :: rmin,rmax,rmean

  real, allocatable :: outfield(:)
  logical(kind=1), allocatable :: outmask(:)
  integer :: ndata,j

  nargs=command_argument_count()

  if(nargs<1) then
1    format(A)
     print 1,'Syntax: dump_values file1 [file2 [...]]'
     print 1,'Dumps the values of the first field of the first GRIB'
     print 1,'record in each file, in text.'
  end if

  do iarg=1,nargs

     ! Get filename
     call get_command_argument(iarg,length=arglen)
     if(allocated(carg)) deallocate(carg)
     allocate(character(len=arglen) :: carg)
     call get_command_argument(iarg,carg)
     print '(A,": read file")',carg

     ! Read entire file
     open(newunit=unit,file=carg,status='old',access='stream')
     inquire(unit,size=lcgrib)
     if(allocated(cgrib)) deallocate(cgrib)
     allocate(character(len=lcgrib) :: cgrib)
     read(unit,pos=1) cgrib
     close(unit)

     ! Decode first field of first GRIB2 record
     ndata=-999
     if(allocated(outfield)) deallocate(outfield)
     if(allocated(outmask)) deallocate(outmask)
     call decode_record(cgrib,lcgrib,outfield,outmask,ndata,ierr)
     if(ierr/=0 .or. ndata<1) then
        write(0,*) 'ABORT: ierr=',ierr,' ndata=',ndata
        stop 2
     endif

     ! Get stats on GRIB2 file.
     rmin=outfield(1)
     rmax=outfield(1)
     dsum=0
     do j=1,ndata
        dsum=dsum+outfield(j)
        rmin=min(rmin,outfield(j))
        rmax=max(rmax,outfield(j))
     enddo

10   format(A,' = ',G0.7)
20   format(A,' = ',I0)

     print 10,'min',rmin
     print 10,'max',rmax
     print 10,'mean',dsum/ndata
     print 20,'count',ndata
  enddo

contains

  subroutine decode_record(cgrib,lcgrib,outfield,outmask,ndata,ierr)
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

    integer, intent(in) :: lcgrib
    character(len=lcgrib), intent(inout) :: cgrib
    integer, intent(out) :: ndata
    real, allocatable, intent(inout) :: outfield(:)
    logical(kind=1), allocatable, intent(inout) :: outmask(:)

    integer :: listsec0(3), listsec1(20), numlocal, numfields, maxvals(7), ierr

    integer :: igds(20), igdslen,ipdslen,ipdsnum,numcoord
    integer, allocatable :: igdstmpl(:), ipdstmpl(:), ideflist(:), idrstmpl(:)
    real, allocatable :: coordlist(:)
    real, allocatable :: tempbuf(:)
    integer :: ifldnum,ndpts,ibmap,idefnum,idrsnum,idrslen,i,j,o

    ifldnum=1

    ierr=-999
    call gribinfo(cgrib,lcgrib,listsec0,listsec1,numlocal,&
         numfields,maxvals,ierr)
    if(ierr/=0) then
       print 40,'gribinfo',ierr
       goto 2000
    endif

    if(allocated(outfield)) deallocate(outfield)
    if(allocated(outmask)) deallocate(outmask)

    allocate(igdstmpl(maxvals(2)))
    allocate(ideflist(maxvals(3)))
    allocate(ipdstmpl(maxvals(4)))
    allocate(coordlist(maxvals(5)))
    allocate(idrstmpl(maxvals(6)))
    allocate(tempbuf(maxvals(7)))
    allocate(outfield(maxvals(7)))
    allocate(outmask(maxvals(7)))

    ierr=-999
    call gettemplates(cgrib,lcgrib,ifldnum,igds,igdstmpl,&
         igdslen,ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,coordlist,&
         numcoord,ierr)
    if(ierr/=0) then
       print 40,'gettemplates',ierr
       goto 2000
    endif

    ierr=-999
    call getfield(cgrib,lcgrib,ifldnum,igds,igdstmpl,igdslen, &
         ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,coordlist,numcoord, &
         ndpts,idrsnum,idrstmpl,idrslen,ibmap,outmask(:),tempbuf,ierr)
    if(ierr/=0) then
       print 40,'getfield',ierr
       goto 2000
    endif

    o=0
    do j=1,maxvals(7)
       if(outmask(j)) then
          o=o+1
          outfield(j)=tempbuf(o)
       endif
    enddo

    ndata=maxvals(7)

2000 continue ! Exception handling section
    deallocate(igdstmpl,ideflist,ipdstmpl,coordlist,idrstmpl,tempbuf)

40  format("FAILED in decode: ",A," status ierr=",I0)
  end subroutine decode_record

end program dump_values
