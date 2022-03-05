module netcdfmod

use netcdf
use parametersmod

implicit none

public :: handle_err
public :: genoutfile

contains

!-------------------------------------------------------

subroutine handle_err(status)

implicit none

!   Internal subroutine - checks error ncstat after each netcdf call,
!   prints out text message each time an error code is returned. 

integer, intent (in) :: status

if(status /= nf90_noerr) then 
  print *, trim(nf90_strerror(status))
  stop
end if

end subroutine handle_err

!-------------------------------------------------------

subroutine genoutfile(filename,xlen,ylen,ofid)

implicit none

character(*), intent(in)  :: filename
integer,      intent(in)  :: xlen
integer,      intent(in)  :: ylen
integer,      intent(out) :: ofid

integer(i2), parameter :: missing = -32768

integer, dimension(3) :: dimids
integer, dimension(3) :: chunks

character(8)  :: today
character(10) :: now

integer :: ncstat
integer :: dimid
integer :: varid

real(dp), dimension(2) :: xrange = [0.,0.]
real(dp), dimension(2) :: yrange = [0.,0.]

!----------------------

call date_and_time(today,now)

ncstat = nf90_create(filename,nf90_hdf5,ofid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'title','BIOME4 netCDF output file')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

call date_and_time(today,now)

ncstat = nf90_put_att(ofid,nf90_global,'timestamp',today//' '//now(1:4))
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'Conventions','COARDS')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'node_offset',1)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

ncstat = nf90_def_dim(ofid,'lon',xlen,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'lon',nf90_float,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','longitude')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','degrees_east')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',xrange)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(1) = dimid

!----

ncstat = nf90_def_dim(ofid,'lat',ylen,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'lat',nf90_float,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','latitude')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','degrees_north')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',yrange)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(2) = dimid

!----

ncstat = nf90_def_dim(ofid,'pft',13,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'pft',nf90_int,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','plant functional type')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','type')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',[1,13])
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(3) = dimid

!----

chunks(1) = min(xlen,200)
chunks(2) = min(ylen,200)
chunks(3) = 1

!----

ncstat = nf90_def_var(ofid,'biome',nf90_short,dimids(1:2),varid,chunksizes=chunks(1:2),deflate_level=1,shuffle=.false.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','biome')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','biome')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

ncstat = nf90_def_var(ofid,'wdom',nf90_short,dimids(1:2),varid,chunksizes=chunks(1:2),deflate_level=1,shuffle=.false.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','dominant tree pft')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','PFT')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

ncstat = nf90_def_var(ofid,'gdom',nf90_short,dimids(1:2),varid,chunksizes=chunks(1:2),deflate_level=1,shuffle=.false.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','dominant non-tree pft')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','PFT')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

ncstat = nf90_def_var(ofid,'npp',nf90_float,dimids,varid,chunksizes=chunks,deflate_level=1,shuffle=.false.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','net primary productivity')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','g m-2')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

! ncstat = nf90_def_var(ofid,'lai',nf90_float,dimids,varid,chunksizes=chunks,deflate_level=1,shuffle=.false.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'long_name','LAI')
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'units','m2 m-2')
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'_FillValue',-9999.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'missing_value',-9999.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

! ncstat = nf90_def_var(ofid,'npp',nf90_float,dimids,varid,chunksizes=chunks,deflate_level=1,shuffle=.false.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'long_name','NPP')
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'units','gC m-2')
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'_FillValue',-9999.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)
! 
! ncstat = nf90_put_att(ofid,varid,'missing_value',-9999.)
! if (ncstat/=nf90_noerr) call handle_err(ncstat)

!----

ncstat = nf90_enddef(ofid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

end subroutine genoutfile

!-------------------------------------------------------

end module netcdfmod