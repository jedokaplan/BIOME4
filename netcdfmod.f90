module netcdfmod

use netcdf
use parametersmod

implicit none

public :: handle_err
public :: genoutfile

contains

! -------------------------------------------------------

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

! -------------------------------------------------------

subroutine genoutfile(jobfile,outfile,xlen,ylen,ofid)

implicit none

character(*), intent(in)  :: jobfile
character(*), intent(in)  :: outfile
integer,      intent(in)  :: xlen
integer,      intent(in)  :: ylen
integer,      intent(out) :: ofid

integer(i2), parameter :: missing = -32768

integer, dimension(4) :: dimids
integer, dimension(4) :: chunks

character(8)  :: today
character(10) :: now

integer :: ncstat
integer :: dimid
integer :: varid

real(dp), dimension(2) :: xrange = [0.,0.]
real(dp), dimension(2) :: yrange = [0.,0.]

! ----------------------
! general data

call date_and_time(today,now)

ncstat = nf90_create(outfile,nf90_hdf5,ofid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'title','BIOME4 netCDF output file')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

call date_and_time(today,now)

ncstat = nf90_put_att(ofid,nf90_global,'timestamp',today//' '//now(1:4))
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'Jobfile',jobfile)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'Conventions','COARDS')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,nf90_global,'node_offset',1)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

! ----
! coordinate lon

ncstat = nf90_def_dim(ofid,'lon',xlen,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'lon',nf90_double,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','longitude')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','degrees_east')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',xrange)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(1) = dimid

! ----
! coordinate lat

ncstat = nf90_def_dim(ofid,'lat',ylen,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'lat',nf90_double,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','latitude')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','degrees_north')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',yrange)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(2) = dimid

! ----
! coordinate PFT

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

! ----
! coordinate month

ncstat = nf90_def_dim(ofid,'month',12,dimid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_def_var(ofid,'month',nf90_int,dimid,varid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','month')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','month')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'actual_range',[1,12])
if (ncstat/=nf90_noerr) call handle_err(ncstat)

dimids(4) = dimid

! ----

chunks(1) = min(xlen,200)
chunks(2) = min(ylen,200)
chunks(3) = 1
chunks(4) = 1

! ----

ncstat = nf90_def_var(ofid,'biome',nf90_short,dimids(1:2),varid,chunksizes=chunks(1:2),deflate_level=1,shuffle=.true.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','biome')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','biome')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

! ----

ncstat = nf90_def_var(ofid,'wdom',nf90_short,dimids(1:2),varid,chunksizes=chunks(1:2),deflate_level=1,shuffle=.true.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','dominant tree pft')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','PFT')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',missing)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

! ----

ncstat = nf90_def_var(ofid,'NPP',nf90_float,dimids(1:3),varid,chunksizes=chunks(1:3),deflate_level=1,shuffle=.true.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','Annual total Net Primary Productivity of the dominant PFT')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','g m-2')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

! ----

ncstat = nf90_def_var(ofid,'LAI',nf90_float,dimids(1:3),varid,chunksizes=chunks(1:3),deflate_level=1,shuffle=.true.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'long_name','Maximum annual Leaf Area Index of the dominant PFT')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'units','m2 m-2')
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'_FillValue',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

ncstat = nf90_put_att(ofid,varid,'missing_value',-9999.)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

! ----

ncstat = nf90_enddef(ofid)
if (ncstat/=nf90_noerr) call handle_err(ncstat)

end subroutine genoutfile

! -------------------------------------------------------

end module netcdfmod