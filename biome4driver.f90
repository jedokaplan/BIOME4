program biome4main

! compile with makefile

use iso_fortran_env
use netcdf
use coordsmod
use netcdfmod, only : handle_err,genoutfile
use parametersmod

implicit none

real(sp), parameter :: p0 = 101325.           ! sea level standard atmospheric pressure (Pa)
real(sp), parameter :: cp =   1004.68506      ! constant-pressure specific heat (J kg-1 K-1)
real(sp), parameter :: T0 =    288.16         ! sea level standard temperature (K)
real(sp), parameter :: g  =      9.80665      ! earth surface gravitational acceleration (m s-1)
real(sp), parameter :: M  =      0.02896968   ! molar mass of dry air (kg mo-1)
real(sp), parameter :: R0 =      8.314462618  ! universal gas constant (J mol-1 K-1)

real(dp),    allocatable, dimension(:)   :: lon
real(dp),    allocatable, dimension(:)   :: lat

real(sp),    allocatable, dimension(:,:)   :: elv
real(sp),    allocatable, dimension(:,:)   :: tcm
real(sp),    allocatable, dimension(:,:)   :: tmin
integer(i2), allocatable, dimension(:,:,:) :: ivar
real(sp),    allocatable, dimension(:,:,:) :: temp
real(sp),    allocatable, dimension(:,:,:) :: prec
real(sp),    allocatable, dimension(:,:,:) :: cldp
real(sp),    allocatable, dimension(:,:,:) :: whc
real(sp),    allocatable, dimension(:,:,:) :: ksat

integer(i2), allocatable, dimension(:,:)   :: biome
integer(i2), allocatable, dimension(:,:)   :: wdom
integer(i2), allocatable, dimension(:,:)   :: gdom
real(sp),    allocatable, dimension(:,:,:) :: npp

! real(sp),    allocatable, dimension(:,:)   :: lai

real(sp) :: co2
real(sp) :: p
real(sp) :: iopt

! I/O variables

integer :: status
integer :: ncid
integer :: dimid
integer :: varid
integer :: xlen
integer :: ylen
integer :: tlen
integer :: llen

integer :: srtx
integer :: srty
integer :: cntx
integer :: cnty
integer :: endx
integer :: endy

real(sp)    :: scale_factor
real(sp)    :: add_offset
integer(i2) :: missing

character(100) :: jobfile
character(100) :: climatefile
character(100) :: soilfile
character(100) :: outfile

character(45) :: coordstring

real(dp), dimension(4) :: boundingbox

integer :: x,y
integer :: i,j

! biome4 arguments; for catalog, see bottom of this code

real(sp), dimension(50)  :: input
real(sp), dimension(500) :: output

integer :: ompchunk
logical :: diag

real(sp), dimension(2) :: rng

namelist / joboptions / climatefile,soilfile,co2

!------------------------------------------------------------------------------------------------------------

call getarg(1,jobfile)

open(10,file=jobfile,status='old')

read(10,nml=joboptions)

close(10)

!-------------------------------------------------------
! input data file size

status = nf90_open(climatefile,nf90_nowrite,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'lon',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=xlen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'lat',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=ylen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'time',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=tlen)
if (status /= nf90_noerr) call handle_err(status)

allocate(lon(xlen))
allocate(lat(ylen))

!-------------------------------------------------------

call getarg(2,coordstring)

if (coordstring == 'alldata') then

  srtx = 1
  srty = 1
  cntx = xlen
  cnty = ylen

else

  call parsecoords(coordstring,boundingbox)
 
  srtx = nint(boundingbox(1))
  srty = nint(boundingbox(3))
  cntx = 1 + nint(boundingbox(2) - boundingbox(1))
  cnty = 1 + nint(boundingbox(4) - boundingbox(3))

end if

endx = srtx + cntx - 1
endy = srty + cnty - 1
 
write(0,*)srtx,srty,cntx,cnty

allocate(elv(cntx,cnty))
allocate(tmin(cntx,cnty))

allocate(ivar(cntx,cnty,tlen))
allocate(temp(cntx,cnty,tlen))
allocate(prec(cntx,cnty,tlen))
allocate(cldp(cntx,cnty,tlen))

!-------------------------------------------------------
! elevation

print *, 'Inquiring elevation variable'
status = nf90_inq_varid(ncid, 'elv', varid)
if (status == nf90_noerr) then

  print *, 'Reading elevation variable'
  status = nf90_get_var(ncid, varid, elv, start=[srtx, srty,1], count=[cntx, cnty])
  if (status /= nf90_noerr) call handle_err(status)
  print *, 'Read elevation variable'

else

  elv = 0.
  print *, 'Elevation variable not found, set to 0'

end if

!-------------------------------------------------------
! temperature

temp = -9999.

status = nf90_inq_varid(ncid,'tmp',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,ivar,start=[srtx,srty,1],count=[cntx,cnty,tlen])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'missing_value',missing)
if (status /= nf90_noerr) call handle_err(status)

where (ivar /= missing)
  temp = real(ivar) * scale_factor + add_offset
end where

!-------------------------------------------------------
! precipitation

prec = -9999.

status = nf90_inq_varid(ncid,'pre',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,ivar,start=[srtx,srty,1],count=[cntx,cnty,tlen])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'missing_value',missing)
if (status /= nf90_noerr) call handle_err(status)

where (ivar /= missing)
  prec = real(ivar) * scale_factor + add_offset
end where

!-------------------------------------------------------
! cloud percent

cldp = -9999.

status = nf90_inq_varid(ncid,'cld',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,ivar,start=[srtx,srty,1],count=[cntx,cnty,tlen])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'missing_value',missing)
if (status /= nf90_noerr) call handle_err(status)

where (ivar /= missing)
  cldp = real(ivar) * scale_factor + add_offset
end where

!-------------------------------------------------------
! absolute minimum temperature

tmin = -9999.

status = nf90_inq_varid(ncid,'cld',varid)
if (status == nf90_noerr) then ! tmin is present, we will read it from the file 

  status = nf90_get_var(ncid,varid,ivar(:,:,1),start=[srtx,srty,1],count=[cntx,cnty])
  if (status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid,varid,'scale_factor',scale_factor)
  if (status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid,varid,'add_offset',add_offset)
  if (status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid,varid,'missing_value',missing)
  if (status /= nf90_noerr) call handle_err(status)

  where (ivar(:,:,1) /= missing)
    tmin = real(ivar(:,:,1)) * scale_factor + add_offset
  end where

else ! tmin is not present in the input, we will estimate it base on temperature

  allocate(tcm(cntx,cnty))

  tcm = minval(temp,dim=3)

  where (tcm /= -9999.)

    tmin = 0.006 * tcm**2 + 1.316 * tcm - 21.9

  end where

  deallocate(tcm)

end if

!-------------------------------------------------------

status = nf90_close(ncid)

!-------------------------------------------------------

status = nf90_open(soilfile,nf90_nowrite,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'layer',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=llen)
if (status /= nf90_noerr) call handle_err(status)

allocate(whc(cntx,cnty,llen))
allocate(ksat(cntx,cnty,llen))

status = nf90_inq_varid(ncid,'whc',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,whc,start=[srtx,srty,1],count=[cntx,cnty,llen])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ncid,'perc',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,ksat,start=[srtx,srty,1],count=[cntx,cnty,llen])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

!-------------------------------------------------------

call getarg(3,outfile)

call genoutfile(outfile,cntx,cnty,ncid)

if (trim(outfile) == 'diag.nc') then
  diag = .true. ! iopt
else
  diag = .false.  ! iopt
end if

!-------------------------------------------------------

allocate(biome(cntx,cnty))
allocate(wdom(cntx,cnty))
allocate(gdom(cntx,cnty))
allocate(npp(cntx,cnty,13))

biome = missing
wdom  = missing
gdom  = missing
npp   = -9999.

! lai = -9999.

do y = 1,cnty

  write(0,*)' working on row ',y,'out of ',cnty

  ompchunk = 4 !min(8,sblock_out(2))

  !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(p,input,output)
  !$OMP DO SCHEDULE(DYNAMIC,ompchunk)

  do x = 1,cntx
  
    if (temp(x,y,1) == -9999.) cycle

    p = p0 * (1. - (g * elv(x,y)) / (cp * T0))**(cp * M / R0)
  
    input(1)     = lat(y)
    input(2)     = co2
    input(3)     = p
    input(4)     = tmin(x,y)
    input(5:16)  = temp(x,y,:)
    input(17:27) = prec(x,y,:)
    input(28:40) = cldp(x,y,:)
    input(41)    = sum(Ksat(x,y,1:3)) / 3.
    input(42)    = sum(Ksat(x,y,4:6)) / 3.
    input(43)    = sum(whc(x,y,1:3))
    input(44)    = sum(whc(x,y,4:6))
    input(49)    = lon(x)
    
    if (diag) then
      input(46) = 1.  ! diagnostic mode on
    else
      input(46) = 0.  ! diagnostic mode off
    end if

    call biome4(input,output)
        
    biome(x,y) = nint(output(1))
    wdom(x,y)  = nint(output(12))
    gdom(x,y)  = nint(output(13))
    npp(x,y,:) = output(60:72)
    
  end do

  !$OMP END DO NOWAIT
  !$OMP END PARALLEL

end do

!-------------------------------------------------------

write(0,*)'writing'

status = nf90_inq_varid(ncid,'lon',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,lon(srtx:endx))
if (status /= nf90_noerr) call handle_err(status)

rng = [minval(lon(srtx:endx)),maxval(lon(srtx:endx))]

status = nf90_put_att(ncid,varid,'actual_range',rng)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'lat',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,lat(srty:endy))
if (status /= nf90_noerr) call handle_err(status)

rng = [minval(lat(srty:endy)),maxval(lat(srty:endy))]

status = nf90_put_att(ncid,varid,'actual_range',rng)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'pft',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,[(i,i=1,13)])
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'biome',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,biome)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'wdom',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,wdom)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'gdom',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,gdom)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_inq_varid(ncid,'npp',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ncid,varid,npp)
if (status /= nf90_noerr) call handle_err(status)

! ---

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

!------------------------------------------------------------------------------------------------------------
! catalog of arguments 
!---------------------------------------
! input: real, dimension(50) :: vars_in(50)
!---------------------------------------
! 1        latitude
! 2        co2
! 3        air pressure
! 4        tmin
! 5-16     temperature (degC)
! 17-27    precip (mm)
! 28-40    cloud (percent)
! 41-44    soil: 1-2 = Ksat (mm/h); 3-4 = volumetric whc (mm); NB k(6) does not appear to be actually used
! 45       not used
! 46       iopt (diagnostic mode: 1 = on, anything else = off)
! 47-48    not used
! 49       longitude
! 50       not used
!
!---------------------------------------
! output: real, dimension(500) :: output
!---------------------------------------
! 1        biome
! 2        lai
! 3        npp
! 4        woody LAI
! 5        woody NPP
! 6        grass LAI
! 7        grass NPP
! 8        APAR
! 9        autotrophic resp
! 10       soil moisture (not used)
! 11       runoff (dominant PFT)
! 12       dominant woody PFT
! 13       annual precip
! 14       PAR (calculated)
! 15       LAI ratio
! 16       npp difference (nppdiff)
! 17       FVC half of LAI (foliar vegetative cover)
! 18       root distribuion
! 19-24    not used
! 25-36    monthly FPAR
! 37-48    monthly NPP for dominant PFT
! 48       longitude (NB will be overwritten by above)
! 49       latitude
! 50       deltaA for dominant PFT (13C)
! 51       mean deltaA
! 52       phi for C4 plants
! 53-59    not used
! 60-72    NPP by PFT
! 73-79    not used
! 80-91    monthly Delta13C dominant PFT
! 92-96    not used
! 97       mean annual hetresp scalar
! 98       percent of NPP that is C4
! 99       annual het resp
! 100      not used
! 101-112  monthly DeltaE, dominant PFT
! 113-124  monthly het resp, dominant PFT
! 125-136  monthly isoresp (product)
! 137-148  monhtly net C flux, dominant PFT (npp-resp)
! 149      annual NEP
! 150      annual mean A/g
! 151-159  not used
! 160-172  monthly mean gc, dominant PFT
! 173-184  monthly LAI, dominant PFT
! 185-196  monthly runoff
! 197-198  not used
! 199      fire days
! 200      green days
! 201-241  ten day LAI, dominant PFT
! 242-300  not used
! 301-314  npp by pft
! 315-328  lai by pft
! 329-388  not used
! 389-400  mean soil moisture, dominant PFT
! 401-413  not used
! 413-424  monthly soil moisture, top layer, dominant PFT
! 425-436  monthly soil moisture, bottom layer, dominant PFT
! 425      soil moisture top layer, dominant PFT (NB will overwrite above)
! 426      soil moisture bottom layer, dominant PFT
! 427      soil moisture ratio, dominant (PFT)
! 437-449  not used
! 450      mean Klit
! 451      mean Ksoil
! 452      temperature of the coldest month
! 453      gdd0
! 454      gdd5
! 455-500  not used
!------------------------------------------------------------------------------------------------------------

end program biome4main