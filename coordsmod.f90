module coordsmod

use parametersmod

implicit none

public  :: parsecoords
public  :: calcpixels
private :: nearest

contains

!-----------------------------------------------------------------------------------------------

subroutine parsecoords(coordstring,bb)

!subroutine to parse a coordinate string

implicit none

character(*),           intent(in)  :: coordstring  ! GMT-style coordinate string (separated by "/")
real(dp), dimension(4), intent(out) :: bb           !bounding box in real numbers

character(10), dimension(4) :: cval = '0'

integer :: i
integer :: lasti = 1
integer :: part  = 1

do i=1,len_trim(coordstring)
  if (coordstring(i:i) == '/') then
    cval(part) = coordstring(lasti:i-1)
    lasti=i+1
    part = part + 1
  end if
end do

cval(part) = coordstring(lasti:i-1)

read(cval,*)bb

if (part < 4) then
  bb(3)=bb(2)
  bb(4)=bb(3)
  bb(2)=bb(1)
end if

end subroutine parsecoords

!-----------------------------------------------------------------------------------------------

subroutine calcpixels(lon,lat,bb,srtx,srty,cntx,cnty)

implicit none

real(dp), dimension(:), intent(in) :: lon
real(dp), dimension(:), intent(in) :: lat
real(dp), dimension(4), intent(in) :: bb

integer, intent(out) :: srtx
integer, intent(out) :: srty
integer, intent(out) :: cntx
integer, intent(out) :: cnty

!local variables

integer :: endx
integer :: endy

!-------------------------

srtx = nearest(bb(1),lon)
srty = nearest(bb(2),lat)
endx = nearest(bb(3),lon)
endx = nearest(bb(4),lat)

cntx = max(endx - srtx,1)
cnty = max(endy - srty,1)

end subroutine calcpixels

!-----------------------------------------------------------------------------------------------

integer function nearest(val,vect)

real(dp) :: val
real(dp), dimension(:) :: vect

integer, dimension(1) :: pos

pos = minloc(abs(val-vect))

nearest = pos(1)

end function nearest

!-----------------------------------------------------------------------------------------------

end module coordsmod
