module coordsmod

use parametersmod

implicit none

public  :: parsecoords
public  :: calcpixels
private :: nearest_coord
private :: include_coord

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


! srtx = nearest_coord(bb(1),lon)
! srty = nearest_coord(bb(2),lat)
! endx = nearest_coord(bb(3),lon)
! endy = nearest_coord(bb(4),lat)

srtx = include_coord(bb(1),lon, .true.)
srty = include_coord(bb(2),lat, .true.)
endx = include_coord(bb(3),lon, .false.)
endy = include_coord(bb(4),lat, .false.)

cntx = max(endx - srtx,1)
cnty = max(endy - srty,1)

end subroutine calcpixels

!-----------------------------------------------------------------------------------------------

integer function nearest_coord(val,vect)

real(dp) :: val
real(dp), dimension(:) :: vect

integer, dimension(1) :: pos

pos = minloc(abs(val-vect))

nearest_coord = pos(1)

end function nearest_coord

!-----------------------------------------------------------------------------------------------

integer function include_coord(val,vect,below)

real(dp) :: val
real(dp), dimension(:) :: vect
logical :: below

integer :: pos
integer :: size_vect

size_vect = size(vect)

if (below) then
  do pos = 1, size_vect
    if (vect(pos).ge.val) then
      include_coord = max(pos - 1, 1)
      exit
    end if
  end do
else
  do pos = size(vect), 1, -1
    if (vect(pos).le.val) then
      include_coord = min(pos + 1, size_vect) 
      exit
    end if
  end do
end if

end function include_coord

!-----------------------------------------------------------------------------------------------

end module coordsmod
