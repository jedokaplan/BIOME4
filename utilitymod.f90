module utilitymod

implicit none

public :: pos
public :: matsol
public :: roundto
public :: tridiag
public :: cubspline
public :: angleinterp
public :: overprint

interface pos
  module procedure pos_sp
  module procedure pos_dp
end interface pos

interface roundto
  module procedure roundto_s
  module procedure roundto_v
end interface

interface tridiag
  module procedure tridiag_sp
  module procedure tridiag_dp
end interface tridiag

interface spline
  module procedure spline_sp
  module procedure spline_dp
end interface spline

interface cubspline
  module procedure cubspline_sp
  module procedure cubspline_dp
end interface cubspline

interface angleinterp
  module procedure angleinterp_sp
  module procedure angleinterp_dp
end interface angleinterp

contains

! ---------------------------------------------------------

integer function iminloc(vect)

use parametersmod, only : sp

implicit none

real(sp), dimension(:), intent(in) :: vect

! ---

iminloc = minloc(vect,dim=1)

end function iminloc

! ---------------------------------------------------------

integer function imaxloc(vect)

use parametersmod, only : sp

implicit none

real(sp), dimension(:), intent(in) :: vect

! ---

imaxloc = maxloc(vect,dim=1)

end function imaxloc

! ---------------------------------------------------------

integer function pos_sp(vect,val)

! finds the index in the vector "vect" that has a value nearest to the input scalar "val"

use parametersmod, only : sp

implicit none

! arguments

real(sp), dimension(:), intent(in) :: vect
real(sp),               intent(in) :: val

! ---

pos_sp = minloc(abs(vect - val),dim=1)

end function pos_sp

! ---------------------------------------------------------

integer function pos_dp(vect,val)

! finds the index in the vector "vect" that has a value nearest to the input scalar "val"

use parametersmod, only : dp

implicit none

! arguments

real(dp), dimension(:), intent(in) :: vect
real(dp),               intent(in) :: val

! ---

pos_dp = minloc(abs(vect - val),dim=1)

end function pos_dp

! ---------------------------------------------------------

subroutine matsol(mat,sol)

! Provides matrix solution to X matrix in a A * X = B system using LU decomposition
! Code adapted from Press et al. (1996) 
! Numerical Recipes in Fortran 90: The Art of Parallel Scientific Computing 2nd Edition (P.1016-1017)
! Coded by Leo Lai ca. 2021

use parametersmod, only : sp

implicit none

! arguments

real(sp), dimension(:,:), intent(inout) :: mat
real(sp), dimension(:),   intent(inout) :: sol

! parameter

real(sp), parameter :: tiny_sp = tiny(1._sp)

! local variables

integer,  dimension(size(sol)) :: indx
real(sp), dimension(size(sol)) :: mv

real(sp), allocatable, dimension(:,:) :: prod
integer,               dimension(1)   :: maxl

integer  :: i
integer  :: n
integer  :: k
integer  :: ll
integer  :: max
real(sp) :: summ

! -------------------

n = size(sol)

mv = 1. / maxval(abs(mat), dim=2)

!---

do i = 1, n

  maxl = maxloc(mv(i:n) * abs(mat(i:n,i)))

  max = (i - 1) + maxl(1)

  indx(i) = max

  !---

  if (mat(i,i) == 0.) mat(i,i) = tiny_sp

  mat(i+1:n, i) = mat(i+1:n, i) / mat(i,i)

  !---

  allocate(prod(i+1:n, i+1:n))

  prod = spread(mat(i+1:n, i), dim=2, ncopies=size(mat(i, i+1:n)))

  prod = prod * spread(mat(i, i+1:n), dim=1, ncopies=size(mat(i+1:n, i)))

  !---

  mat(i+1:n, i+1:n) = mat(i+1:n, i+1:n) - prod

  deallocate(prod)

end do

!---

k = 0

do i = 1, n

  ll = indx(i)
  summ = sol(ll)
  sol(ll) = sol(i)

  if (k /= 0) then

    summ = summ - dot_product(mat(i, k:i-1), sol(k:i-1))

  else if (summ /= 0.) then

    k = i

  end if

  sol(i) = summ

end do

!---

do i = n, 1, -1

  sol(i) = (sol(i) - dot_product(mat(i, i+1:n), sol(i+1:n))) / mat(i,i)

end do

end subroutine matsol

! -----------------------------------------------------------------------

function roundto_s(val,decimals)

! round a scalar value to a given number of decimal places

use parametersmod, only : sp

implicit none

real(sp), intent(in) :: val       ! the input value
integer,  intent(in) :: decimals  ! the decimal precision

real(sp) :: roundto_s

real(sp) :: scale

! ----

scale = 10.**decimals

roundto_s = real(nint(val * scale)) / scale

end function roundto_s

! ---------------------------------------------------------

function roundto_v(val,decimals)

! round a vector of values to a given number of decimal places

use parametersmod, only : sp

implicit none

real(sp), dimension(:), intent(in) :: val       ! the input vector
integer,                intent(in) :: decimals  ! the decimal precision

real(sp), dimension(size(val)) :: roundto_v

real(sp) :: scale

! ----

scale = 10.**decimals

roundto_v = real(nint(val * scale)) / scale

end function roundto_v

! ---------------------------------------------------------

subroutine tridiag_sp(a,b,c,r,u)

! linear solver for tridiagonal matrices
! based on Numerical Recipes

use parametersmod, only : sp

implicit none

! arguments

real(sp), dimension(:), intent(in)  :: a  ! left-side off-diagonal vector, size b(n-1)
real(sp), dimension(:), intent(in)  :: b  ! diagonal vector
real(sp), dimension(:), intent(in)  :: c  ! right-side off-diagonal vector, size b(n-1)
real(sp), dimension(:), intent(in)  :: r  ! right-hand side, size b(n)
real(sp), dimension(:), intent(out) :: u  ! result vector, size b(n)

! local variables

real(sp), dimension(size(b)) :: gam

integer  :: n
integer  :: j
real(sp) :: bet

! ----
! setup

n = size(b)

bet = b(1)

if (bet == 0.) then
  write(0,*)'error tridiag: first element of diagonal vector cannot be zero'
  stop
end if

u(1) = r(1) / bet

! decomposition and forward substitution step

do j = 2,n  

  gam(j) = c(j-1) / bet

  bet = b(j) - a(j-1) * gam(j)

  if (bet == 0.) then
    write(0,*)'error tridiag: beta = 0'
    stop
  end if

  u(j) = (r(j) - a(j-1) * u(j-1)) / bet
    
end do

! backsubstitution step

do j = n-1,1,-1

  u(j) = u(j) - gam(j+1) * u(j+1)

end do

end subroutine tridiag_sp

! ---------------------------------------------------------

subroutine spline_sp(x,y,yp1,ypn,y2)

! create a spline object for interpolation
! based on Numerical Recipes

use parametersmod, only : sp

implicit none

! arguments

real(sp), dimension(:), intent(in)  :: x    ! vector of independent variable
real(sp), dimension(:), intent(in)  :: y    ! vector of dependent variable
real(sp),               intent(in)  :: yp1  ! first derivative of y at y(1)
real(sp),               intent(in)  :: ypn  ! first derivative of y at y(n)
real(sp), dimension(:), intent(out) :: y2   ! second derivatives of y at x

! parameter

real(sp), parameter :: ypmax = 1.e30

! local variables
integer :: n

real(sp), dimension(size(x)) :: a  ! tridiagonal vectors
real(sp), dimension(size(x)) :: b
real(sp), dimension(size(x)) :: c
real(sp), dimension(size(x)) :: r

! ----

n = size(x)

c(1:n-1) = x(2:n) - x(1:n-1)  ! vector of interval distances

r(1:n-1) = 6. * ((y(2:n) - y(1:n-1)) / c(1:n-1))  ! vector of first derivatives

r(2:n-1) = r(2:n-1) - r(1:n-2)  ! vector of second derivatives

a(2:n-1) = c(1:n-2)  ! off-axis diagonals

b(2:n-1) = 2. * (c(2:n-1) + a(2:n-1))

b(1) = 1.  ! endpoints of the central diagonal
b(n) = 1. 

! set the lower boundary condition for first derivative (upper off-diagonal vector c)

if (yp1 >= ypmax) then
  r(1) = 0.
  c(1) = 0.
else
  r(1) = (3. / (x(2) - x(1))) * ((y(2) - y(1)) / (x(2) - x(1)) - yp1)  
  c(1) = 0.5
end if

! set the upper boundary condition for the first derivative (lower off-diagonal vector a)

if (ypn >= ypmax) then
  r(n) = 0.
  a(n) = 0.
else
  r(n) = (-3. / (x(n) - x(n-1))) * ((y(n) - y(n-1)) / (x(n) - x(n-1)) - ypn)
  a(n) = 0.5
end if

call tridiag(a(2:n),b(1:n),c(1:n-1),r(1:n),y2(1:n))

end subroutine spline_sp

! ---------------------------------------------------------

real(sp) function cubspline_sp(xa,ya,y2a,x)

! evaluate a spline object y2a at a location given by x
! based on Numerical Recipes

use parametersmod, only : sp

implicit none

! arguments

real(sp), dimension(:), intent(in) :: xa
real(sp), dimension(:), intent(in) :: ya
real(sp), dimension(:), intent(in) :: y2a
real(sp),               intent(in) :: x

! local variables

integer  :: nrst
integer  :: khi
integer  :: klo
integer  :: n
real(sp) :: a
real(sp) :: b
real(sp) :: h

! ----

n = size(xa) 

! find the values in xa bracketing x

nrst = pos(xa,x)

if (x < xa(nrst)) then
  klo = nrst-1
  khi = nrst
else
  klo = nrst
  khi = nrst+1
end if

h = xa(khi) - xa(klo)

if (h == 0.) then
  write(0,*)'error cubspline: interval = 0'
  stop
end if

a = (xa(khi) - x) / h
b = (x - xa(klo)) / h

cubspline_sp = a * ya(klo) + b * ya(khi) + ((a**3 - a) * y2a(klo) + (b**3 - b) * y2a(khi)) * (h**2) / 6.

end function cubspline_sp

! ---------------------------------------------------------

subroutine tridiag_dp(a,b,c,r,u)

! linear solver for tridiagonal matrices
! based on Numerical Recipes

use parametersmod, only : dp

implicit none

! arguments

real(dp), dimension(:), intent(in)  :: a  ! left-side off-diagonal vector, size b(n-1)
real(dp), dimension(:), intent(in)  :: b  ! diagonal vector
real(dp), dimension(:), intent(in)  :: c  ! right-side off-diagonal vector, size b(n-1)
real(dp), dimension(:), intent(in)  :: r  ! right-hand side, size b(n)
real(dp), dimension(:), intent(out) :: u  ! result vector, size b(n)

! local variables

real(dp), dimension(size(b)) :: gam

integer  :: n
integer  :: j
real(dp) :: bet

! ----
! setup

n = size(b)

bet = b(1)

if (bet == 0.) then
  write(0,*)'error tridiag: first element of diagonal vector cannot be zero'
  stop
end if

u(1) = r(1) / bet

! decomposition and forward substitution step

do j = 2,n  

  gam(j) = c(j-1) / bet

  bet = b(j) - a(j-1) * gam(j)

  if (bet == 0.) then
    write(0,*)'error tridiag: beta = 0'
    stop
  end if

  u(j) = (r(j) - a(j-1) * u(j-1)) / bet
    
end do

! backsubstitution step

do j = n-1,1,-1

  u(j) = u(j) - gam(j+1) * u(j+1)

end do

end subroutine tridiag_dp

! ---------------------------------------------------------

subroutine spline_dp(x,y,yp1,ypn,y2)

! create a spline object for interpolation
! based on Numerical Recipes

use parametersmod, only : dp

implicit none

! arguments

real(dp), dimension(:), intent(in)  :: x    ! vector of independent variable
real(dp), dimension(:), intent(in)  :: y    ! vector of dependent variable
real(dp),               intent(in)  :: yp1  ! first derivative of y at y(1)
real(dp),               intent(in)  :: ypn  ! first derivative of y at y(n)
real(dp), dimension(:), intent(out) :: y2   ! second derivatives of y at x

! parameter

real(dp), parameter :: ypmax = 1.e30

! local variables
integer :: n

real(dp), dimension(size(x)) :: a  ! tridiagonal vectors
real(dp), dimension(size(x)) :: b
real(dp), dimension(size(x)) :: c
real(dp), dimension(size(x)) :: r

! ----

n = size(x)

c(1:n-1) = x(2:n) - x(1:n-1)  ! vector of interval distances

r(1:n-1) = 6. * ((y(2:n) - y(1:n-1)) / c(1:n-1))  ! vector of first derivatives

r(2:n-1) = r(2:n-1) - r(1:n-2)  ! vector of second derivatives

a(2:n-1) = c(1:n-2)  ! off-axis diagonals

b(2:n-1) = 2. * (c(2:n-1) + a(2:n-1))

b(1) = 1.  ! endpoints of the central diagonal
b(n) = 1. 

! set the lower boundary condition for first derivative (upper off-diagonal vector c)

if (yp1 >= ypmax) then
  r(1) = 0.
  c(1) = 0.
else
  r(1) = (3. / (x(2) - x(1))) * ((y(2) - y(1)) / (x(2) - x(1)) - yp1)  
  c(1) = 0.5
end if

! set the upper boundary condition for the first derivative (lower off-diagonal vector a)

if (ypn >= ypmax) then
  r(n) = 0.
  a(n) = 0.
else
  r(n) = (-3. / (x(n) - x(n-1))) * ((y(n) - y(n-1)) / (x(n) - x(n-1)) - ypn)
  a(n) = 0.5
end if

call tridiag(a(2:n),b(1:n),c(1:n-1),r(1:n),y2(1:n))

end subroutine spline_dp

! ---------------------------------------------------------

real(dp) function cubspline_dp(xa,ya,y2a,x)

! evaluate a spline object y2a at a location given by x
! based on Numerical Recipes

use parametersmod, only : dp

implicit none

! arguments

real(dp), dimension(:), intent(in) :: xa
real(dp), dimension(:), intent(in) :: ya
real(dp), dimension(:), intent(in) :: y2a
real(dp),               intent(in) :: x

! local variables

integer  :: nrst
integer  :: khi
integer  :: klo
integer  :: n
real(dp) :: a
real(dp) :: b
real(dp) :: h

! ----

n = size(xa) 

! find the values in xa bracketing x

nrst = pos(xa,x)

if (x < xa(nrst)) then
  klo = nrst-1
  khi = nrst
else
  klo = nrst
  khi = nrst+1
end if

h = xa(khi) - xa(klo)

if (h == 0.) then
  write(0,*)'error cubspline: interval = 0'
  stop
end if

a = (xa(khi) - x) / h
b = (x - xa(klo)) / h

cubspline_dp = a * ya(klo) + b * ya(khi) + ((a**3 - a) * y2a(klo) + (b**3 - b) * y2a(khi)) * (h**2) / 6.

end function cubspline_dp

! ---------------------------------------------------------

real(sp) function angleinterp_sp(a0,a1,wgt)

! interpolate between two angles given in degrees given weight as wgt, returns degrees

use parametersmod, only : sp,pi => pi_sp,pir => pir_sp

implicit none

! arguments

real(sp), intent(in) :: a0
real(sp), intent(in) :: a1
real(sp), intent(in) :: wgt

! parameter

real(sp), parameter :: amax = 2. * pi

! local variables

real(sp) :: r0
real(sp) :: r1
real(sp) :: r2
real(sp) :: da
real(sp) :: dist

! ----

r0 = pir * a0
r1 = pir * a1

da = mod(r1 - r0,amax)

dist = mod(2. * da,amax) - da

r2 = r0 + dist * wgt

angleinterp_sp = r2 / pir

if (angleinterp_sp >= 360.) angleinterp_sp = angleinterp_sp - 360.

end function angleinterp_sp

! ---------------------------------------------------------

real(dp) function angleinterp_dp(a0,a1,wgt)

! interpolate between two angles given in degrees given weight as wgt, returns degrees

use parametersmod, only : dp,pi,pir

implicit none

! arguments

real(dp), intent(in) :: a0
real(dp), intent(in) :: a1
real(dp), intent(in) :: wgt

! parameter

real(dp), parameter :: amax = 2. * pi

! local variables

real(dp) :: r0
real(dp) :: r1
real(dp) :: r2
real(dp) :: da
real(dp) :: dist

! ----

r0 = pir * a0
r1 = pir * a1

da = mod(r1 - r0,amax)

dist = mod(2. * da,amax) - da

r2 = r0 + dist * wgt

angleinterp_dp = r2 / pir

if (angleinterp_dp >= 360._dp) angleinterp_dp = angleinterp_dp - 360._dp

end function angleinterp_dp

! ---------------------------------------------------------

integer function bp2ce(yrbp)

! returns the year CE based on a year BP

implicit none

integer, intent(in) :: yrbp

if (yrbp < 1950) then

  bp2ce = 1950 - yrbp

else

  bp2ce = 1949 - yrbp

end if

end function bp2ce

! ---------------------------------------------------------

logical function leapyear(yearCE)
  
implicit none

integer, intent(in) :: yearCE  !requires year CE as input (negative is BCE, no year zero)

!---

if ((mod(yearCE,4) == 0 .and. mod(yearCE,100) /= 0) .or. mod(yearCE,400) == 0) then
  leapyear = .true.
else
  leapyear = .false.
end if

end function leapyear

! ---------------------------------------------------------

subroutine overprint(message)

use parametersmod, only : stderr

implicit none

! argument

character(*), intent(in) :: message

! parameter

character, parameter :: cr = char(13)

! ---

write(stderr,'(a)',advance='no')message
flush(0)
write(0,'(a1)',advance='no')cr

end subroutine overprint

! ---------------------------------------------------------

end module utilitymod
