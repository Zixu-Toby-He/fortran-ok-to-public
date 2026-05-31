
!这是微分方程y'=f(x,y)的方程右边的f(x,y)
function equation_right(x,y)
	implicit none
	real(kind=8) :: x,y
	real(kind=8) :: equation_right

	equation_right=-1d0*(x*y)**2

	return
end function

!这是微分方程y'=-(x^2)(y^2)的精确解
function y_accurate(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: y_accurate
	
	y_accurate=3d0/(1+x*x*x)

	return
end function