!本子程序使用梯形法进行积分的运算
subroutine trapezoid(I,h,error,upper_bound,lower_bound)
	implicit none
	real(kind=8) :: I,h,error,upper_bound,lower_bound
	real(kind=8), external :: S_trapezoid
	
	I=S_trapezoid(h,upper_bound,lower_bound)

	error=4d0*(S_trapezoid(h,upper_bound,lower_bound)-S_trapezoid(h/2d0,upper_bound,lower_bound))/3d0

	return
end subroutine

function S_trapezoid(h,upper_bound,lower_bound)
	implicit none
	real(kind=8) :: h,upper_bound,lower_bound,x
	integer(kind=4) :: n,counter
	real(kind=8) :: S_trapezoid
	real(kind=8), external :: f_integrand

	S_trapezoid=0
	counter=0
	x=lower_bound
	n=(upper_bound-lower_bound)/h
	do while(counter<=(n-1))
		S_trapezoid=S_trapezoid+h*(f_integrand(x)+f_integrand(x+h))/2d0
		x=x+h
		counter=counter+1
	end do
	return
end function