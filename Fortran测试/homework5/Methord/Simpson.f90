!本子程序使用辛普森法进行积分的计算
subroutine Simpson(I,h,error,upper_bound,lower_bound)
	implicit none
	real(kind=8) :: I,h,error,upper_bound,lower_bound
	real(kind=8), external :: S_Simpson

	I=S_Simpson(h,upper_bound,lower_bound)
	error=16d0*(S_Simpson(h,upper_bound,lower_bound)-S_Simpson(h/2d0,upper_bound,lower_bound))/15d0

	return
end subroutine

function S_Simpson(h,upper_bound,lower_bound)
	implicit none
	real(kind=8) :: h,upper_bound,lower_bound,x
	integer(kind=4) :: m,counter
	real(kind=8) :: S_Simpson
	real(kind=8), external :: f_integrand

	S_Simpson=0
	counter=0
	x=lower_bound
	m=(upper_bound-lower_bound)/(2d0*h)
	do while(counter<=(m-1))
		S_Simpson=S_Simpson+h*(f_integrand(x)+4*f_integrand(x+h)+f_integrand(x+2*h))/3d0
		x=x+2*h
		counter=counter+1
	end do
	return
end function