!本子程序使用辛普森法进行积分的计算
subroutine Simpson(I,h,upper_bound,lower_bound,lambda)
	implicit none
	real(kind=8) :: I,h,upper_bound,lower_bound,lambda
	real(kind=8), external :: S_Simpson

	I=S_Simpson(h,upper_bound,lower_bound,lambda)

	return
end subroutine

function S_Simpson(h,upper_bound,lower_bound,lambda)
	implicit none
	real(kind=8) :: h,upper_bound,lower_bound,x,lambda
	integer(kind=4) :: m,counter
	real(kind=8) :: S_Simpson
	real(kind=8), external :: f_integrand

	S_Simpson=0
	counter=0
	x=lower_bound
	m=(upper_bound-lower_bound)/(2d0*h)
	do while(counter<=(m-1))
		S_Simpson=S_Simpson+h*(f_integrand(x,lambda)+4*f_integrand(x+h,lambda)+f_integrand(x+2*h,lambda))/3d0
		x=x+2*h
		counter=counter+1
	end do
	return
end function

function f_integrand(lambda,x)
	implicit none
	real(kind=8) :: x,lambda
	real(kind=8) :: f_integrand
	f_integrand=100*sin(lambda*x)
	return
end function