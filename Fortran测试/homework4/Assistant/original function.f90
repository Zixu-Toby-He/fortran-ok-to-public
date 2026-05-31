!这里用于储存本程序处理问题的原函数与其导数

!这是原函数
function f_origine(x)
	implicit none
	real(kind=8) :: x,f_origine
	f_origine=1d0/(1d0+x*x)
	return
end function

!这是导函数，实际并没有用上
function f_derivative(x)
	implicit none
	real(kind=8) :: x,f_derivative
	f_derivative=(-2d0*x)/(1+x*x)**2
	return
end function