!原函数f(x)=x^3-3x，即将原方程写为f(x)=0的函数
function f_original(x)
	real(kind=8) :: x
	real(kind=8) :: f_original
	
	f_original=x*x*x-3*x

	return
end function

!这里是原函数导数f'(x)=3x^2-3
function f_derivitive(x)
	real(kind=8) :: x
	real(kind=8) :: f_derivitive
	
	f_derivitive=3*x*x-3

	return
end function