function f_integrand(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f_integrand
	f_integrand=sin(x)
	return
end function