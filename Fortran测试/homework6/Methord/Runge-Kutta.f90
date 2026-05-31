!本子程序用于龙格-库塔法对常微分方程的计算
subroutine Runge_Kutta(x_destination,y_destination,x_initial,y_initial,h)
	implicit none
	real(kind=8) :: x_destination,y_destination,x_initial,y_initial,h
	real(kind=8) :: x,y,K(4)
	real(kind=8), external :: equation_right

	x=x_initial
	y=y_initial

	do while(abs(x-x_destination)>=1d-10)
		K(1)=equation_right(x,y)
		K(2)=equation_right(x+0.5d0*h,y+0.5*h*K(1))
		K(3)=equation_right(x+0.5d0*h,y+0.5*h*K(2))
		K(4)=equation_right(x+h,y+h*K(3))
		y=y+h*(K(1)+2d0*K(2)+2d0*K(3)+K(4))/6d0
		x=x+h
	end do
	y_destination=y

	return
end subroutine