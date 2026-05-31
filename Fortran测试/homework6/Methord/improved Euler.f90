!本子程序用于改进欧拉法对常微分方程的计算
subroutine improved_Euler(x_destination,y_destination,x_initial,y_initial,h)
	implicit none
	real(kind=8) :: x_destination,y_destination,x_initial,y_initial,h
	real(kind=8) :: x,y,y_adjust
	real(kind=8), external :: equation_right

	x=x_initial
	y=y_initial

	do while(abs(x-x_destination)>=1d-10)
		y_adjust=y+h*equation_right(x,y)
		y=y+0.5d0*h*(equation_right(x,y)+equation_right(x+h,y_adjust))
		x=x+h
	end do
	y_destination=y

	return
end subroutine