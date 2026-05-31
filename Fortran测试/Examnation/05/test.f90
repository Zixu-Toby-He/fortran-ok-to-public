program test
	implicit none
	real(kind=8) :: x_r,x,y,y_1,error,h
	real(kind=8),external :: f
	
	h=1d-2
	x=0d0
	y=0d0
	x_r=1d-1
	y_1=0
	do while(x<=1d0)
		y=y+h*f(x,y)
		y_1=y_1+h*f(x,y_1)/2d0
		y_1=y_1+h*f(x+h/2d0,y_1)/2d0
		x=x+h
		error=2d0*(y_1-y)
		if(abs(x-x_r)<=1d-6) then
			write(*,*) x
			write(*,"(f8.3)") y
			write(*,"(f8.3)") error
			x_r=x_r+1d-1
			write(*,*) ' '
		end if
	end do
	
	stop
end program


function f(x,y)
	implicit none
	real(kind=8) :: x,y
	real(kind=8) :: f

	f=3d0*x+2d0*y*y

	return
end function