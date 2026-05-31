program test
	implicit none
	real(kind=8) :: x_old,x_new
	integer(kind=4) :: counter
	real(kind=8), external :: g,f

	interface
	end interface

	x_old=10d0
	x_new=g(x_old)
	counter=1
	
!	write(*,*) x_old,f(x_old),g(x_old)
	do while(counter<=20)
		x_old=x_new
		x_new=g(x_old)
		counter=counter+1
	end do
	write(*,*) ' '
	write(*,*) x_old,f(x_old),g(x_old)

	write(*,*) ' '
	pause

	stop
end program

subroutine A()
	implicit none
	
	return
end subroutine

function f(x)
	implicit none
	real(kind=8) :: f
	real(kind=8) :: x

	f=x**3d0-2d0*x**2d0-3d0*x+1d0

	return
end function

function g(x)
	implicit none
	real(kind=8) :: g
	real(kind=8) :: x

	g=(2d0*x**2+3d0*x-1)**(1d0/3d0)
	return
end function
