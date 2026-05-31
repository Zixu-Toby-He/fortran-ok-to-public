program test
	implicit none
	real(kind=8) :: x,I,error
	integer(kind=4) :: n
	real(kind=8),external :: integrate
	
	n=2
	do while(n<=32)
		I=integrate(n)
		error=16d0/15d0*(integrate(2*n)-integrate(n))
		write(*,*) n
		write(*,*) I
		write(*,*) error
		write(*,*) ' '
		n=n*2
	end do
	pause
	stop
end program

function integrate(n)
	implicit none
	integer(kind=4) :: n
	real(kind=8) :: integrate
	real(kind=8) :: x,h
	real(kind=8),external :: f

	h=2d0/(n*1d0)
	integrate=0
	x=0d0
	do while(x<1d0)
		integrate=integrate+h*(f(x)+4*f(x+h/2d0)+f(x+h))/6d0
		x=x+h
	end do

	return
end function

function f(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f

	f=exp(x*x+2d0)+exp(5d0*x)

	return
end function