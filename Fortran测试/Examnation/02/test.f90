program test
	implicit none
	real(kind=8) :: x,x_origine(6),y_origine(6),a(6)
	integer(kind=4) :: counter
	real(kind=8), external :: y

	interface
		subroutine determine(a,x_origine,y_origine)
			implicit none
			real(kind=8) :: a(6),x_origine(6),y_origine(6)
		end subroutine
		function f(x,x_origine,a)
			implicit none
			real(kind=8) :: x,x_origine(6),a(6)
			real(kind=8) :: f
		end function
	end interface
	
	counter=1
	do while(counter<=6)
		x_origine(counter)=2d0*(counter-1d0)
		y_origine(counter)=y(x_origine(counter))
		counter=counter+1
	end do
	counter=1
!	write(*,*) x_origine
!	write(*,*) y_origine

	call determine(a,x_origine,y_origine)

	counter=0
	do while(counter<=10)
		x=counter*1d0
		write(*,*) x
		write(*,*) y(x)
		write(*,*) f(x,x_origine,a)
		write(*,*) f(x,x_origine,a)-y(x)
		write(*,*) ' '
		counter=counter+1
	end do

	stop
end program

function f(x,x_origine,a)
	implicit none
	real(kind=8) :: x,x_origine(6),a(6)
	real(kind=8) :: f
	real(kind=8) :: times
	integer(kind=1) :: counter

	f=a(1)
	times=1
	counter=1
	do while(counter<=5)
		times=times*(x-x_origine(counter))
		f=f+a(counter+1)*times
		counter=counter+1
	end do

	return
end function



subroutine determine(a,x_origine,y_origine)
	implicit none
	real(kind=8) :: a(6),x_origine(6),y_origine(6)
	real(kind=8) :: solve(6,6)
	integer(kind=1) :: counter(2)

	counter=1
	do while(counter(1)<=6)
		solve(1,counter(1))=y_origine(counter(1))
		counter(1)=counter(1)+1
	end do
	
	counter=1
	counter(1)=2
	do while(counter(1)<=6)
		counter(2)=counter(1)
		solve(counter(1),counter(1))=(solve(counter(1)-1,counter(1))-solve(counter(1)-1,counter(1)-1))/(x_origine(counter(1))-x_origine(1))
		do while(counter(2)<=6)
			solve(counter(1),counter(2))=(solve(counter(1)-1,counter(2))-solve(counter(1)-1,counter(2)-1))/(x_origine(counter(2))-x_origine(counter(2)+1-counter(1)))
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do

	counter=1
	do while(counter(1)<=6)
		a(counter(1))=solve(counter(1),counter(1))
		counter(1)=counter(1)+1
	end do

	return
end subroutine

function y(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: y

	y=5d0*cos(x)+4d0*x*x

	return
end function
