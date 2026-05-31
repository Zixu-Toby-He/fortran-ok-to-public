program test
	implicit none
	real(kind=8) :: x_new(6),x_old(6),a(6,6),b(6)
	real(kind=8) :: G_matrix(6,6),g_vector(6),times
	integer(kind=4) :: counter(3)

	open(unit=10,file="matrix.txt")
	open(unit=11,file="vector.txt")
	read(10,*) a
	read(11,*) b
	close(11)
	close(10)
	a=transpose(a)

	x_new=0d0
	x_old=0d0

!	write(*,*) ' '
!	write(*,"(6f5.2)") transpose(a)
!	write(*,*) ' '
!	write(*,"(f5.2)") b
	
	counter=1
	do while(counter(1)<=6)
		times=a(counter(1),counter(1))
		G_matrix(counter(1),:)=(-1d0)*a(counter(1),:)/times
		g_vector(counter(1))=b(counter(1))/times
		G_matrix(counter(1),counter(1))=G_matrix(counter(1),counter(1))+1d0
		counter(1)=counter(1)+1
	end do

!	write(*,*) ' '
!	write(*,"(6f5.2)") transpose(G_matrix)
!	write(*,*) ' '
!	write(*,"(f5.2)") g_vector

	counter=1
	do while(counter(1)<=10)
		x_old=x_new
		counter(2)=1
		do while(counter(2)<=6)
			x_new(counter(2))=0d0
			counter(3)=1
			do while(counter(3)<=6)
				x_new(counter(2))=x_new(counter(2))+G_matrix(counter(2),counter(3))*x_new(counter(3))
				counter(3)=counter(3)+1
			end do
			x_new(counter(2))=x_new(counter(2))+g_vector(counter(2))
			counter(2)=counter(2)+1
		end do
		write(*,*) counter(1)
		write(*,"(f7.3)") x_new
		write(*,*) ' '
		counter(1)=counter(1)+1
	end do


	pause

	stop
end program