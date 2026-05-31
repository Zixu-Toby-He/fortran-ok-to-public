!解上三角矩阵方程

subroutine up_right_solve(solve,x)
	implicit none
	real(kind=8) :: solve(14,15),x(14)
	integer(kind=1) :: counter(2)

	counter=14
	do while(counter(1)>=1)
		counter(2)=14
		x(counter(1))=solve(counter(1),15)
		do while(counter(2)>counter(1))
			x(counter(1))=x(counter(1))-solve(counter(1),counter(2))*x(counter(2))
			counter(2)=counter(2)-1
		end do
		x(counter(1))=x(counter(1))/solve(counter(1),counter(1))
		counter(1)=counter(1)-1
	end do

	return
end subroutine
