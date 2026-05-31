
!Warning: 本文件中解上下三角矩阵的方法需要通过增广矩阵传递参数，请在调用时注意参数转化！！！


!解上三角矩阵方程

subroutine up_right_solve(solve,x)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: solve(lines,lines+1),x(lines)
	integer(kind=1) :: counter(2)

	counter=lines
	do while(counter(1)>=1)
		counter(2)=lines
		x(counter(1))=solve(counter(1),lines+1)
		do while(counter(2)>counter(1))
			x(counter(1))=x(counter(1))-solve(counter(1),counter(2))*x(counter(2))
			counter(2)=counter(2)-1
		end do
		x(counter(1))=x(counter(1))/solve(counter(1),counter(1))
		counter(1)=counter(1)-1
	end do

	return
end subroutine


!解下三角矩阵方程

subroutine down_left_solve(solve,x)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: solve(lines,lines+1),x(lines)
	integer(kind=1) :: counter(2)

	counter=1

	do while(counter(1)<=lines)
		x(counter(1))=solve(counter(1),lines+1)
		counter(2)=1
		do while(counter(2)<counter(1))
			x(counter(1))=x(counter(1))-solve(counter(1),counter(2))*x(counter(2))
			counter(2)=counter(2)+1
		end do
		x(counter(1))=x(counter(1))/solve(counter(1),counter(1))
		counter(1)=counter(1)+1
	end do
		
	return
end subroutine