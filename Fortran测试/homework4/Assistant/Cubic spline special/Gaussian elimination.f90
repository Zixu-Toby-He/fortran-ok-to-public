!本子程序利用高斯消元法解方程Ax=b
!该方法致力于通过行间运算将增广矩阵上三角化，再解上三角矩阵的方程来解决这个问题

!该方法在理论上可以得到一个方程组的精确解
subroutine Gaussian_elimination(A,x,b)
	implicit none
	real(kind=8) :: A(14,14),b(14),x(14),times
	real(kind=8) :: solve(14,15),exchanger,minus
	integer(kind=1) :: counter(3)

	interface
		subroutine up_right_solve(solve,x)
			implicit none
			real(kind=8) :: solve(14,15),x(14)
		end subroutine
	end interface

	counter=1

	!对增广矩阵赋初值
	do while(counter(1)<=14)
		solve(counter(1),1:14)=A(counter(1),1:14)
		solve(counter(1),15)=b(counter(1))
		counter(1)=counter(1)+1
	end do
	counter(1)=1


	!处理矩阵，利用行间的加减运算将矩阵对角化
	do while(counter(1)<=14)

		!使用不完整的排序算法（由下到上一回）将最大的摆到最前面来
		counter(2)=14
		do while(counter(2)>counter(1))
			if(abs(solve(counter(2)-1,counter(1)))<abs(solve(counter(2),counter(1)))) then
				counter(3)=counter(1)
				do while(counter(3)<=15)
					exchanger=solve(counter(2)-1,counter(3))
					solve(counter(2)-1,counter(3))=solve(counter(2),counter(3))
					solve(counter(2),counter(3))=exchanger
					counter(3)=counter(3)+1
				end do
				counter(3)=counter(1)
			end if
			counter(2)=counter(2)-1
		end do
		counter(2)=counter(1)+1

		!进行减法，将矩阵转化为上三角矩阵
		do while(counter(2)<=14)
			times=solve(counter(2),counter(1))/solve(counter(1),counter(1))
			counter(3)=counter(1)
			do while(counter(3)<=15)
				solve(counter(2),counter(3))=solve(counter(2),counter(3))-solve(counter(1),counter(3))*times
				counter(3)=counter(3)+1
			end do
			counter(3)=counter(1)+1
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do

	!函数：解上三角矩阵	
	call up_right_solve(solve,x)

	return
end subroutine


