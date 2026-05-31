!本子程序使用杜立特法进行线性方程组Ax=b的求解
!该方法将A分解为一个下三角矩阵和一个上三角矩阵的乘积A=LU，再通过解一个下三角方程Ly=b和一个上三角方程Ux=y来进行运算
!该方法在理论上可以得到一个方程组的精确解

subroutine Doolittle_decomposition(A,x,b)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: A(lines,lines),x(lines),b(lines),L(lines,lines),U(lines,lines),solve(lines,lines+1),y(lines)
	integer(kind=1) :: counter(3)

	interface
		subroutine down_left_solve(solve,x)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: solve(lines,lines+1),x(lines)
		end subroutine
		subroutine up_right_solve(solve,x)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: solve(lines,lines+1),x(lines)
		end subroutine
	end interface

	U=0
	L=0
	counter=1

	!赋L的对角值和初始位置值（总是在初始位置出现程序错误，我养成习惯先定初值，顺便赋值L对角）
	do while(counter(1)<=lines)
		L(counter(1),counter(1))=1
		U(1,counter(1))=A(1,counter(1))
		L(counter(1),1)=A(counter(1),1)/U(1,1)
		counter(1)=counter(1)+1
	end do

	!分解A为LU
	counter(1)=2
	do while(counter(1)<=lines)
		counter(3)=1

		!U的对角值进行赋予（防止出错）
		U(counter(1),counter(1))=A(counter(1),counter(1))
		do while(counter(3)<=counter(1)-1)
			U(counter(1),counter(1))=U(counter(1),counter(1))-L(counter(1),counter(3))*U(counter(3),counter(1))
			counter(3)=counter(3)+1
		end do
		
		!对U和L分别进行赋值
		counter(2)=counter(1)+1		
		do while(counter(2)<=lines)
			counter(3)=1
			U(counter(1),counter(2))=A(counter(1),counter(2))
			L(counter(2),counter(1))=A(counter(2),counter(1))
			do while(counter(3)<=counter(1)-1)
				U(counter(1),counter(2))=U(counter(1),counter(2))-L(counter(1),counter(3))*U(counter(3),counter(2))
				L(counter(2),counter(1))=L(counter(2),counter(1))-L(counter(2),counter(3))*U(counter(3),counter(1))
				counter(3)=counter(3)+1
			end do
			L(counter(2),counter(1))=L(counter(2),counter(1))/U(counter(1),counter(1))
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do
	
	!对Ly=b进行增广矩阵的赋值（解上下三角矩阵传参需要，个人设置）
	counter=1
	do while(counter(1)<=lines)
		counter(2)=1
		do while(counter(2)<=lines)
			solve(counter(1),counter(2))=L(counter(1),counter(2))
			counter(2)=counter(2)+1
		end do
		solve(counter(1),10)=b(counter(1))
		counter(1)=counter(1)+1
	end do

	!解下三角	
	call down_left_solve(solve,y)

	!对Ux=y进行增广矩阵赋值（解上下三角矩阵传参需要，个人设置）
	counter=1
	do while(counter(1)<=lines)
		counter(2)=1
		do while(counter(2)<=lines)
			solve(counter(1),counter(2))=U(counter(1),counter(2))
			counter(2)=counter(2)+1
		end do
		solve(counter(1),10)=y(counter(1))
		counter(1)=counter(1)+1
	end do
	
	!解上三角
	call up_right_solve(solve,x)

	return
end subroutine

