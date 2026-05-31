!本子程序用于利用超松弛迭代法进行方程的求根
!该方法迭代时，x_new=(1-ω)*x_old+ω*x_new_GS，只需在算出GS迭代法的x_new每一项后对其进行加权计算
!个人尝试做了其对应的矩阵方法，但出现了明显漏洞，无法找到，故没有继续尝试
!该方法理论上不能得到精确解，故将其给出的解与杜立特法结果（与高斯法结果完全一致）对比可以得到误差

subroutine Overrelaxation(A,x,b,error)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: A(lines,lines),b(lines),x(lines)
	real(kind=8) :: D(lines,lines),L(lines,lines),U(lines,lines)
	real(kind=8) :: G_matrix(lines,lines),g_vector(lines)
	real(kind=8) :: error,delta,omega
	real(kind=8) :: x_new(lines),x_old(lines)
	integer(kind=1) :: counter(3)

	interface
		subroutine DLU(A,D,L,U)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),D(lines,lines),L(lines,lines),U(lines,lines)
		end subroutine
	end interface
	
	omega=1.5d0
	counter=1
	G_matrix=0
	g_vector=0

	call DLU(A,D,L,U)
	
	!对迭代矩阵和向量进行赋值
	do while(counter(1)<=lines)
		counter(2)=1
		g_vector(counter(1))=b(counter(1))/D(counter(1),counter(1))
		do while(counter(2)<=lines)
			G_matrix(counter(1),counter(2))=(L(counter(1),counter(2))+U(counter(1),counter(2)))/D(counter(1),counter(1))
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do
	
	!进行赋值与初次迭代
	counter=1
	x_new=0
	x_old=x_new
	do while(counter(1)<=lines)
		counter(2)=1
		x_new(counter(1))=0
		do while(counter(2)<=lines)
			x_new(counter(1))=x_new(counter(1))+G_matrix(counter(1),counter(2))*x_new(counter(2))
			counter(2)=counter(2)+1
		end do
		x_new(counter(1))=x_new(counter(1))+g_vector(counter(1))
		counter(1)=counter(1)+1
	end do
	x_new=x_new*omega+x_old*(1d0-omega)
	delta=0
	do while(counter(3)<=lines)
		delta=delta+(x_old(counter(3))-x_new(counter(3)))**2
		counter(3)=counter(3)+1
	end do
	delta=sqrt(delta)
	counter=1

	!进行迭代
	do while(delta>=error)
		x_old=x_new

		!迭代过程
		do while(counter(1)<=lines)
			counter(2)=1
			x_new(counter(1))=0
			do while(counter(2)<=lines)
				x_new(counter(1))=x_new(counter(1))+G_matrix(counter(1),counter(2))*x_new(counter(2))
				counter(2)=counter(2)+1
			end do
			x_new(counter(1))=x_new(counter(1))+g_vector(counter(1))
			counter(1)=counter(1)+1
		end do
		x_new=x_new*omega+x_old*(1d0-omega)
		counter(1)=1
		counter(2)=1

		!改变量计算
		delta=0
		do while(counter(1)<=lines)
			delta=delta+(x_old(counter(1))-x_new(counter(1)))**2
			counter(1)=counter(1)+1
		end do
		delta=sqrt(delta)
		counter(1)=1
		counter(3)=counter(3)+1
	end do
	x=x_new
	write(*,"('一共经历了',I3,'次迭代')") counter(3) 

	return
end subroutine