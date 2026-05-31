program H3
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: A_origine(lines,lines),b_origine(lines)
	real(kind=8) :: x_Gauss(lines),x_Doolittle(lines),x_GS(lines),x_Overrelaxation(lines)
	real(kind=8) :: error	

	interface
		subroutine readin(A,b)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),b(lines)
		end subroutine
		subroutine Gaussial_elimination(A,x,b)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),b(lines),x(lines)
		end subroutine
		subroutine Doolittle_decomposition(A,x,b)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),b(lines),x(lines)
		end subroutine
		subroutine Gauss_Seidel(A,x,b,error)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),b(lines),x(lines),error
		end subroutine
		subroutine Overrelaxation(A,x,b,error)
			implicit none
			integer(kind=1),parameter :: lines=9
			real(kind=8) :: A(lines,lines),x(lines),b(lines),error
		end subroutine
	end interface
	
	write(*,*) "本程序用于运用不同的方法计算同一个线性方程组Ax=b"
	write(*,*) "其对应A存放于Matrix.txt文件，b存放于Ax.txt文件"
	write(*,*) "为检查其结果正确性，请打开Answer check.txt文件进行检查"
	write(*,*) " "

	write(*,*) "现在读取相应数据"
	call readin(A_origine,b_origine)               !读取
	write(*,*) "读取成功"
	write(*,*) " "

	write(*,*) "现在通过消元法与分解法解这个方程组，理论上可以得到的精确解"
	write(*,*) " "

	call Gaussian_elimination(A_origine,x_Gauss,b_origine)
	write(*,*) "高斯消元法的结果是："
	write(*,*) x_Gauss
	write(*,*) " "
	pause

	call Doolittle_decomposition(A_origine,x_Doolittle,b_origine)
	write(*,*) "杜立特分解法的结果是："
	write(*,*) x_Doolittle
	write(*,*) " "
	pause

	write(*,*) " "
	write(*,*) "现在通过迭代法解这个方程组，理论上不能得到的精确解"
	write(*,*) "一下两种迭代法迭代统一从原点开始迭代"
	write(*,*) "请输入预计误差：（误差请不要设置小于1.1*10^(-16)否则超松弛法可能会陷入死循环）"
	read(*,*) error
	write(*,*) " "
	pause

	call Gauss_Seidel(A_origine,x_GS,b_origine,error)
	write(*,*) "高斯-赛德尔迭代法的结果是："
	write(*,*) x_GS
	write(*,*) " "
	write(*,*) "以下为此次计算与真实值的偏差："
	write(*,*) x_GS-x_Doolittle
	write(*,*) " "
	write(*,*) "该偏差在迭代次数达到54次时达到最小值，此时error为10^(-45)量级"
	write(*,*) "矩阵法迭代次数修正到57次，且得到的最终结果偏差比直接计算更大。"
	write(*,*) " "
	pause
	write(*,*) " "

	call Overrelaxation(A_origine,x_Overrelaxation,b_origine,error)
	write(*,*) "超松弛法的结果是："
	write(*,*) x_Overrelaxation
	write(*,*) " "
	write(*,*) "以下为此次计算与真实值的偏差："
	write(*,*) x_Overrelaxation-x_Doolittle
	write(*,*) " "
	write(*,*) "该偏差在迭代次数达到67次时达到最小值，此时error为10^(-16)量级"
	pause
	stop
end program


include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Methord\Gaussian elimination.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Methord\Doolittle decomposition.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Methord\Gauss-Seidel.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Methord\Overrelaxation.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Assistant\Readin.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Assistant\Triangle solve.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework3\Assistant\DLU.f90"

