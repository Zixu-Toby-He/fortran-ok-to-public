!雅戈比迭代法
!雅戈比迭代法即，转化为x_(j+1)=f(x_j)。有两种转换方法，一种f(x)=(1/3)*x^3，另一种f(x)=(3x)^(1/3)
!经图解分析，前者在(-√3,√3)范围收敛至0，外面发散，后者在正半轴收敛至√3，负半轴收敛-√3，故选择后者
subroutine Jacobi(arror,solution)
	implicit none
	real(kind=8) :: arror
	real(kind=8) :: solution
	real(kind=8) :: x_old,x_new
	integer(kind=4) :: counter=0
	real(kind=8), external :: f_Jacobi

	!请不要忘记修改这里
	write(*,*) "您正在解的方程是x^3-3*x=0，仔细确认请您确认该方程。"
	!请不要忘记修改这里
	write(*,*) " "
	write(*,*) "若需要对所解方程以及函数进行修改，请及时关闭程序进行修改，以免造成不必要的误会"
	write(*,*) " "
	write(*,*) "如需修改，请不要忘记修改上方对话框中的方程，在程序中会以“!请不要忘记修改这里”字样进行提示。"
	pause

	write(*,*) "请输入初值："
	read(*,*) x_old
	write(*,*) "正在计算，请稍等"

	x_new=f_Jacobi(x_old)
	do while(abs(x_old-x_new)>arror)
		x_old=x_new
		counter=counter+1
		x_new=f_Jacobi(x_old)
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I3,'次迭代')") counter 

	return
end subroutine

!雅戈比迭代法选取函数
function f_Jacobi(x)                                         
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f_Jacobi

	f_Jacobi=sign(abs(3*x)**(1.0d0/3.0d0),3*x)               !这一行表示函数
	
	return
end function