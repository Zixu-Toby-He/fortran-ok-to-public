!雅戈比迭代法
!雅戈比迭代法即，转化为x_(j+1)=f(x_j)。有两种转换方法，一种f(x)=(1/3)*x^3，另一种f(x)=(3x)^(1/3)
!经图解分析，前者在(-√3,√3)范围收敛至0，外面发散，后者在正半轴收敛至√3，负半轴收敛-√3，0处收敛至0，故选择后者
subroutine Jacobi(error,initial,solution)
	implicit none
	real(kind=8) :: error
	real(kind=8) :: solution
	real(kind=8) :: initial
	real(kind=8) :: x_old,x_new
	integer(kind=4) :: counter=0
	real(kind=8), external :: f_Jacobi

	counter=0
	x_old=initial

	write(*,*) "雅戈比迭代法正在计算，请稍等"

	x_new=f_Jacobi(x_old)
	do while(abs(x_old-x_new)>error)
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