!本文件利用牛顿插值法对给定的数据进行插值

!牛顿插值法子程序，调用时自变量数据与因变量数据分开导入，若是放置在同一个二阶矩阵中的话请分开放置
!插值结果分别储存在x_result与y_result中，若要提取导入其他位置请设置好接口
subroutine Newton(x_given,y_given)
	implicit none
	real(kind=8) :: x_given(16),y_given(16)
	real(kind=8) :: x_result(6001),y_result(6001),error(6001)
	real(kind=8) :: a(16)
	real(kind=8), external :: f_origine
	integer(kind=2) :: counter

	interface
		function f_Newton(x_given,a,x)
			implicit none
			real(kind=8) :: a(16),x_given(16)
			real(kind=8) :: x
			real(kind=8) :: f_Newton
		end function
		subroutine coefficient_Newton(x_given,y_given,a)
			implicit none
			real(kind=8) :: x_given(16),y_given(16),a(16)
		end subroutine
	end interface
	
	!设置参数
	call coefficient_Newton(x_given,y_given,a)
	counter=1
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	open(unit=31,file="C:\Program Files\Microsoft Visual Studio\My code\homework4\Result data\Newton.csv")
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	do while(counter<=6001)
		x_result(counter)=x_given(1)+(counter-1d0)*(x_given(16)-x_given(1))/6000d0
		y_result(counter)=f_Newton(x_given,a,x_result(counter))
		error(counter)=y_result(counter)-f_origine(x_result(counter))

		!该功能实现将结果输出到指定文件夹的文件中，若不需要请删去
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		write(31,"(e35.25,',',e35.25,',',e35.25,',',e35.25,',',f10.2,'%')") x_result(counter),y_result(counter),f_origine(x_result(counter)),error(counter),error(counter)/f_origine(x_result(counter))*100
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		counter=counter+1
	end do

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	close(31)
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	return
end subroutine


!牛顿插值法参数设定子程序
subroutine coefficient_Newton(x_given,y_given,a)
	implicit none
	real(kind=8) :: x_given(16),y_given(16),a(16)
	real(kind=8) :: solve(16,16)
	integer(kind=1) :: counter(2)

	!计算f(x_1...,x_m;x_k)并存储在一个下三角矩阵中方便调用，需要用的系数a则为该矩阵的对角

	a=0
	solve=0
	counter=1
	solve(:,1)=y_given(:)
	a(1)=solve(1,1)
	counter(1)=2
	
	do while(counter(1)<=16)
		counter(2)=counter(1)
		solve(counter(1),counter(1))=(solve(counter(1),counter(1)-1)-solve(counter(1)-1,counter(1)-1))/(x_given(counter(1))-x_given(1))
		a(counter(1))=solve(counter(1),counter(1))
		do while(counter(2)<=16)
			solve(counter(2),counter(1))=(solve(counter(2),counter(1)-1)-solve(counter(2)-1,counter(1)-1))/(x_given(counter(2))-x_given(counter(2)-counter(1)+1))
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do

	counter=1
	return
end subroutine

!牛顿插值法函数，在给定参数情况下自变量为实数
function f_Newton(x_given,a,x)
	implicit none
	real(kind=8) :: a(16),x_given(16)
	real(kind=8) :: x
	real(kind=8) :: f_Newton
	real(kind=8) :: add
	integer(kind=1) :: counter
	
	counter=1
	f_Newton=0
	add=1
	do while(counter<=16)
		f_Newton=f_Newton+a(counter)*add
		add=add*(x-x_given(counter))
		counter=counter+1
	end do

	return
end function