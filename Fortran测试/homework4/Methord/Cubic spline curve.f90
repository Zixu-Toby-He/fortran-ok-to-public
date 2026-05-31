!本文件利用三次样条曲线插值法对给定的数据进行插值

!三次样条曲线插值法子程序，调用时自变量数据与因变量数据分开导入，若是放置在同一个二阶矩阵中的话请分开放置
!插值结果分别储存在x_result与y_result中，若要提取导入其他位置请设置好接口
subroutine Cubic_spline_curve(x_given,y_given)
	implicit none
	real(kind=8) :: x_given(16),y_given(16)
	real(kind=8) :: x_result(6001),y_result(6001),error(6001)
	real(kind=8) :: m(16)
	real(kind=8) :: h
	real(kind=8), external :: f_origine
	integer(kind=2) :: counter(2)

	interface
		function f_cubic(i,x,m,x_given,y_given,h)
			implicit none
			real(kind=8) :: m(16),x_given(16),y_given(16)
			real(kind=8) :: x,h
			real(kind=8) :: f_cubic
			integer(kind=2) :: i
		end function
		subroutine coefficient_cubic(x_given,y_given,m,h)
			implicit none
			real(kind=8) :: x_given(16),y_given(16),m(16)
			real(kind=8) :: h
		end subroutine
	end interface

	counter=1
	h=(x_given(16)-x_given(1))/15d0
	m=0

	!设置参数
	call coefficient_cubic(x_given,y_given,m,h)
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	open(unit=40,file="C:\Program Files\Microsoft Visual Studio\My code\homework4\Result data\Cubic sline curve.csv")
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	do while(counter(2)<=6001)
		counter(1)=(counter(2)-2)/400+1
		x_result(counter(2))=x_given(1)+(counter(2)-1)*(x_given(16)-x_given(1))/6000d0
		y_result(counter(2))=f_cubic(counter(1),x_result(counter(2)),m,x_given,y_given,h)
		error(counter)=y_result(counter(2))-f_origine(x_result(counter(2)))
		
		!该功能实现将结果输出到指定文件夹的文件中，若不需要请删去
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		write(40,"(e35.25,',',e35.25,',',e35.25,',',e35.25,',',f10.2,'%')") x_result(counter(2)),y_result(counter(2)),f_origine(x_result(counter(2))),error(counter(2)),error(counter(2))/f_origine(x_result(counter(2)))*100d0
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		counter(2)=counter(2)+1
	end do

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	close(40)
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	return
end subroutine


!牛顿插值法函数，在给定参数情况下自变量为实数
!由于每个部分函数表达式都不完全一样，对应的第i部分用参数i表示
function f_cubic(i,x,m,x_given,y_given,h)
	implicit none
	real(kind=8) :: m(16),x_given(16),y_given(16)
	real(kind=8) :: x,h
	real(kind=8) :: f_cubic
	integer(kind=2) :: i

	f_cubic=((h+2*(x-x_given(i)))*((x-x_given(i+1))**2)*y_given(i)+(h-2*(x-x_given(i+1)))*(x-x_given(i))**2*y_given(i+1))/(h**3)
	f_cubic=f_cubic+((x-x_given(i))*(((x-x_given(i+1))**2))*m(i)+(x-x_given(i+1))*(((x-x_given(i))**2))*m(i+1))/(h*h)

	return
end function


!牛顿插值法参数设定子程序
subroutine coefficient_cubic(x_given,y_given,m,h)
	implicit none
	real(kind=8) :: x_given(16),y_given(16),m(16)
	real(kind=8) :: h
	real(kind=8) :: A(14,14),c(14)
	integer(kind=1) :: counter

	!设置需要解的方程
	!这里由于每个h都相等，针对这个进行了算法的简化，要进行其他使用时请对算法进行有针对性的修改
	A(1,1)=2d0
	counter=2
	m=0
	do while(counter<=14)
		A(counter,counter)=2d0
		A(counter-1,counter)=0.5d0
		A(counter,counter-1)=0.5d0
		c(counter)=1.5d0*(y_given(counter+2)-y_given(counter))/h
		counter=counter+1
	end do
	
	!解线性方程
	call Gaussian_elimination(A,m(2:15),c)

	return
end subroutine