!本文件利用拉格朗日插值法对给定的数据进行插值

!拉格朗日插值法子程序，调用时自变量数据与因变量数据分开导入，若是放置在同一个二阶矩阵中的话请分开放置
!插值结果分别储存在x_result与y_result中，若要提取导入其他位置请设置好接口
subroutine Lagrange(x_given,y_given)
	implicit none
	real(kind=8) :: x_given(16),y_given(16)
	real(kind=8) :: x_result(6001),y_result(6001),error(6001)
	real(kind=8), external :: f_origine
	integer(kind=2) ::counter
	
	interface
		function f_Lagrange(x_given,y_given,x)
			implicit none
			real(kind=8) :: x
			real(kind=8) :: x_given(16),y_given(16)
			real(kind=8) :: f_Lagrange
		end function
	end interface

	counter=1
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	open(unit=20,file="C:\Program Files\Microsoft Visual Studio\My code\homework4\Result data\Lagrange.csv")
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	do while(counter<=6001)
		x_result(counter)=x_given(1)+(counter-1)*(x_given(16)-x_given(1))/6000d0
		y_result(counter)=f_Lagrange(x_given,y_given,x_result(counter))
		error(counter)=y_result(counter)-f_origine(x_result(counter))
		
		!该功能实现将结果输出到指定文件夹的文件中，若不需要请删去
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		write(20,"(e35.25,',',e35.25,',',e35.25,',',e35.25,',',f10.2,'%')") x_result(counter),y_result(counter),f_origine(x_result(counter)),error(counter),error(counter)/f_origine(x_result(counter))*100
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		counter=counter+1
	end do
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	close(20)
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	counter=1

	return
end subroutine


!拉格朗日插值法函数，在给定数据情况下自变量为实数
function f_Lagrange(x_given,y_given,x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: x_given(16),y_given(16)
	real(kind=8) :: f_Lagrange
	real(kind=8) :: add
	integer(kind=1) :: counter(2)
	real(kind=8), external :: f_origine
	
	counter=1
	add=1
	f_Lagrange=0

	do while(counter(1)<=16)
		add=y_given(counter(1))
		counter(2)=1
		do while(counter(2)<=16)
			if(counter(1)==counter(2)) then
				add=add*1
			else
				add=add*((x-x_given(counter(2)))/(x_given(counter(1))-x_given(counter(2))))
			end if
			counter(2)=counter(2)+1
		end do
		f_Lagrange=f_Lagrange+add
		counter(1)=counter(1)+1
	end do

	return
end function
