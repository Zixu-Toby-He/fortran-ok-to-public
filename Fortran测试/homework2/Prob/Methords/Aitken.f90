!埃特金法
!埃特金法将方程转化为x=f(x)，进行x_k+1=f(f(x_k))-(f(f(x_k))-f(x_k))^2/(f(f(x_k))-2f(x_k)+x_k)
!对方程x^3-3*x=0，此处将其转化为x=1/3*x^3
subroutine Aitken(error,initial,solution)
	implicit none
	real(kind=8) :: error,initial
	real(kind=8) :: solution
	real(kind=8) :: x_old,x_new
	integer(kind=4) :: counter=1
	real(kind=8), external :: f_Aitken
	
	counter=1
	x_old=initial

	write(*,*) "埃特金法正在计算，请稍等"

	x_new=f_Aitken(x_old)
	do while(abs(x_old-x_new)>error)
		x_old=x_new
		counter=counter+1
		x_new=f_Aitken(x_old)
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I4,'次迭代')") counter

	return
end subroutine

!埃特金法选取函数
function f_Aitken(x)                                         
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f_Aitken
	real(kind=8), external :: f_Aitken_pre

	f_Aitken=f_Aitken_Pre(f_Aitken_Pre(x))-(f_Aitken_Pre(f_Aitken_Pre(x))-f_Aitken_Pre(x))**2/(f_Aitken_Pre(f_Aitken_Pre(x))-2*f_Aitken_Pre(x)+x)               !这一行表示函数
	
	return
end function

!埃特金法的预备函数
function f_Aitken_Pre(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f_Aitken_Pre

	f_Aitken_Pre=1d0/3d0*x**3               !这一行表示函数

	return
end function