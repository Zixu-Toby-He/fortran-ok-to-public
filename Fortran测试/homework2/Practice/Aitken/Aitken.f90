!埃特金法
!埃特金法将方程转化为x=f(x)，进行x_k+1=f(f(x_k))-(f(f(x_k))-f(x_k))^2/(f(f(x_k))-2f(x_k)+x_k)
!对方程x^3-3*x=0，此处将其转化为x=1/3*x^3
subroutine Aitken(arror,solution)
	implicit none
	real(kind=8) :: arror
	real(kind=8) :: solution
	real(kind=8) :: x_old,x_new
	integer(kind=4) :: counter=1
	real(kind=8), external :: f_Aitken

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

	x_new=f_Aitken(x_old)
	do while(abs(x_old-x_new)>arror)
		x_old=x_new
		counter=counter+1
		x_new=f_Aitken(x_old)
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I3,'次迭代')") counter 

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

function f_Aitken_Pre(x)
	implicit none
	real(kind=8) :: x
	real(kind=8) :: f_Aitken_Pre

	f_Aitken_Pre=1d0/3d0*x**3               !这一行表示函数

	return
end function