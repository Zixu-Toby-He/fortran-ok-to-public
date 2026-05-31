!使用数值方法为加速迭代法
!该方法是用最初值的导数进行计算，而不必像牛顿法每次都换导数
!迭代公式：x_(k+1)=(f(x_k)-L*x)/(1-L)，其中f(x)对应方程为x=f(x)，此处选取f(x)=1/3*x^3
subroutine Post_acceleration(arror,solution)
	implicit none
	real(kind=8) :: arror
	real(kind=8) :: solution
	real(kind=8) :: x_old,x_new
	real(kind=8) :: L
	integer(kind=4) :: counter=1
	real(kind=8), external :: f_Post,f_derivitive

	!请不要忘记修改这里
	write(*,*) "您正在解的方程是x^3-3*x=0，仔细确认请您确认该方程。"
	!请不要忘记修改这里
	write(*,*) " "
	write(*,*) "若需要对所解方程以及函数进行修改，请及时关闭程序进行修改，以免造成不必要的误会"
	write(*,*) " "
	write(*,*) "如需修改，请不要忘记修改上方对话框中的方程，在程序中会以“!请不要忘记修改这里”字样进行提示。"
	pause

	write(*,*) "请输入初值：（请注意避开±2√3/3与±1）"
	read(*,*) x_old
	write(*,*) "正在计算，请稍等"

	L=f_derivitive(x_old)
	x_new=f_Post(x_old,L)
	do while(abs(x_old-x_new)>arror)
		x_old=x_new
		counter=counter+1
		x_new=f_Post(x_old,L)
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I3,'次迭代')") counter 

	return
end subroutine

!后加速法选取函数
function f_Post(x,L)                                         
	implicit none
	real(kind=8) :: x
	real(kind=8) :: L
	real(kind=8) :: f_Post

	f_Post=(x*x*x/3d0-L*x)/(1d0-L)               !这一行表示函数
	
	return
end function