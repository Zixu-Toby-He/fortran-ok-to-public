!使用数值方法为加速迭代法
!该方法是用最初值的导数进行计算，而不必像牛顿法每次都换导数
!迭代公式：x_(k+1)=(f(x_k)-L*x)/(1-L)，其中f(x)对应方程为x=f(x)，此处选取f(x)=1/3*x^3
subroutine Post_acceleration(error,initial,solution)
	implicit none
	real(kind=8) :: error
	real(kind=8) :: solution
	real(kind=8) :: x_old,x_new
	real(kind=8) :: L,initial
	integer(kind=4) :: counter=1
	real(kind=8), external :: f_Post,f_derivitive

	counter=1
	x_old=initial

	write(*,*) "加速迭代法正在计算，请稍等"

	L=f_derivitive(x_old)
	x_new=f_Post(x_old,L)
	do while(abs(x_old-x_new)>error)
		x_old=x_new
		counter=counter+1
		x_new=f_Post(x_old,L)
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I3,'次迭代')") counter

	x_old=0
	x_new=0
	counter=0

	return
end subroutine

!加速迭代法选取函数
function f_Post(x,L)                                         
	implicit none
	real(kind=8) :: x
	real(kind=8) :: L
	real(kind=8) :: f_Post

	f_Post=(x*x*x/3d0-L*x)/(1d0-L)               !这一行表示函数
	
	return
end function