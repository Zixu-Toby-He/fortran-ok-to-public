program H5
	implicit none
	real(kind=8) :: h,upper_bound,lower_bound
	real(kind=8) :: I_accurate,I_Simpson,error_Simpson,I_trapezoid,error_trapezoid

	h=0.1d0
	upper_bound=5d0
	lower_bound=1d0
	I_accurate=cos(1d0)-cos(5d0)

	write(*,*) "本程序用于利用不同方法数值计算如下积分"
	write(*,*) "┏ 5"
	write(*,*) "┃   sin(x)dx"
	write(*,*) "┛ 1"
	write(*,*) "很容易看出这个问题的结果为cos(1)-cos(5)，数值上为以下结果"
	write(*,*) I_accurate
	write(*,*) "这里给这个结果来验证以下方法的可靠性与程序实现的正确性"
	write(*,*) " "
	pause

	write(*,*) "现在开始辛普森法的计算"
	call Simpson(I_Simpson,h,error_Simpson,upper_bound,lower_bound)
	write(*,*) "辛普森法计算完成"
	write(*,*) "计算结果为",I_Simpson
	write(*,*) "公式误差为",error_Simpson
	write(*,*) "与实际值偏差为",I_Simpson-I_accurate
	pause

	write(*,*) "现在开始梯形法的计算"
	call trapezoid(I_trapezoid,h,error_trapezoid,upper_bound,lower_bound)
	write(*,*) "梯形法计算完成"
	write(*,*) "计算结果为",I_trapezoid
	write(*,*) "误差为",error_trapezoid
	write(*,*) "与实际值偏差为",I_trapezoid-I_accurate
	pause

	stop
end program


include "C:\Program Files\Microsoft Visual Studio\My code\homework5\Methord\Simpson.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework5\Methord\trapezoid.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework5\Assistant\original function.f90"
