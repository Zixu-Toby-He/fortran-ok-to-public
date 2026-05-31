program H6
	implicit none
	real(kind=8) :: result_accurate,result_basic_Euler(4),result_improved_Euler(4),result_Runge_Kutta(4)
	real(kind=8) :: step(4)
	real(kind=8) :: x_initial,y_initial
	real(kind=8), external :: equation_right,y_accurate
	integer(kind=1) :: counter

	write(*,*) "本程序用于用不同的数值方法解如下条件的微分方程"
	write(*,*) "微分方程：dy/dx=-x^2y^2"
	write(*,*) "定解条件：y(0)=3，取值范围x∈[0,1.5]"
	write(*,*) "使用的方法有欧拉法、改进欧拉法和龙格-库塔法，计算的指标为y(1.5)"
	write(*,*) "计算不同的方法时，均使用0.1、0.05、0.025、0.0125作为步长进行计算"
	write(*,*) ' '
	write(*,*) "该方程形式简单，经手动计算得y(x)=3/(x^3+1)，y(1.5)=24/35=0.6857"
	write(*,*) "该结果用于检验计算的正确与否"
	write(*,*) ' '

	result_accurate=y_accurate(1.5d0)
	step(1)=1d-1
	step(2)=5d-2
	step(3)=2.5d-2
	step(4)=1.25d-2
	x_initial=0d0
	y_initial=3d0
	
	
	write(*,*) "现在开始数值计算"
	counter=1
	do while(counter<=4)
		call basic_Euler(1.5d0,result_basic_Euler(counter),x_initial,y_initial,step(counter))
		call improved_Euler(1.5d0,result_improved_Euler(counter),x_initial,y_initial,step(counter))
		call Runge_Kutta(1.5d0,result_Runge_Kutta(counter),x_initial,y_initial,step(counter))
		counter=counter+1
	end do
	write(*,*) "计算已完成"
	write(*,*) ' '

	counter=1
	write(*,*) "结果精确值为",result_accurate
	write(*,*) ' '
	do while(counter<=4)
		write(*,*) "当步长为",step(counter),"时"
		write(*,*) "基本欧拉法得到的结果为",result_basic_Euler(counter),"，误差",result_basic_Euler(counter)-result_accurate
		write(*,*) "改进欧拉法得到的结果为",result_improved_Euler(counter),"，误差",result_improved_Euler(counter)-result_accurate
		write(*,*) "龙格库塔法得到的结果为",result_Runge_Kutta(counter),"，误差",result_Runge_Kutta(counter)-result_accurate
		write(*,*) ' '
		counter=counter+1
	end do
	write(*,*) ' '
	write(*,*) "可以看出在这些步长下精确度龙格—库塔法精确度最高，基本欧拉法精确度最低"
	write(*,*) ' '
	pause

	stop
end program

include "C:\Program Files\Microsoft Visual Studio\My code\homework6\Methord\improved Euler.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework6\Methord\basic Euler.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework6\Methord\Runge-Kutta.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework6\Assistant\original functions.f90"


