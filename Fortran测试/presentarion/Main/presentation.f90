program presentationr
	implicit none
	real(kind=8) :: pi,e
	
	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\presentarion\Assistant\pi.txt")
	read(10,"(f100.98)") pi
	close(unit=10)
	open(unit=11,file="C:\Program Files\Microsoft Visual Studio\My code\presentarion\Assistant\e.txt")
	read(11,"(f100.98)") e
	close(unit=11)

	write(*,*) "一根1m长截面直径为w的铝棒除两端均与外界孤立"
	write(*,*) "初始时其各处温度均为100℃，但两端均与0摄氏度的冰水混合"
	write(*,*) "求一段时间后温度与距离的对应关系"
	write(*,*) "（注：原题为单位为开尔文，但由于0K太过迷惑故改单位为摄氏度）"

	write(*,*) " "
	write(*,*) "                  绝热"
	write(*,*) "       ┏----------------------┓"
	write(*,*) "   0℃ ┃       100℃          ┃ 0℃"
	write(*,*) "       ┗----------------------┛"
	write(*,*) "                  绝热"
	write(*,*) " "
	pause

	write(*,*) " "
	write(*,*) "           \partial T      K    \partial^2 T"
	write(*,*) "微分方程：------------===-----*---------------"
	write(*,*) "           \partial t     cρ   \partial x^2"
	write(*,*) " "

	write(*,*) "边界条件：T(0,t)=T(1m,t)=0℃"
	write(*,*) "初值条件：T(0<x<1m,0)=100℃"

	write(*,*) " "
	write(*,*) "这个问题有两种思路解决，一种是差分法，另一种是利用特征函数"
	write(*,*) "差分法利用前面步骤得到的数值进行下一步运算，计算机实现时直接用微分方程进行处理"
	write(*,*) "特征函数法则利用边界条件与积分计算出展开式系数，并取数值上重要项进行带入运算"
	write(*,*) " "
	pause

	write(*,*) "现在通过特征函数与积分进行计算"
	write(*,*) " "
	call eigen(pi,e)
	write(*,*) "计算完成"
	write(*,*) " "
	pause

	write(*,*) " "
	write(*,*) "现在通过差分法进行计算"
	call FD()
	write(*,*) "计算完成"
	write(*,*) " "
	pause

	stop
end program


include "C:\Program Files\Microsoft Visual Studio\My code\presentarion\Assistant\Simpson.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\presentarion\Methord\eigen.f90"


subroutine FD()
	implicit none
	real(kind=8) :: step_t,step_x,eta
	real(kind=8) :: x(1001),T_before(1001),T_after(1001)
	integer(kind=4) :: counter(2)
	
	write(*,*) "差分法是用(f(x+Δx)-f(x))/Δx来代替f'(x)这样一种思路来对方程进行处理的方法"
	write(*,*) "设置合理的步长，计算机可以利用此方法通过迭代得到具有一定准确性的数据"
	write(*,*) "该方法虽然不能导出理论精确值，但具有方便实现的优点"
	write(*,*) " "
	write(*,*) "具体方程T(x,t+Δt)=T(x,t)+(K/cρΔt)/(Δx)^2*(T(x+Δx,t)+T(x-Δx,t)-2T(x,t))"
	write(*,*) ""
	write(*,*) "这里设置步长Δx=0.001,Δt=0.0000001cρ/K"

	step_t=1d-7
	step_x=1d-3
	eta=step_t/(step_x*step_x)
	write(*,"(f11.9)") step_t
	return

	x(1)=0d0
	T_before(1)=0d0
	T_after(1)=0d0
	counter=2
	do while(counter(1)<=1001)
		x(counter(1))=x(counter(1)-1)+step_x
		T_before(counter(1))=100d0
		T_after(counter(1))=100d0
		counter(1)=counter(1)+1
	end do
	T_before(1001)=0d0
	T_after(1001)=0d0
	counter=1

	write(*,*) "开始迭代"
	write(*,*) " "
	do while(counter(1)<=1d7)
		T_before=T_after
		T_after(1)=0d0
		T_after(1001)=0d0
		counter(2)=2
		do while(counter(2)<=1000)
			T_after(counter(2))=T_before(counter(2))+eta*(T_before(counter(2)+1)+T_before(counter(2)-1)-2d0*T_before(counter(2)))
			counter(2)=counter(2)+1
		end do
		write(*,*) counter(1)
		if((counter(1)==1).or.(counter(1)==1d5).or.(counter(1)==1d6).or.(counter(1)==5d6).or.(counter(1)==1d7)) then
			counter(2)=1
			open(unit=301,file="C:\Program Files\Microsoft Visual Studio\My code\presentarion\Result\FD.csv")
			do while(counter(2)<=1001)
				write(301,*) x(counter(2)),',',T_after(counter(2)),',',T_before(counter(2))
				counter(2)=counter(2)+1		
			end do
			close(301)
			pause
		end if
		counter(1)=counter(1)+1
	end do
	

	return
end subroutine