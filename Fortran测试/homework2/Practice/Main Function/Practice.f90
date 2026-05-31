!本程序用于用数值方法解x^3-3*x=0
!该方程精确根有0与±√3，现在用数值方法解这道题

!主程序用于调用各类数值方法，并给出解析解及其近似值
program H2Practice
	implicit none
	real(kind=8), parameter:: root_0=0
	real(kind=8) :: root_pos
	real(kind=8) :: root_neg
	real(kind=8) :: error
	real(kind=8) :: solution_Jacobi,solution_Newton,solution_Post,solution_Aitken

	root_pos=sqrt(3d0)
	root_neg=-root_pos

	write(*,*) "请输入预计误差："
	read(*,*) error
	error=abs(error)
	write(*,*) ' '

	call Jacobi(error,solution_Jacobi)
	write(*,"('雅戈比迭代法给出的结果是',F30.26)") solution_Jacobi
	write(*,*) solution_Jacobi-root_pos
	pause

	call Newton_downhill(error+10d0**(-16),solution_Newton)
	write(*,"('牛顿下山法给出的结果是',F30.26)") solution_Newton
	write(*,*) solution_Newton-root_pos
	pause
	
	call Post_acceleration(error,solution_Post)
	write(*,"('加速迭代法给出的结果是',F30.26)") solution_Post
	write(*,*) solution_Post-root_pos
	pause
	
	call Aitken(error,solution_Aitken)
	write(*,"('埃特金法给出的结果是',F30.26)") solution_Aitken
	write(*,*) solution_Aitken-root_pos
	pause

	write(*,"('解析解的近似值为',F30.26)") root_pos
	pause

	stop
end program

include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Practice\original function\original function.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Practice\Jacobi\Jacobi.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Practice\Newton downhill\Newton downhill.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Practice\Post acceleration\Post acceleration.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Practice\Aitken\Aitken.f90"

