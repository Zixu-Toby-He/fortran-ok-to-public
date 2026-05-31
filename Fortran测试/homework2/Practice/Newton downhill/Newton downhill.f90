!牛顿下山法
!牛顿下山法运用导数进行初值到根的逼近
subroutine Newton_downhill(error,solution)
	implicit none
	real(kind=8) :: error,solution
	real(kind=8) :: x_old,x_new,x_trial,lamda=1d0
	integer(kind=4) :: counter_1=1,counter_2=0
	real(kind=8), external :: f_Newton,f_original

	!请不要忘记修改这里
	write(*,*) "您正在解的方程是x^3-3*x=0，仔细确认请您确认该方程。"
	!请不要忘记修改这里
	write(*,*) " "
	write(*,*) "若需要对所解方程以及函数进行修改，请及时关闭程序进行修改，以免造成不必要的误会"
	write(*,*) " "
	write(*,*) "如需修改，请不要忘记修改上方对话框中的方程，在程序中会以“!请不要忘记修改这里”字样进行提示。"
	pause

	write(*,*) "请输入初值：（注意避开+1与-1）"
	read(*,*) x_old

	do while(((x_old)**2==1))
		write(*,*) "请输入初值：（注意避开+1与-1）"
		read(*,*) x_old
	end do
	write(*,*) "正在计算，请稍等"

	lamda=1d0
	x_trial=f_Newton(x_old,lamda)
	do while((abs(f_original(x_trial))>=abs(f_original(x_old))).and.(x_trial==1))
		counter_2=counter_2+1
		lamda=lamda/2d0
		x_trial=f_Newton(x_old,lamda)
	end do
	x_new=x_trial

	do while(abs(x_old-x_new)>error)
		counter_1=counter_1+1
		lamda=1d0
		x_old=x_new
		x_trial=f_Newton(x_old,lamda)
		do while((abs(f_original(x_trial))>=abs(f_original(x_old))).and.(x_trial==1))
			counter_2=counter_2+1
			lamda=lamda/2d0
			x_trial=f_Newton(x_old,lamda)
		end do
		x_new=x_trial
	end do

	solution=x_new
	write(*,"('计算已完成，一共进行了',I3,'次大迭代，',I5,'次小迭代')") counter_1,counter_2

	return
end subroutine

!牛顿下山法的迭代函数
function f_Newton(x,lamda)
	implicit none
	real(kind=8) :: x,lamda
	real(kind=8) :: f_Newton
	real(kind=8), external :: f_original,f_derivitive

	f_Newton=x-lamda*f_original(x)/f_derivitive(x)             !这里是函数

	return
end function