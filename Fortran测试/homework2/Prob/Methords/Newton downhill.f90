!牛顿下山法
!牛顿下山法运用导数进行初值到根的逼近
subroutine Newton_downhill(error,initial,solution)
	implicit none
	real(kind=8) :: error,solution,initial
	real(kind=8) :: x_old,x_new,x_trial,lamda=1d0
	integer(kind=4) :: counter_1=1,counter_2=0
	real(kind=8), external :: f_Newton,f_original

	counter_1=1
	counter_2=0
	x_old=initial

	write(*,*) "牛顿下山法正在计算，请稍等"

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