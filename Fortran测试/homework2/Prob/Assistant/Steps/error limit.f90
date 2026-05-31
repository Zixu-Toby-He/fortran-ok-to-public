subroutine error_limit()
	implicit none
	real(kind=8) :: error=0d0
	real(kind=8) :: initial=3d0
	real(kind=8) :: solution_Jacobi,solution_Newton,solution_Post,solution_Aitken




	!进行雅戈比迭代法的运算
	write(*,*) "现在进行雅戈比法迭代"
	call Jacobi(error,initial,solution_Jacobi)                        !进行迭代
	write(*,"('雅戈比迭代的解为',F25.20)") solution_Jacobi
	write(*,*) " "


	!进行牛顿下山法的运算
	write(*,*) "现在进行牛顿下山法迭代"
	call Newton_downhill(error+2.23d0*10d0**(-16),initial,solution_Newton)           !进行迭代
	write(*,"('牛顿下山法迭代的解为',F25.20)") solution_Newton
	write(*,*) " "


	!进行加速迭代法的运算
	write(*,*) "现在进行加速迭代法迭代"
	call Post_acceleration(error,initial,solution_Post)           !进行迭代
	write(*,"('加速迭代的解为',F25.20)") solution_Post
	write(*,*) " "


	!进行埃特金迭代法的运算
	write(*,*) "现在进行埃特金法迭代"
	call Aitken(error,initial,solution_Aitken)                    !进行迭代
	write(*,"('埃特金迭代的解为',F25.20)") solution_Aitken
	write(*,*) " "


	write(*,*)"这几种方法的误差为"
	write(*,"('雅戈比法：',D20.10)") solution_Jacobi-1d0*sqrt(3d0)
	write(*,"('牛顿下山法：',D20.10,',修正为2.23*10^(-16)')") solution_Newton-1d0*sqrt(3d0)
	write(*,"('加速迭代法：',D20.10)") solution_Post-1d0*sqrt(3d0)
	write(*,"('埃特金迭代法：',D20.10)") solution_Aitken-1d0*sqrt(3d0)


	write(*,*) "  经试验，牛顿下山法虽然现实误差为0，但在可操作时间内无法达到实验要求的状态，取误差修正2.23*10^(-16)作为最终误差"


	return
end subroutine