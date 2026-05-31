!此处用于寻找根
subroutine root_search(error)
	implicit none
	real(kind=8) :: error
	real(kind=8) :: initials(41)
	real(kind=8) :: root_Jacobi(3),root_Newton(3),root_Post(3),root_Aitken(3)
	real(kind=8) :: solution_Jacobi,solution_Newton,solution_Post,solution_Aitken
	integer(kind=1) :: i,root_counter(3)

	!从文件中读取初值（尝试用子程序书写但找不出漏洞）
	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\initiate\initial.txt")
	read (10,*) initials
	close(unit=10)

	i=1
	root_counter=0
	root_Jacobi=0
	root_Newton=0
	root_Post=0
	root_Aitken=0

	!进行雅戈比迭代法的运算
	write(*,*) "现在进行雅戈比法迭代"
	write(*,*) " "
	do while(i<=41)
		write(*,"('现在代入初值',F5.2)") initials(i)
		call Jacobi(error,initials(i),solution_Jacobi)                    !进行迭代
		call classify(error,root_Jacobi,root_counter,solution_Jacobi)     !进行根的归类
		i=i+1
		write(*,"('该次迭代结果为',F25.20)") solution_Jacobi
		write(*,*) " "
	end do
	root_Jacobi=root_Jacobi/root_counter                                  !根的处理是将不同初值迭代出来的同一根取平均值
	write(*,"('雅戈比迭代的解为',F25.20)") root_Jacobi
	write(*,*) " "
	i=1
	root_counter=0
	pause

	!进行牛顿下山法的运算
	write(*,*) "现在进行牛顿下山法迭代"
	write(*,*) " "
	do while(i<=41)
		write(*,"('现在代入初值',F5.2)") initials(i)
		call Newton_downhill(error,initials(i),solution_Newton)           !进行迭代
		call classify(error,root_Newton,root_counter,solution_Newton)     !进行根的归类
		i=i+1
		write(*,"('该次迭代结果为',F25.20)") solution_Newton
		write(*,*) " "
	end do
	root_Newton=root_Newton/root_counter                                  !根的处理是将不同初值迭代出来的同一根取平均值
	write(*,"('牛顿下山法迭代的解为',F25.20)") root_Newton
	write(*,*) " "
	i=1
	root_counter=0
	pause

	!进行加速迭代法的运算
	write(*,*) "现在进行加速迭代法迭代"
	write(*,*) " "
	do while(i<=41)
		if ((abs(initials(i))==1.2d0).or.((abs(initials(i))==1.1d0))) then
			i=i+1
			cycle
		end if
		write(*,"('现在代入初值',F5.2)") initials(i)
		call Post_acceleration(error,initials(i),solution_Post)           !进行迭代
		call classify(error,root_Post,root_counter,solution_Post)         !进行根的归类
		i=i+1
		write(*,"('该次迭代结果为',F25.20)") solution_Post
		write(*,*) " "
	end do
	root_Post=root_Post/root_counter                                      !根的处理是将不同初值迭代出来的同一根取平均值
	write(*,"('加速迭代的解为',F25.20)") root_Post
	write(*,*) " "
	i=1
	root_counter=0
	pause

	!进行埃特金迭代法的运算
	write(*,*) "现在进行埃特金法迭代"
	write(*,*) " "
	do while(i<=41)
		write(*,"('现在代入初值',F5.2)") initials(i)
		call Aitken(error,initials(i),solution_Aitken)                    !进行迭代
		call classify(error,root_Aitken,root_counter,solution_Aitken)     !进行根的归类
		i=i+1
		write(*,"('该次迭代结果为',F25.20)") solution_Aitken
		write(*,*) " "
	end do
	root_Aitken=root_Aitken/root_counter                                  !根的处理是将不同初值迭代出来的同一根取平均值
	write(*,"('埃特金迭代的解为',F25.20)") root_Aitken
	write(*,*) " "
	i=1
	root_counter=0
	pause

	write(*,*)"这几种方法的误差为"
	write(*,"('雅戈比法：',D20.10)") root_Jacobi(1)-sqrt(3d0)
	write(*,"('牛顿下山法：',D20.10)") root_Newton(1)-sqrt(3d0)
	write(*,"('加速迭代法：',D20.10)") root_Post(1)-sqrt(3d0)
	write(*,"('埃特金迭代法：',D20.10)") root_Aitken(1)-sqrt(3d0)



	return
end subroutine

!本子程序用于对根进行归类
subroutine classify(error,root,counter,solution)
	implicit none
	real(kind=8) :: root(3),error
	integer(kind=1) :: counter(3)
	real(kind=8) :: solution

	if (abs(solution-sqrt(3d0))<=100*error) then
		root(1)=root(1)+solution
		counter(1)=counter(1)+1
	else
		if (abs(solution+sqrt(3d0))<=100*error) then
			root(3)=root(3)+solution
			counter(3)=counter(3)+1
		else
			if (abs(solution)<=100*error) then
				root(2)=root(2)+solution
				counter(2)=counter(2)+1
			end if
		end if
	end if

	return
end subroutine


