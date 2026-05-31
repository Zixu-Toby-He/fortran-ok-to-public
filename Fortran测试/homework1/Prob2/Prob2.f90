subroutine calculation(a,v,c,quadra)                                           !该子程序用于整合计算
	implicit none
	integer(kind=2) :: a(5,5),v(5),c(5)
	integer(kind=2) :: quadra

	interface
		subroutine multiply(a,v,c)
			implicit none
			integer(kind=2) :: a(5,5),v(5),c(5)
		end subroutine
		subroutine quadratic(v,c,quadra)
			implicit none
			integer(kind=2) :: v(5),c(5)
			integer(kind=2) :: quadra
		end subroutine
	end interface

	call readin(a,v)
	call multiply(a,v,c)
	call quadratic(v,c,quadra)

end subroutine


program A1P2                                                                      !主程序用于启动、简单说明和屏幕输出
	implicit none
	integer(kind=2) :: a(5,5),v(5),c(5)
	integer(kind=2) :: quadra
	interface
		subroutine calculation(a,v,c,quadra)
			implicit none
			integer(kind=2) :: a(5,5),v(5),c(5)
			integer(kind=2) :: quadra
		end subroutine
	end interface

	write(*,*) "本程序用于计算特定的一个5*5矩阵与5维列向量的二次型"
	write(*,*) "矩阵与向量分别存储于文件matrix.txt与vector.txt内，如要修改请打开文件修改"
	pause

	call calculation(a,v,c,quadra)

	pause

	write(*,"('     / ',I2,' \')") c(1)
	write(*,"('    | ',I3,'  |')") c(2)
	write(*,"('A*v=| ',I3,'  |')") c(3)
	write(*,"('    | ',I3,'  |')") c(4)
	write(*,"('     \ ',I2,' /')") c(5)
	write(*,*) " "
	write(*, "('(v^T)*A*v=',I5)") quadra

	pause
	stop
end program


subroutine readin(a,v)                                                   !从文件中读取相关矩阵和向量
	implicit none
	integer(kind=2)::a(5,5),v(5)
	open(unit=10,file="matrix.txt")
	read(10,*) a(1:5,1:5)
	write(*,"(5I3)") a(1:5,1:5)
	write(*,*) "矩阵读入成功"
	close(unit=10)
	pause

	open(unit=11,file="vector.txt")
	read(11,*) v(1:5)
	write(*,*) v(1:5)
	write(*,*) "向量读入成功"
	close(unit=11)
	write(*,*)"信息读入成功，准备计算"
	pause

	return
end subroutine


subroutine multiply(a,v,c)                                                !矩阵乘法计算
	implicit none
	integer(kind=2):: a(5,5),v(5),c(5)
	integer(kind=1):: i=1

	write(*,*) "开始计算矩阵乘法"
	do while(i<=5)
		c(i)=a(i,1)*v(1)+a(i,2)*v(2)+a(i,3)*v(3)+a(i,4)*v(4)+a(i,5)*v(5)
		i=i+1
	end do
	write(*,*)"矩阵乘法计算已完成"
	return
end subroutine


subroutine quadratic(v,c,quadra)                                          !利用矩阵乘法结果计算二次型
	implicit none
	integer(kind=2) :: v(5),c(5)
	integer(kind=2) :: quadra

	write(*,*) "开始计算二次型"
	quadra=v(1)*c(1)+v(2)*c(2)+v(3)*c(3)+v(4)*c(4)+v(5)*c(5)
	write(*,*) "二次型计算已完成"

	return
end subroutine