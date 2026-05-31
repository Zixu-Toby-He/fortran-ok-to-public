subroutine eigen(pi,e)
	implicit none
	real(kind=8) :: A(1000),h,pi,e
	integer(kind=4) :: counter

	interface
		subroutine sketch(A,pi,e,k_t)
			implicit none
			real(kind=8) :: A(1000),pi,e,k_t
		end subroutine
	end interface

	counter=1

	write(*,*) "通过分离变量并对边界条件进行分析，可以得到本征函数如下"
	write(*,*) " "
	write(*,*) "T_n(x,t)=A_n*sin(λx)*e^(-(k/(cρ))λt)，其中λ=nπ*1m^(-1)"
	write(*,*) " "
	write(*,*) "                  ∞"
	write(*,*) "                 -----"
	write(*,*) "                  ╲"
	write(*,*) "而对于T则有T(x,t)=  >   A_n*sin(λx)*e^(-(k/(cρ))λt)，λ=nπ*1m^(-1)"
	write(*,*) "                  ╱"
	write(*,*) "                 -----"
	write(*,*) "                  n=1"
	write(*,*) "此时对于常数A_n则通过代入初值条件T(0<x<1m,0)=100℃解决"
	write(*,*) "     ┏ 1m"
	write(*,*) "A_n=2┃    100℃*sin(λx)dx"
	write(*,*) "     ┛ 0"
	write(*,*) "（手算是不可能手算的，这辈子也不可能手算-200/(nπ)*((-1)^n-1)）"
	write(*,*) " "

	write(*,*) "现在开始A_n赋值，n=1~1000"

	do while(counter<=1000)
		h=1d-2/(counter*1d0)
		call Simpson(A(counter),h,1d0,0d0,counter*pi)
		A(counter)=2*A(counter)
		counter=counter+1
	end do

	write(*,*) "A_n赋值完成"
	write(*,*) " "
	pause
	write(*,*) " "
	write(*,*) "计算t=k_t*cp/(kπ)时的温度分布，此时n=1的成分刚好变化到原来的e^-k_t"
	write(*,*) "k_t取0.0000001、0.01、0.1、0.5、1"

	call sketch(A,pi,e,0.0000001d0)
	pause
	call sketch(A,pi,e,0.01d0)
	pause
	call sketch(A,pi,e,0.1d0)
	pause
	call sketch(A,pi,e,0.5d0)

	write(*,*) "因为文件相关内容未处理好，只能一次产生一组点，将这些点平移到excel文档中进行画图"

	return
end subroutine

subroutine sketch(A,pi,e,k_t)
	implicit none
	real(kind=8) :: A(1000),pi,e,k_t
	real(kind=8) :: x,y
	integer(kind=4) :: counter

	x=0
	y=0
	open(unit=22,file="C:\Program Files\Microsoft Visual Studio\My code\presentarion\Result\eigen.csv")
	do while(x<=1.001d0)
		y=0
		counter=1
		do while(counter<=1000)
			y=y+A(counter)*sin(counter*pi*x)*e**(-1d0*k_t*counter)
			counter=counter+1
		end do
		write(22,*) x,',',y
		x=x+0.001d0
	end do
	close(unit=22)
	return
end subroutine