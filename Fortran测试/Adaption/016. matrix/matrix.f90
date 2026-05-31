!本程序用于测试矩阵的属性
!事实证明一般理解下的矩阵乘法要用transpose(matmul(transpose(a),transpose(b)))来实现
program matrix
	implicit none
	real(kind=4) :: a(2,2)=(/1,2,3,4/),b(2,2)=(/5,6,7,8/),c(2,2)
	real(kind=4) :: d(5,1)=(/1,2,3,4,5/),e(1,5)
	
	data e /1,2,3,4,5/
	c=a+b
	write(*,*) ' '
	write(*,*) c(1,2)
	write(*,*) ' '
	write(*,"(2f7.2)") a-b
	write(*,*) ' '
	write(*,"(2f7.2)") a*b
	write(*,*) ' '
	write(*,"(2f7.2)") a/b
	write(*,*) ' '
	write(*,"(2f7.2)") 2*a+3*b
	write(*,*) ' '
	write(*,"(2f7.2)") matmul(a,b)
	write(*,*) ' '
	write(*,"(2f7.2)") transpose(matmul(transpose(a),transpose(b)))
	write(*,*) ' '
	write(*,*) transpose(matmul(transpose(d),transpose(e)))
	write(*,*) ' '
	write(*,*) matmul(e,d)
	pause
	stop
end program