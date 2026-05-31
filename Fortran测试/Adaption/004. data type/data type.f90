!本程序用于计算不同类型数据的值传递问题
program datatype
	implicit none
	real(kind=4) :: a=3
	integer(kind=1) :: b=1
	real(kind=4), external :: f

	b=f(a)
	write(*,*) b
	a=b
	write(*,*) a/2
	a=b*1.0
	write(*,*) a/2
	a=b*1d0
	write(*,*) a/2

	pause
	stop
end program

function f(x)
	implicit none
	real(kind=4) :: x
	real(kind=4) :: f

	f=x/2

	return
end function