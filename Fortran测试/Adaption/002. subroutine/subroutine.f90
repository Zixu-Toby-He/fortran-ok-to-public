!本程序用于测试子程序的参数传递
!事实证明是地址传递
program test
	implicit none
	integer(kind=1) :: a=1,b=2

	call change(a,b)

	write(*,*) a,b

	stop
end program

subroutine change(x,y)
	implicit none
	integer(kind=1) :: x,y
	integer(kind=1) :: c
	c=x
	x=y
	y=c
return
end subroutine