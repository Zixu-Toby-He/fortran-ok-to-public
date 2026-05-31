!该程序证明了该编译器具有自带的save功能
program repetibe_subroutine
	implicit none

	call sub()
	call sub()

	stop
end program




subroutine sub()
	implicit none
	integer(kind=1) :: i=1
	i=i+1
	write(*,*) i
end subroutine
