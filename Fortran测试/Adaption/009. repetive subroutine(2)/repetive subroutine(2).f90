program repetive_subroutine_2
	implicit none
	integer(kind=1) :: a=1

	call sub(a)
	call sub(a)
	stop
end program

subroutine sub(a)
	implicit none
	integer(kind=1) :: a
	integer(kind=1) :: i
	i=a
	i=i+1
	write(*,*) i
	return
end subroutine
