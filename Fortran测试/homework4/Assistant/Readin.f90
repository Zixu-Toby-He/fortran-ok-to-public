subroutine readin(x,y)
	implicit none
	real(kind=8) :: x(16),y(16)

	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\x.txt")
	open(unit=11,file="C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\y.txt")
	read(10,*) x
	read(11,*) y
	close(unit=10)
	close(unit=11)
	return
end subroutine