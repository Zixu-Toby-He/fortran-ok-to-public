!用于从读取子程序
subroutine readin(A,b)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: A(lines,lines),b(lines)
	integer(kind=1) :: counter=1

	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\homework3\Matrix.txt")
	open(unit=11,file="C:\Program Files\Microsoft Visual Studio\My code\homework3\Ax.txt")
	do while(counter<=9)
		read(10,*) A(counter,1:lines)
		read(11,*) b(counter)
		counter=counter+1
	end do
	close(unit=10)
	close(unit=11)
	counter=1

	return
end subroutine