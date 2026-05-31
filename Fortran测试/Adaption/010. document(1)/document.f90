!本文件测试接入端口是否能够输入
!事实证明可以，这对系统输出文件有帮助
program document
	implicit none
	integer(kind=1) :: n
	character :: Doc="document.txt"

	read(*,*) n
	open(unit=n,file=Doc)
	write(n,*) n
	close(unit=n)

	stop
end program