!实验证明要用文件名要用字符串，字符数组做不到
program document
	implicit none
	character(len=100) :: n="423.txt"
	open(unit=101,file=n)
	write(101,*) 20
	close(unit=101)
end program