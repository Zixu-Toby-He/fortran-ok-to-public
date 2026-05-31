!本程序用于地址的输出
program file
	implicit none
	
	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\Adaption\013. File\123.txt")
	write(10,*) "这个文件在C:\Program Files\Microsoft Visual Studio\My code\Adaption\013. File"
	close(unit=10)
end program