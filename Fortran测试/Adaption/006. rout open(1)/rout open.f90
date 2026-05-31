program rout_open
	implicit none
	integer(kind=1) :: a

	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\Adaption\006. rout open\rout open\Rout open File.txt")
	read(10,*) a
	close(unit=10)
	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\Adaption\006. rout open\rout open\Rout open File_1.txt")
	write(10,*) a
	close(unit=10)
	write(*,*) a
end program