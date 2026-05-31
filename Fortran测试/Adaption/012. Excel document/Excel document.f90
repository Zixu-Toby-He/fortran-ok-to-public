program Excel_document
	implicit none
	open(unit=10,file="TEST.csv")
	write(10,"(I2,',',I2)") 1,1
	write(10,*) 1
	close(10)
end program