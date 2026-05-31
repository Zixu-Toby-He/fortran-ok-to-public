program A1P1                                          !Assignment 1 Problem 1
       implicit none
       integer(kind=1) :: a(5,5),b(5,5)               !a为手动输入并且存在文件里的数组，b需要从文件中读出来并且输出在屏幕上
       
	   write(*,*) "本程序用于将一个5×5的矩阵写入文件data.txt，并将它读出显示出来"
	   write(*,*) "请输入一个5×5的矩阵："
	   read(*,*) a(1,1:5)                             !输入数组中每个元素的值
	   read(*,*) a(2,1:5)
	   read(*,*) a(3,1:5)
	   read(*,*) a(4,1:5)
	   read(*,*) a(5,1:5)

	   open(unit=10,file="data.txt")                  !打开文件并写入数组a
	   write(10,"(5I3)") a(1:5,1:5)	   
	   write(*,*) "您录入的矩阵如下："                !录入告知
	   write(*,"(5I3)") a(1:5,1:5)
	   write(*,*)   "写入成功！"
	   close(unit=10)

	   pause

	   open(unit=10,file="data.txt")                  !打开文件并写入数组b
	   read(10,*) b(1:5,1:5)
	   write(*,*) "为您读出的矩阵是："                !输出所读数组
	   write(*,"(5I3)") b(1:5,1:5)
	   write(*,*)"读出成功！"
	   close(unit=10)

	   pause

	   stop

end program
