program just_1_bit
	integer(kind=1) :: i=1

	do while(i>=0)
		write(*,*) i
		i=i+1
	end do

end program

!结果输出到127，即kind=1的整数最大到127
!kind=2最大输出到约32767
!kind=4为214748367
!i=4/3与i=5/3得到结果均为1，故该过程取小，没有四舍五入
!i=-4/3与i=-5/3得到结果均为-1，故该过程绝对值取小