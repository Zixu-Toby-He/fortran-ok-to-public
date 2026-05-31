!在本程序单位制下G=1
!本程序仅模拟二维情况下的潮汐瓦解

program TidalDisruption
	implicit none
	integer(kind=4),parameter :: ObjectNumber=4                               !等效天体个数（4个）
	real(kind=8) :: mass_center,mass_object                                   !mass_center为中心大质量星体质量，mass_object为被瓦解星体质量的1/ObjectNumber
	real(kind=8) :: location(2,ObjectNumber),velocity(2,ObjectNumber)         !物体的x、y、z坐标与速度
	real(kind=8) :: delta_t                                                   !时间t的步长

	interface
		subroutine initiation(mass_center,mass_object,location,velocity,ObjectNumber)
			implicit none
			integer(kind=4) :: ObjectNumber
			real(kind=8) :: mass_center,mass_object,mass_star
			real(kind=8) :: location(2,ObjectNumber),velocity(2,ObjectNumber)
		end subroutine
		subroutine evolution(mass_center,mass_object,location,velocity,ObjectNumber,delta_t)
			implicit none
			integer(kind=4) :: ObjectNumber
			real(kind=8) :: mass_center,mass_object
			real(kind=8) :: location(2,ObjectNumber),velocity(2,ObjectNumber)
			real(kind=8) :: delta_t
		end subroutine
	end interface

	
	delta_t=1d-7

	call initiation(mass_center,mass_object,location,velocity,ObjectNumber)            !初始化（在距离原点r处半径为d均匀分布）
	call evolution(mass_center,mass_object,location,velocity,ObjectNumber,delta_t)     !演进（根据时间与动力学）

	pause
	stop
end program


!演进（根据时间与动力学）
subroutine evolution(mass_center,mass_object,location,velocity,ObjectNumber,delta_t)
	implicit none
	integer(kind=4) :: ObjectNumber                                           !等效天体个数（4个）
	real(kind=8) :: mass_center,mass_object                                   !mass_center为中心大质量星体质量，mass_object为被瓦解星体质量的1/ObjectNumber
	real(kind=8) :: location(2,ObjectNumber),velocity(2,ObjectNumber)         !物体的x、y坐标与速度
	real(kind=8) :: location_center(2)                                        !质心的x、y坐标
	real(kind=8) :: force(2,ObjectNumber)                                     !物体在x、y方向上的受力
	real(kind=8) :: delta_t                                                   !时间t的步长
	
	integer(kind=4) :: counter                                                !计数变量

	interface
		subroutine GetForce(mass_center,mass_object,location,force,ObjectNumber)
			integer(kind=4) :: ObjectNumber
			real(kind=8) :: mass_center,mass_object
			real(kind=8) :: location(2,ObjectNumber)
			real(kind=8) :: force(2,ObjectNumber)
		end subroutine
	end interface

	counter=1

	open(unit=21,file="Object.csv")
	open(unit=22,file="相对位置.csv")
	open(unit=23,file="相对位置2.csv")

	location_center(1)=sum(location(1,:))/Objectnumber
	location_center(2)=sum(location(2,:))/Objectnumber

	write(21,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,1),location(2,1),location(1,2),location(2,2),location(1,3),location(2,3),location(1,4),location(2,4)
	write(22,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,2)-location(1,1),location(2,2)-location(2,1),location(1,3)-location(1,1),location(2,3)-location(2,1),location(1,4)-location(1,1),location(2,4)-location(2,1)
	write(23,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,1)-location_center(1),location(2,1)-location_center(2),location(1,2)-location_center(1),location(2,2)-location_center(2),location(1,3)-location_center(1),location(2,3)-location_center(2),location(1,4)-location_center(1),location(2,4)-location_center(2)

	do while(counter<=1249900*5)
		call GetForce(mass_center,mass_object,location,force,ObjectNumber)
		location=location+(velocity+force*delta_t/(2d0*mass_object))*delta_t
		velocity=velocity+force*delta_t/mass_object
		location_center(1)=sum(location(1,:))/Objectnumber
		location_center(2)=sum(location(2,:))/Objectnumber
		if(counter==(counter/1000)*1000) then
			write(21,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,1),location(2,1),location(1,2),location(2,2),location(1,3),location(2,3),location(1,4),location(2,4)
			write(22,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,2)-location(1,1),location(2,2)-location(2,1),location(1,3)-location(1,1),location(2,3)-location(2,1),location(1,4)-location(1,1),location(2,4)-location(2,1)
			write(23,"(e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',',',',e35.25,',',e35.25,',')") location(1,1)-location_center(1),location(2,1)-location_center(2),location(1,2)-location_center(1),location(2,2)-location_center(2),location(1,3)-location_center(1),location(2,3)-location_center(2),location(1,4)-location_center(1),location(2,4)-location_center(2)
		end if
		counter=counter+1
	end do

	close(21)
	close(22)
	close(23)


	return
end subroutine

!计算各个物体的受力
subroutine GetForce(mass_center,mass_object,location,force,ObjectNumber)
	integer(kind=4) :: ObjectNumber
	real(kind=8) :: mass_center,mass_object

	real(kind=8) :: location(2,ObjectNumber)
	real(kind=8) :: force(2,ObjectNumber)
	
	real(kind=8) :: distance                             !取两点的距离
	integer(kind=4) :: counter(2)                        !计数变量

	distance=0d0
	counter=1

	force=0

	do while(counter(1)<=ObjectNumber)
		distance=sqrt(location(1,counter(1))**2+location(2,counter(1))**2)                                                                 !计算与中心天体的距离
		force(1,counter(1))=-(mass_center*mass_object)/(distance**3)*location(1,counter(1))                                                !计算中心天体x方向产生的引力
		force(2,counter(1))=-(mass_center*mass_object)/(distance**3)*location(2,counter(1))                                                !计算中心天体y方向产生的引力
		counter(2)=1
		do while(counter(2)<=ObjectNumber)                                                                                                 !其他天体
		distance=sqrt((location(1,counter(1))-location(1,counter(2)))**2+(location(2,counter(1))-location(2,counter(2)))**2)               !计算两个天梯距离
			if (distance>=1d-15) then                                                                                                      !判断是不是自己，是自己不计算引力
				force(1,counter(1))=force(1,counter(1))-(mass_object**2)/(distance**3)*(location(1,counter(1))-location(1,counter(2)))     !计算其他天体x方向产生的引力
				force(2,counter(1))=force(2,counter(1))-(mass_object**2)/(distance**3)*(location(2,counter(1))-location(2,counter(2)))     !计算其他天体y方向产生的引力
			end if
			counter(2)=counter(2)+1                                                                                                        !计算下一个施力物体
		end do
		counter(1)=counter(1)+1                                                                                                            !计算下一个受力物体
	end do

	return
end subroutine



!初始化（在距离原点r处半径为d均匀分布）
subroutine initiation(mass_center,mass_object,location,velocity,ObjectNumber)
	implicit none
	real(kind=8), parameter :: pi=3.141592653589793d0                         !常数pi=3.14
	
	!这些是分析的时候要用的参数的初始值
	integer(kind=4) :: ObjectNumber                                           !等效天体个数（4个）
	real(kind=8) :: mass_center,mass_object,mass_star                         !mass_center为中心大质量星体质量，mass_object为被瓦解星体质量mass_star的1/ObjectNumber
	real(kind=8) :: location(2,ObjectNumber),velocity(2,ObjectNumber)         !物体的x、y坐标与速度

	!这些是给出的初始数据
	real(kind=8) :: r,d                                                       !r是质心到原点距离，d为等效半径
	real(kind=8) :: v_0,spin,theta                                            !v_0为初始速率，spin为自转角速度，theta为初速度与r方向的夹角

	!这是计数变量
	integer(kind=4) :: counter

	d=3d-1
	r=1d1
	mass_center=1d5
	mass_star=4d0
	v_0=20d0
	spin=v_0/r
	theta=pi/2d0

	mass_object=mass_star/(Objectnumber*1d0)
	counter=1


	do while(counter<=ObjectNumber)
		location(1,counter)=r+d*cos(2d0*pi/ObjectNumber*(counter-1d0))
		location(2,counter)=d*sin(2d0*pi/ObjectNumber*(counter-1d0))
		velocity(1,counter)=v_0*cos(theta)+spin*d*sin(2d0*pi/ObjectNumber*(counter-1d0))
		velocity(2,counter)=v_0*sin(theta)+spin*d*cos(2d0*pi/ObjectNumber*(counter-1d0))
		counter=counter+1
	end do
	
	return
end subroutine

