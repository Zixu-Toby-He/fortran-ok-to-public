program H4
	implicit none
	real(kind=8) :: x_read(16),y_read(16)
	interface
		subroutine readin(x,y)
			implicit none
			real(kind=8) :: x(16),y(16)
		end subroutine
		subroutine Lagrange(x_read,y_read)
			implicit none
			real(kind=8) :: x_read(16),y_read(16)
		end subroutine
		subroutine Newton(x_read,y_read)
			implicit none
			real(kind=8) :: x_read(16),y_read(16)
		end subroutine
		subroutine Cubic_spline_curve(x_read,y_read)
			implicit none
			real(kind=8) :: x_read(16),y_read(16)
		end subroutine
	end interface

	write(*,*) "本程序用于插值法计算拟合曲线"
	write(*,*) "给定点，通过不同的插值法作出通过这些点的平滑曲线"
	write(*,*) "以下是这些点的坐标，一共16个点"
	write(*,*) "y=1/(x^2+1),x=-5+10/15*i,0<=i<=15"
	write(*,*) "这些点横竖坐标分别储存在Assistant文件夹的两个txt文件中"
	write(*,*) "通过插值算出来的点将储存在result文件夹中对应的csv文件中"
	write(*,*) "利用Excel读取csv文件，可以进行后续画图、转换格式等操作"
	write(*,*) " "
	pause

	write(*,*) "现在开始读取数据"
	call readin(x_read,y_read)
	write(*,*) "读取已完成"
	write(*,*) " "
	pause

	write(*,*) "现在开始拉格朗日插值法计算，插入后总共包含6001个点"
	call Lagrange(x_read,y_read)
	write(*,*) "拉格朗日插值法计算完成"
	write(*,*) "通过作图分析可以看出，该方法能够作出平滑曲线"
	write(*,*) "在原函数在|x|<=约0.3的位置与原函数相差不大，但该区间外则有较大偏差"
	pause

	write(*,*) "现在开始牛顿插值法计算，插入后总共包含6001个点"
	call Newton(x_read,y_read)
	write(*,*) "牛顿插值法计算完成"
	write(*,*) "这个方法得到的结果和拉格朗日法几乎完全相同"
	write(*,*) "这是因为这两种方法展开来都是幂级数，其对应的各个系数应该完全一致"
	pause

	write(*,*) "现在开始三次样条曲线插值法计算，插入后总共包含6001个点"
	call Cubic_spline_curve(x_read,y_read)
	write(*,*) "三次样条曲线插值法计算完成"
	write(*,*) "这个方法得到的结果相比较前两种更接近于原函数"
	write(*,*) "前两种方法由于龙格效应而在距离“数据中心”较远位置产生剧烈抖动"
	write(*,*) "根据资料了解到这与原函数泰勒多项式不收敛有关"
	write(*,*) "而三次样条曲线插值法中以两点为基本单位则保证每一点都在“数据中心”附近"
	write(*,*) "这在一定程度上避免这个问题"

	pause
	stop
end program

include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Methord\Lagrange.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Methord\Newton.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Methord\Cubic spline curve.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\original function.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\Readin.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\Cubic spline special\Gaussian elimination.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework4\Assistant\Cubic spline special\Triangle solve.f90"


