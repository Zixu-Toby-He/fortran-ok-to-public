!本程序用于用不同数值方法解方程x^3-3*x=0
!要通过不同的数值方法找到方程的所有根，并比较误差与收敛速度
!本程序调用文件时用的是路径查找文件，如需再次实现，请删去路径成分
program H2Problem
	implicit none
	real(kind=8) :: error
	integer(kind=1) :: i

	interface
		subroutine root_search(error)
		implicit none
		real(kind=8) :: error
		end subroutine
	end interface

	write(*,*) " 现在开始用不同的方法找方程x^3-3*x=0的根"
	write(*,*) " 通过其他方法已知方程解析解为0与±√3，现在通过数值方法在精确度10^(-12)下找到这些根"
	write(*,*) " 通过知道解析解，我们可以大幅简化数值解中迭代结果不同但表示的是同根的判断，而且通过前期实验已经确认总能找到初值能够得到相应解的"
	write(*,*) " 由于每个迭代法子程序都加入了输出迭代次数的功能，故可以通过观察迭代次数同时得到收敛快慢的信息"
	pause
	call root_search(0.000000000001d0)
	write(*,*) " "

	write(*,*) " "
	write(*,*) "  现在进行误差极限分析，即迭代到最后x_(k+1)=x_k时的误差。该误差可认为是由算法本身、计算机数据存储造成的"
	write(*,*) "  该结果可以用来评估算法的计算机实现的可能性"
	write(*,*) "  以下为了统一，统一从3开始向正根迭代"
	write(*,*) " "
	call error_limit()
	write(*,*) " "

	pause
	stop
end program

include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Methords\Aitken.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Methords\Jacobi.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Methords\Newton downhill.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Methords\Post acceleration.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\initiate\original function.f90"

include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\Steps\root search.f90"
include "C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\Steps\error limit.f90"
