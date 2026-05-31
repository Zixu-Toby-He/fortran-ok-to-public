program A1P3                                  !主程序用于启动、说明与屏幕输出
	implicit none
	integer(kind=4), parameter :: n=12,m=8
	integer(kind=4) :: hairetu,kumiai
	write(*,*) "本程序用于计算n=12,m=8时的排列数P^12_8与组合数C^12_8"

	call calculation(m,n,hairetu,kumiai)

	write(*,"('P^12_8=',I8)") hairetu
	write(*,"('C^12_8=',I3)") kumiai

	stop

end program


subroutine calculation(m,n,hairetu,kumiai)     !本子程序用于需要值的计算
	implicit none
	integer(kind=4):: m,n,hairetu,kumiai
	integer(kind=4), external :: A,C
	
	hairetu=A(m,n)
	kumiai=C(m,n)
	return
end subroutine


function A(m,n)                                  !本函数用于排列数的计算，由于m、n较小用阶乘代码较为容易
	implicit none
	integer(kind=4)::m,n
	integer(kind=4)::A
	integer(kind=4), external::fractional
	A=fractional(n)/fractional(n-m)
	return
end function

function C(m,n)                                  !本函数用于组合数的计算，由于m、n较小用阶乘代码较为容易
	implicit none
	integer(kind=4)::m,n
	integer(kind=4)::C
	integer(kind=4), external::fractional,A
	integer(kind=4)::alpha,beta
	alpha=A(m,n)
	beta=fractional(m)
	C=A(m,n)/fractional(m)
	return
end function

function fractional(a)                           !本函数用于计算阶乘，由于m、n较小不会超出范围，故不需要更复杂的数据结构
	implicit none
	integer(kind=4)::a
	integer(kind=4)::fractional
	integer(kind=4)::i=1
	fractional=1

	do while(i<=a)
		fractional=fractional*i
		i=i+1
	end do
	i=1
	return
end function
