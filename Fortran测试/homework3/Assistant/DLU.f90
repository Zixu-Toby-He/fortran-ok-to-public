!本子程序将方阵A分解为一个上三角矩阵-U和对角矩阵D以及下三角矩阵-L的和，即A=D-U-L
!D、L、U这三个参数在迭代法计算中很重要

subroutine DLU(A,D,L,U)
	implicit none
	integer(kind=1),parameter :: lines=9
	real(kind=8) :: A(lines,lines),D(lines,lines),L(lines,lines),U(lines,lines)
	integer(kind=1) :: counter(2)

	counter=1
	D=0
	L=0
	U=0
	do while (counter(1)<=lines)
		D(counter(1),counter(1))=A(counter(1),counter(1))
		counter(2)=counter(1)+1
		do while (counter(2)<=lines)
			U(counter(1),counter(2))=A(counter(1),counter(2))
			L(counter(2),counter(1))=A(counter(2),counter(1))
			counter(2)=counter(2)+1
		end do
		counter(1)=counter(1)+1
	end do
	L=-1d0*L
	U=-1d0*U
	return
end subroutine