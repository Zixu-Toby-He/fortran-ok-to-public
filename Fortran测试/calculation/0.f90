!本程序用于解微分方程
program calculation
	implicit none
	real(kind=8) :: beta,k,T
	real(kind=8) :: r,V
	real(kind=8) :: delta_r,V_div,intg
	real(kind=8) :: equation_right
	integer(kind=4) :: counter

	r=1d-11
	V=0d0
	V_div=0d0
	intg=0d0
	delta_r=1d4
	k=1.380649d-23
	T=1d7
	beta=1d0/(k*T)
	counter=1

	open(unit=11,file="result.csv")

	do while(counter<=1000)
		intg=intg+equation_right(r,V,beta)
		V_div=intg/(r*r)
		V=V+delta_r*V_div
		write(11,"(F30.20,',',F30.20)") r,V
		r=r+delta_r
		counter=counter+1
	end do

	close(11)

	pause

end program


function equation_right(r,V,beta)
	implicit none
	real(kind=8) :: r,V,beta
	real(kind=8) :: equation_right
	real(kind=8) :: e=2.7182818284590452353602874d0
	real(kind=8) :: pi=3.141592653589793d0
	real(kind=8) :: G=6.67259d-11
	
	equation_right=-4d0*pi*G*(r*r)*(e**(beta*V))

	return
end function