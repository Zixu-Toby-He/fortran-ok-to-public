program pi_k
	implicit none
	real(kind=8) :: pi,pi_new,pi_old,add,i,error
	real(kind=8) :: time_start,time_end


	call cpu_time(time_start)

	error=1d-10
	i=0d0
	add=1d0/(2d0*i+1)
	pi_new=0d0
	
	write(*,*) error
	do while(add>=error)
		pi_old=pi_new
		add=1d0/(2d0*i+1d0)
		pi_new=pi_new+add
		i=i+1d0
	end do
	pi=4d0*pi_new
	write(*,*) pi

	call cpu_time(time_end)
	write(*,*) (time_end-time_start)
	stop
end program