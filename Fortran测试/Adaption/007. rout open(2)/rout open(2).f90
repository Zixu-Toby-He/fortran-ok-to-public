program rout_open_2
	implicit none
	real(kind=8) :: error
	real(kind=8) :: initials(41)


	interface
		subroutine get_initials(initials)
			implicit none
			real(kind=8) :: initials(41)
		end subroutine
	end interface

!	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\initiate\initial.txt")
!	read (10,*) initials
!	close(unit=10)

	call get_initials(initials)

	write(*,*) initials

end program

subroutine get_initials(initials)
	implicit none
	real(kind=8) :: initials(41)
	
	open(unit=10,file="C:\Program Files\Microsoft Visual Studio\My code\homework2\Prob\Assistant\initiate\initial.txt")
	read (10,*) initials
	close(unit=10)

	return
end subroutine