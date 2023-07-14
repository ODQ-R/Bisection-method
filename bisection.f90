module basic_types

    implicit none
	
    private
	
    public :: int4, real8
	
    ! define basic types
    integer, parameter :: int4	= selected_int_kind(8)
    integer, parameter :: real8 = selected_real_kind(p=15)
	
end module basic_types
	
module io_files
use basic_types
	
	implicit none
	
	private
	
	public :: output_unit, result_file_unit
	
	! define basec units
	integer(int4), parameter :: output_unit = 6       ! sreen
	integer(int4), parameter :: result_file_unit = 30 ! results file
	
end module io_files
	
program bisection
use io_files
use basic_types

	implicit none

	integer(int4) :: i, maxIterations
	real(real8) :: xl, xu, xr, tol

	xl = 0.5_real8		! Lower interval
	xu = 1.0_real8		! Upper interval
	tol = 1e-4		! Desired tolerance for the root
	maxIterations = 100	! Maximum number of iterations

	! Bisection method
	do i = 1, maxIterations
		
		xr = (xl + xu) / 2.0 
		if (f(xl) * f(xr) < 0) then
			xu = xr    
		else
			xl = xr    
		end if
		
		if(i == 1) write(output_unit,'(4X,A16)') "BISECTION Method" 
		if(i == 1) write(output_unit,*)
		if(i == 1) write(output_unit,'((8X,A2))') "xr"
		write(output_unit,'((4X,F9.4))') xr
		
		! Check convergence
        if (abs(f(xr)) < tol) then
            exit
	end if
	
	end do

	! Plot the results	
	write(output_unit,*)
	write(output_unit,'((8X,A4,10X,A2))') "f(x)", "xr"
	write(output_unit,'((4X,F9.3,4X,F9.4))') abs(f(xr)), xr
	write(output_unit,*)
	
	write(output_unit,'(5X,A30,2X,I3,2X,A2,2X,I3)') "** Converged in the iteration:", i, "of", maxIterations 
  	write(output_unit,*)
	
	contains 
	
	! Function declaration
	real function f(x)
		real(real8), intent(in) :: x
		f = x**10 - 1
	end function f

end program bisection
