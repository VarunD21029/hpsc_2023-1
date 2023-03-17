! A fortran code to calculate the square root of a no. using newton method
program square_root
        implicit none 
        ! x the no. for which the suare root to be calculated
        ! y1 is the output of the code not required in this version
        ! tol is the tolerence
        ! s_init is the initial
        ! debug is for printing the iteration details
        real (kind=8) :: x1, y1, tol, s_init
        logical :: debug
        integer :: n, kmax
        x1=100
        tol=1.0e-14
        kmax=1000
        s_init=1
        debug = .True.
        ! sqrt_sub is the subroutine for finding root by Newton's method
        call sqrt_sub(x1,y1,n,kmax,s_init,tol,debug)
end program square_root
        
subroutine sqrt_sub(x,sqrtN,iter,kmax,s_init,tol,debug)
 ! calculates the square root of the function using Newton`s method
        implicit none
        real (kind=8), intent(in) :: x, tol, s_init
        logical, intent(in) :: debug
        integer, intent(in) :: kmax
        real (kind=8), intent(inout) :: sqrtN
        integer, intent(inout) :: iter
        ! local variable
        real (kind=8) :: delta_s, s0
        sqrtN=s_init
        if (x==0) then
                sqrtN=0
                iter=0 
                print *, "The square root is zero"
        else if (x.lt.0) then 
                print *, "Error value of x is negative" 
        else 
                do iter=1,kmax 
                s0=sqrtN
                if (debug.eqv..True.) then
                        print *, "After iteration no. ", iter,"s =",sqrtN
                end if
                sqrtN=0.5d0*(sqrtN+x/sqrtN)
                delta_s= sqrtN-s0
                if(abs(delta_s/x).LT.tol) EXIT 
                enddo
                print *, "The square root value is:", sqrtN
                print *, "The process converges at iteration no.", iter    
        end if
end subroutine sqrt_sub

        
