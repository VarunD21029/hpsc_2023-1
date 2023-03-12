program square_root
        implicit none 
        real (kind=8) :: x1,y1,tol1
        integer :: s1,kmax1 
        s1=1
        kmax1=100
        tol1=1.0e-14
        x1=2
        call mysqrt(x1,y1)
        print *, "square root is ", y1
end program square_root
        
subroutine mysqrt(x,y,s,kmax,tol)
 ! calculates the square root of the function using Newton`s method
        implicit none
        real (kind=8), intent(in) :: x,tol
        real (kind=8), intent(out) :: y
        integer, intent(in) :: kmax
        integer ::i,s0,delta_s
        do i=1,kmax
        s0=y(i)
        y(i)=0.5*(y(i)+x/y(i))
        delta_s=y(i)-s0
        if((abs(delta_s/x))<tol) EXIT
        enddo
end subroutine mysqrt

        
