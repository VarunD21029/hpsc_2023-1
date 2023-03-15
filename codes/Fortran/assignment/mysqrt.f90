program square_root
        implicit none 
        real (kind=8) :: x1,y1
        integer :: s1,kmax1 
        kmax1=1000
        x1=2
        call mysqrt(x1,y1,kmax1)
        print *, "square root is ", y1
end program square_root
        
subroutine mysqrt(x,y,kmax)
 ! calculates the square root of the function using Newton`s method
        implicit none
        real (kind=8), intent(in) :: x
        real (kind=8), intent(out) :: y
        real (kind=8) :: tol, s0, delta_s
        integer, intent(in) :: kmax
        integer ::i
        y=1
        tol=1.0e-14
        do i=1,kmax
        s0=y
        y=0.5d0*(y+x/y)
        delta_s=y-s0
        if(abs(delta_s/x).LT.tol) EXIT
        enddo
end subroutine mysqrt

        
