program mysqrt
! Function called sqrtNT(x) returns sqrt using Newton's method
        implicit none
        real (kind=8) :: x,y,sqrt_true
        real (kind=8), external:: sqrtNT 
        x = -2.d0
        sqrt_true = sqrt(x)
        y = sqrtNT(x)  ! Uses our created sqrt function
        print *, "x= ",x
        print *, "true square root= ", sqrt_true
        print *, "my_sqrt=", y
        print *, "error =", sqrt_true - y
end program mysqrt

function sqrtNT(x)
        implicit none       
        real (kind=8), intent(in) :: x
        real (kind=8) :: sqrtNT
        !local variables
        real (kind=8) :: tol,s0,delta_s,y
        integer :: i,kmax
        kmax=100
        y=1
        tol=1.d-14
        do i=1,kmax
        s0=y
        y=0.5d0*(y+x/y)
        delta_s=y-s0
        if(abs(delta_s/x).LT.tol) EXIT
        enddo
        if (i.ge.kmax) then
                print *, "Newton's method did not converge for kmax iterations", kmax
        endif
                sqrtNT=y
end function sqrtNT
