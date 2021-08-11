program main

   integer :: n = huge(1), i
   real :: a = 3.0
   real, allocatable :: x(:), y(:)

   allocate (x(n), y(n))
   x(1:n) = 2.0
   y(1:n) = 1.0

!$acc kernels
   do i = 1, n
      y(i) = sqrt(a)*sqrt(x(i)) + sqrt(y(i))
   end do
!$acc end kernels

end program main
