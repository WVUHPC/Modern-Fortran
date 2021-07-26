program square

   implicit none

   real :: x, x2
   integer :: i

   do i = 1, 100
      x = i
      x2 = x*x
      if (x2 < 100) &
         print *, 'X=', I, ' X^2 wil have less than 3 digits'
   end do

end program

