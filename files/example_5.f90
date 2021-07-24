program do_exit

   implicit none

   integer :: i
   real, parameter :: pi = 3.141592653
   real :: x, y

   do i = 0, 360, 10
      x = cos(real(i)*pi/180)
      y = sin(real(i)*pi/180)

      if (abs(x) < 1E-7) then
         print *, "Small denominator (exit)"
         exit
      end if
      print *, i, x, y, y/x
   end do

   print *, 'Final values:', i, x

end program
