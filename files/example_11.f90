program stack

   implicit none

   integer, parameter :: n = 14490
   integer :: i, j, k
   real :: a(n, n)

   do k = 1, 1
      do i = 1, n
         do j = 1, n
            a(j, i) = j + 1.0/real(n)*i
         end do
      end do
   end do

end program
