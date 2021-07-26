program f95attributes

   implicit none

   integer :: i, j
   integer, parameter :: n = 100
   real :: x
   real, parameter :: pi = 3.141592653
   real, dimension(n) :: array
   real, dimension(:, :), allocatable :: dyn_array2d

   allocate (dyn_array2d(2*n, 2*n))

   do i = 1, 2*n
      do j = 1, 2*n
         dyn_array2d(j, i) = j + 0.0001*i
      end do
   end do

   do i = 1, 10
        print '(10(F9.4))', dyn_array2d(i,1:10)
   end do

end program
