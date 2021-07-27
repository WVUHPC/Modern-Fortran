program alloc_array

   implicit none

   integer, parameter :: n = 10, m = 20
   integer :: i, j, ierror

   real :: a(10)
   real, dimension(10:100, -50:50) :: b
   real, allocatable :: c(:, :)

   real, dimension(:), allocatable :: x_1d
   real, dimension(:, :), allocatable :: x_2d

   allocate (c(-9:10, -4:5))

   allocate (x_1d(n), x_2d(n, m), stat=ierror)
   if (ierror /= 0) stop 'error in allocation'

   do i = 1, n
      x_1d(i) = i
      do j = 1, m
         c(j - 10, i - 5) = j-10 + 0.01*(i-5) ! contiguous operation
         x_2d(i, j) = i + 0.01*j              ! non-contiguous operation
      end do
   end do

   print *,''
   print '(A, 10(F6.2))', 'x_1d:', x_1d(:)

   print *,''
   do j = -9, 10
      print '(A, 10(F6.2))', 'c   :', c(j, :)
   end do

   print *,''
   do i = 1, n
      print '(A, 20(F6.2))', 'x_2d:', x_2d(i, :)
   end do

   deallocate (c, x_1d, x_2d)

end program
