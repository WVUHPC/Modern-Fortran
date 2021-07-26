program main

   implicit none

   integer :: i, j
   real :: s1

   real, dimension(10, 10) :: c1010, d1010

   s1 = 0.125

   d1010 = 0.0
   d1010(1:9:2, 1:9:2) = reshape([((sin(real(i))*sin(real(j)), i=1, 5), j=1, 5)], [5, 5])
   d1010(2:10:2, 2:10:2) = reshape([((cos(real(i))*cos(real(j)), i=1, 5), j=1, 5)], [5, 5])

   print *, char(10), ' d1010: '
   do i = 1, 10

      print "(*(F9.4))", (d1010(i, j), j=1, 10)
   end do

   c1010 = s1
   c1010(1:9:2, 1) = d1010(1, 1:9:2)

   print *, char(10), 'c1010: '
   print "(*(g0))", ((abs(c1010(i, j)), " ", j=1, 10), new_line("A"), i=1, 10)

end program
