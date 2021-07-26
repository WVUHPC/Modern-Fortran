program main

   implicit none

   integer :: i, j
   real :: s1
   real, dimension(10) :: a10, b10
   real, dimension(4) :: x4 = [1., 2., 3., 4.]  ! Fortran 2003
   real, dimension(5) :: x5 = (/1., 2., 3., 4., 5./) ! Old Fortran way
   real, dimension(4) :: y4, z4
   real, dimension(8) :: z8
   real, allocatable :: xx(:)
   real, dimension(:, :), allocatable :: yy

   s1 = 1.25

   y4 = [-1., 0., 1., 2.]
   z4(1:4) = [(sqrt(real(i)), i=1, 4)]
   z8(1:7:2) = [(sqrt(real(i)), i=1, 4)]
   z8(2:8:2) = [(sqrt(real(i)), i=1, 4)]

   print *, 'x4: ', x4
   print *, 'y4: ', y4
   print *, 'z4: ', z4
   print *, 'z8: ', z8

   b10(1:9:2) = [(sin(real(i)), i=1, 5)]
   b10(2:10:2) = [(cos(real(i)), i=1, 5)]

   print *, 'b10: ', b10

   a10 = b10

   print *, 'a10: ', a10

   a10(1:10) = b10(1:10)
   a10(2:3) = b10(4:5)
   a10 = s1
   a10(1:3) = b10(1:5:2) ! a10(1) = b10(1)
   !                     ! a10(2) = b10(3)
   !                     ! a10(3) = b10(5)

   print *, 'a10: ', a10
   print *, 'b10: ', b10

end program
