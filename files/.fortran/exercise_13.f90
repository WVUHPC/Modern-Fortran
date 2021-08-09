module mod_quaternions

   type quaternion
      real :: a, b, c, d
   end type quaternion

   interface operator(+)
      module procedure sum_qt
   end interface

contains

   function sum_qt(x, y)
      type(quaternion)             :: sum_qt
      type(quaternion), intent(in) :: x, y
      sum_qt%a = x%a + y%a
      sum_qt%b = x%b + y%b
      sum_qt%c = x%c + y%c
      sum_qt%d = x%d + y%d
   end function sum_qt

end module mod_quaternions

program main

   use mod_quaternions

   implicit none

   type(quaternion) :: x = quaternion(1.0, 2.0, 3.0, 4.0)
   type(quaternion) :: y = quaternion(5.0, 6.0, 7.0, 8.0)
   type(quaternion) :: z

   z = x + y

   print *, z%a, z%b, z%c, z%d

end program

