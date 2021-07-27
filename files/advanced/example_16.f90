module mod_public_private

   implicit none

   public

   real, parameter :: pi = 3.141592653, &
                      c = 299792458, &
                      e = 2.7182818285

   real, private :: rad_2_deg = 180.0/pi
   real, private :: deg_2_rad = pi/180.0

   private :: sin_deg, cos_deg

   public :: tan_deg

contains

   function sin_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = sin(x*deg_2_rad)
   end function

   function cos_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = cos(x*deg_2_rad)
   end function

   function tan_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = sin_deg(x)/cos_deg(x)
   end function

end module mod_public_private

program priv_pub_module

   use mod_public_private

   implicit none

   real :: r = 2.0

   print *, 'Area = ', pi*r**2

   ! This print will not work
   ! The variables rad_2_deg and deg_2_rad are private
   !print *, rad_2_deg, deg_2_rad

   print *, 'Tan(45) ', tan_deg(45.0)

   ! These lines will not work as functions are private
   !print *, 'Sin(45) ', sin_rad(45.0)
   !print *, 'Cos(45) ', cos_rad(45.0)

end program

