program ratio

   implicit none

   integer, parameter :: n = 40
   real, parameter :: pi = 3.14159265359

   integer :: i, cube_side
   real :: area_to_volume_S(n), area_to_volume_P(n), surface_sphere, volume_cube, diameter

   do i = 1, n
      cube_side = i
      diameter = sqrt(3*real(cube_side)**2)
      surface_sphere = pi*diameter**2
      volume_cube = real(cube_side)**3
      area_to_volume_S(i) = surface_sphere/volume_cube
   end do

!$OMP PARALLEL DO default(none)
   do i = 1, n
      cube_side = i
      diameter = sqrt(3*real(cube_side)**2)
      print *, i, cube_side
      surface_sphere = pi*diameter**2
      volume_cube = real(cube_side)**3
      area_to_volume_P(i) = surface_sphere/volume_cube
   end do
!$OMP END PARALLEL DO

   do i = 1, n
      if (abs(area_to_volume_S(i) - area_to_volume_P(i)) > 1e-2) then
         print *, "Wrong value for i=", i, area_to_volume_S(i), area_to_volume_P(i)
      end if
   end do

end program ratio
