module mod_constants

   use iso_fortran_env

   implicit none

   real, parameter :: pi = 3.1415926536
   real, parameter :: e = 2.7182818285

   real(kind=real64), parameter :: elementary_charge = 1.602176634D-19
   real(kind=real64), parameter :: G_grav = 6.67430D-11     ! Gravitational constant
   real(kind=real64), parameter :: h_plank = 6.62607015D-34  ! Plank constant
   real(kind=real64), parameter :: c_light = 299792458       ! Light Speed
   real(kind=real64), parameter :: vacuum_electric_permittivity = 8.8541878128D-12
   real(kind=real64), parameter :: vacuum_magnetic_permeability = 1.25663706212D-6
   real(kind=real64), parameter :: electron_mass = 9.1093837015D-31
   real(kind=real64), parameter :: fine_structure = 7.2973525693D-3
   real(kind=real64), parameter :: Josephson = 483597.8484
   real(kind=real64), parameter :: Rydberg = 10973731.568160
   real(kind=real64), parameter :: von_Klitzing = 25812.80745

contains

   subroutine show_consts()
      print *, "G = ", G_grav
      print *, "h = ", h_plank
      print *, "c = ", c_light
   end subroutine show_consts

end module mod_constants

program physical_constants

   use mod_constants

   implicit none

   print *, sqrt(2.0_real64)
   print *, sqrt(2.0_real128)
   print *, 'Inverse of Fine Structure constant = ', 1.0_real64 / fine_structure

   call show_consts()

end program

