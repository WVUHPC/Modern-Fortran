program use_structs

   implicit none

   integer :: i

   ! Electron Configuration
   ! Example: [Xe] 6s1 4f14 5d10
   type electron_configuration

      character(len=3) :: base_configuration
      integer, allocatable, dimension(:) :: levels
      integer, allocatable, dimension(:) :: orbitals
      integer, allocatable, dimension(:) :: n_electrons

   end type electron_configuration

   ! Information about one atom
   type atom

      integer :: Z
      character(len=3) symbol
      character(len=20) name
      integer, allocatable, dimension(:) :: oxidation_states
      real :: electron_affinity, ionization_energy
      type(electron_configuration) :: elec_conf ! structure

   end type atom

   ! Structures (Variables) of the the derived type my_struct
   type(atom) :: gold
   type(atom), dimension(15) :: lanthanide

   gold%Z = 79
   gold%symbol = 'Au'
   gold%name = 'Gold'
   allocate (gold%oxidation_states(2))
   gold%oxidation_states = [3, 1]
   gold%electron_affinity = 2.309
   gold%ionization_energy = 9.226
   allocate (gold%elec_conf%levels(3))
   allocate (gold%elec_conf%orbitals(3))
   allocate (gold%elec_conf%n_electrons(3))
   gold%elec_conf%base_configuration = 'Xe'
   gold%elec_conf%levels = [6, 4, 5]
   gold%elec_conf%orbitals = [1, 4, 3]
   gold%elec_conf%n_electrons = [1, 14, 10]

   print *, 'Atom name', gold%name
   print *, 'Configuration:', gold%elec_conf%base_configuration
   do i = 1, size(gold%elec_conf%levels)
      print '(3(I4))', &
         gold%elec_conf%levels(i), &
         gold%elec_conf%orbitals(i), &
         gold%elec_conf%n_electrons(i)
   end do
end program
