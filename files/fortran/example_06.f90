program main

   type position
      real :: x, y, z
   end type position

   type atom
      character(20) :: atom_name
      character(3)  :: symbol
      integer       :: atom_Z
      real          :: electronegativity
      type(position):: atom_pos
   end type atom

   type(atom) :: gold = atom('Gold', 'Au', 79, 2.54, position(1.0, -1.125, 3.5))

   gold%atom_Z = 79

   print *, 'Name       : ', gold%atom_name
   print *, 'Z          : ', gold%atom_Z
   print *, 'Position Z : ', gold%atom_pos%z

end program
