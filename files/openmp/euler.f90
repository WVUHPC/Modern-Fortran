program reduction

   implicit none
   integer, parameter :: r15 = selected_real_kind(15)
   integer, parameter :: n = 40
   integer :: i, j

   real(kind=r15) :: euler = 1.0
   real(kind=r15) :: facto
   real(kind=r15), parameter :: e_ref = 2.71828182845904523536028747135266249775724709369995_r15

   !$OMP PARALLEL DO default(none) shared(euler) private(facto) reduction(+:euler)
   do i = 1, n

      facto = real(1)

      ! Not need to run this internal loop in the serial case
      ! Can you remove it and still use OpenMP?
      do j = 1, i
         facto = facto*j
      end do
      euler = euler + real(1)/facto
      print *, "I= ", i, "Euler Number: ", euler, "Error: ", abs(euler - e_ref)

   end do
   !$OMP END PARALLEL DO

   print *, "Final Euler Number: ", euler

end program
