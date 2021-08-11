program main

   use omp_lib

   implicit none

   integer, parameter :: k15 = selected_real_kind(15)
   real(kind=k15) :: partial_prod, total_prod
   integer :: i
   real(kind=k15)    :: z=0.25 

!$OMP PARALLEL PRIVATE(partial_prod) SHARED(total_prod)
   partial_prod = 1; 
   total_prod = 1; 
   !$OMP DO
   do i = 1, huge(i)
      partial_prod = partial_prod * (1.0 - z**2 / real(i)**2)
   end do
   !$OMP END DO

   !$OMP CRITICAL
   total_prod = total_prod * partial_prod
   !$OMP END CRITICAL

!$OMP END PARALLEL
   print *, "Total Prod: ", total_prod

end program
