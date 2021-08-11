program main

   use omp_lib

   integer :: id, nt

   !$OMP PARALLEL private(id, nt)
   id = omp_get_thread_num()
   nt = omp_get_num_threads()
   print *, "Hello OpenMP", id, ' of ', nt
   !$OMP END PARALLEL

   print *, omp_get_wtime()

end program
