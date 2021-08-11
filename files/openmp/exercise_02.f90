program main

use omp_lib

implicit none

integer :: i, thread_id

!$OMP PARALLEL PRIVATE(thread_id)
    thread_id = OMP_GET_THREAD_NUM()

    do i=0, OMP_GET_MAX_THREADS()
        if (i == thread_id) then
            print *, "Hello OpenMP from thread: ", thread_id
        end if
        !$OMP BARRIER
    end do
!$OMP END PARALLEL

stop

end program

