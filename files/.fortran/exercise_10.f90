program main

   implicit none

   real, allocatable, dimension(:) :: x
   integer :: rnd_nseed, n
   integer, allocatable, dimension(:) :: rnd_seed

   print *, 'Enter number of elements'
   read *, n

   allocate (x(n))

   call random_seed(size=rnd_nseed)
   print *, 'Random Seed (n)    : ', rnd_nseed
   allocate(rnd_seed(rnd_nseed))
   call random_seed(get=rnd_seed)
   print *, 'Random Seed (array): ', rnd_seed 

   call random_number(x)

   print *, 'Number of elements :', size(x)
   print *, 'Sum of values in array : ', sum(x) 
   print *, 'Average : ', sum(x)/size(x)

   deallocate (x)

   stop

end program
