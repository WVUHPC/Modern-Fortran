---
title: "OpenACC: Parallel Accelerators"
start: 840
teaching: 60
exercises: 0
questions:
- ""
objectives:
- "How to write code for GPUs from a directive-base model?"
keypoints:
- "OpenACC is another directive-base model for parallel programming."
- "OpenACC is similar to OpenMP but take advantage of accelerators such as GPUs"
---

# OpenACC

OpenACC is another directive-based model for parallel programming.
It is similar to OpenMP but focused more on Accelerators rather than Multicore processors as OpenMP.

Similar to OpenMP, the idea is to take the original code and introduce directives in places that could benefit from parallel programming.
If you use only directives, the code still compiles with compilers that do not support OpenACC and the code will run serially as usual.

OpenACC is another directive-based approach for parallel programming with a more general scope than the original OpenMP.
Before version 4.0, OpenMP was designed to provide parallelism to multicore systems, an alternative to more tedious alternatives like programming with pthreads.
With the advent of accelerators, like GPUs, a more general paradigm was needed that preserve the nice features that OpenMP bring to parallel computing.

An accelerator is, in general, a device that is able to perform certain classes of computations with more performance than the CPU.
Accelerators such as GPUs come with a high bandwidth memory that is separated from the main CPU RAM.
Any processing that happens in an accelerator must first transfer the data from the CPU memory into the device memory and at the end of the calculation, move the data back.

Currently, the best support for OpenACC comes from NVIDIA compilers (nvfortran), followed by GCC compilers (gfortran). Intel compilers do not support OpenACC and there is no plan for support it in the near future.

In this lesson, we will follow a similar path as we did for OpenMP.
First, we parallelize a loop and later we generalize the operation to more abstract parallel entities, such as tasks.
Most of the complexity added to OpenACC compared to OpenMP comes from the fact that accelerators usually work on their own memory space, meaning that to use them, data must be transferred into the accelerator and final data move it back to CPU memory.
That is more complex than the way OpenMP works where data is always in CPU memory and threads are able to see that memory space directly and operate with it.

The structure of OpenACC directives in Fortran is as follows:

~~~
!$acc <directive> [clause, [[,] clause] . . . ]
~~~
{: .source}

This is called and OpenACC **construct**.
A construct is made of one **directive** followed by one or more **clauses**

If directive and its clauses became too long, you can split the line using **backslash**(``\``) at the end of the line to extend the interpretation to the next line.


Beyond using OpenACC directives you could prefer to have fine detail on the parallel execution.
OpenACC offers an API similar to OpenMP which also offers one.
Using them and the code will only be able to work with the OpenACC runtime environment.

In Fortran, the API is activated by including the module ``openacc``

~~~
use openacc
~~~
{: .source}

## First steps with OpenACC

Consider this small code (``example_01.f90``):

~~~
module process_mod
contains
   subroutine process(a, b, n)
      real :: a(:), b(:)
      integer :: n, i

      print *, "subroutine process..."
!$ACC PARALLEL LOOP
      do i = 1, n
         b(i) = exp(sin(a(i)))
      end do
!$ACC END PARALLEL LOOP
   end subroutine
end module process_mod

program simple

   use process_mod

   integer, parameter :: n = 1*10**9
   integer :: i
   real, allocatable:: a(:), b(:)
   allocate (a(n), b(n))

   print *, "initializing array..."
!$ACC PARALLEL LOOP
   do i = 1, n
      a(i) = asin(log(real(i)))
   end do
!ACC END PARALLEL LOOP

   call process(a, b, n)

end program simple
~~~
{: .language-fortran}

This code captures the essence of many scientific code.
It has a subroutine inside a module and there is a loop to initialize an array and another loop to produce a computation.

On these two loops we enclose them with OpenACC directives.
Before we discuss those lets first compile the code and run it under several conditions.
For OpenACC we will use the NVIDIA Compilers (Formerly PGI compilers).
As we will execute the code on a GPU the first step is to request an interactive session on a compute node with GPU.
Execute this command to request 1 node, 8 cores and 1 GPU for 4 hours

~~~
$> qsub -I -q comm_gpu_inter -l nodes=1:ppn=8:gpus=1
~~~
{: .language-bash}

The command above request 1 GPU card, 8 CPU cores on 1 node.
The default wall time for ``comm_gpu_inter`` queue is 4 hours, so that is the amount of time that is given when the job start running.
The reason for selecting 8 hours is that most GPU compute nodes offer 3 GPU cards and those machines have 24 cores, the ratio then is 8 CPU cores for each GPU card.
There are ways of using more than one GPU card but 1 GPU is enough during these examples and exercises.

The job will run on a GPU node.
You can check the presence of the GPU by running the command ``nvidia-smi``.
The output is shown below:

~~~
Wed Aug 03 12:37:57 2021       
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 465.19.01    Driver Version: 465.19.01    CUDA Version: 11.3     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|                               |                      |               MIG M. |
|===============================+======================+======================|
|   0  NVIDIA Quadro P...  Off  | 00000000:AF:00.0 Off |                  Off |
| 26%   19C    P8     8W / 250W |      0MiB / 24449MiB |      0%   E. Process |
|                               |                      |                  N/A |
+-------------------------------+----------------------+----------------------+

+-----------------------------------------------------------------------------+
| Processes:                                                                  |
|  GPU   GI   CI        PID   Type   Process name                  GPU Memory |
|        ID   ID                                                   Usage      |
|=============================================================================|
|  No running processes found                                                 |
+-----------------------------------------------------------------------------+
~~~
{: .output}

Once you get assigned one of the GPU compute nodes. You need to load the module for the NVIDIA HPC SDK and change to the folder with the code examples:

~~~
$> module load lang/nvidia/nvhpc
$> cd Modern-Fortran/files/openacc
~~~
{: .language-bash}

The NVIDIA compilers offer some environment variables that can be used for performance analysis during executions.
Execute this to enable the timing when running with OpenACC on GPUs:

~~~
$> export PGI_ACC_TIME=1
~~~
{: .language-bash}

For our first compilation, we will compile without any special argument for the compiler.
By default, the compiler will ignore entirely any OpenACC directive and compile a pure Fortran code.

~~~
$> nvfortran example_01.f90
$> time ./a.out
~~~
{: .language-bash}
~~~
initializing array...
subroutine process...

real	0m27.616s
user	0m26.072s
sys	  0m1.479s
~~~
{: .output}

We are using time to get the amount of time took by the execution of the code.
Notice that there are 2 loops that take most of the time, one initializing the array ``a`` and a second that computes ``b``.
We have used a few functions like ``exp``, ``sin`` and their inverses.
Those are simple mathematical operations but computing with them takes a few CPU cycles for each value, making them particularly expensive for a large array like ``a``.
Compiling the code like above, the resulting executable will only run on one core.
The timing says that 27 seconds are needed on the machine.

## Example 1: SAXPY

For our first example we will demonstrate the use of OpenACC to parallelize the
function  Y = aX + Y,

~~~
module mod_saxpy

contains

   subroutine saxpy(n, a, x, y)

      implicit none

      real :: x(:), y(:), a
      integer :: n, i

!$ACC PARALLEL LOOP
      do i = 1, n
         y(i) = a*x(i) + y(i)
      end do
!$ACC END PARALLEL LOOP

   end subroutine saxpy

end module mod_saxpy

program main

   use mod_saxpy

   implicit none

   integer, parameter :: n = huge(n)
   real :: x(n), y(n), a = 2.3
   integer :: i

   print *, "Initializing X and Y..."

!$ACC PARALLEL LOOP
   do i = 1, n
      x(i) = sqrt(real(i))
      y(i) = sqrt(1.0/real(i))
   end do
!$ACC END PARALLEL LOOP

   print *, "Computing the SAXPY operation..."

!$ACC PARALLEL LOOP
   do i = 1, n
      y(i) = a*x(i) + y(i)
   end do
!$ACC END PARALLEL LOOP

   call saxpy(n, a, x, y)

end program main
~~~
{: .language-fortran}

There are two sections parallelized in the code, on each one of them there is
some data being implicitly moved to the GPU memory an back to CPU memory.


## Laplace example

This is a good example for demonstrating how a code can be improved step by step adding OpenACC directives.
We start with the serial version of the code.

### Serial Laplace

~~~
program serial
      implicit none

      !Size of plate
      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      double precision, parameter    :: max_temp_error=0.01

      integer                        :: i, j, max_iterations, iteration=1
      double precision               :: dt=100.0
      real                           :: start_time, stop_time

      double precision, dimension(0:rows+1,0:columns+1) :: temperature, temperature_last

      print*, 'Maximum iterations [100-4000]?'
      read*,   max_iterations

      call cpu_time(start_time)      !Fortran timer

      call initialize(temperature_last)

      !do until error is minimal or until maximum steps
      do while ( dt > max_temp_error .and. iteration <= max_iterations)

         do j=1,columns
            do i=1,rows
               temperature(i,j)=0.25*(temperature_last(i+1,j)+temperature_last(i-1,j)+ &
                                      temperature_last(i,j+1)+temperature_last(i,j-1) )
            enddo
         enddo

         dt=0.0

         !copy grid to old grid for next iteration and find max change
         do j=1,columns
            do i=1,rows
               dt = max( abs(temperature(i,j) - temperature_last(i,j)), dt )
               temperature_last(i,j) = temperature(i,j)
            enddo
         enddo

         !periodically print test values
         if( mod(iteration,100).eq.0 ) then
            call track_progress(temperature, iteration)
         endif

         iteration = iteration+1

      enddo

      call cpu_time(stop_time)

      print*, 'Max error at iteration ', iteration-1, ' was ',dt
      print*, 'Total time was ',stop_time-start_time, ' seconds.'

end program serial


! initialize plate and boundery conditions
! temp_last is used to to start first iteration
subroutine initialize( temperature_last )
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,j

      double precision, dimension(0:rows+1,0:columns+1) :: temperature_last

      temperature_last = 0.0

      !these boundary conditions never change throughout run

      !set left side to 0 and right to linear increase
      do i=0,rows+1
         temperature_last(i,0) = 0.0
         temperature_last(i,columns+1) = (100.0/rows) * i
      enddo

      !set top to 0 and bottom to linear increase
      do j=0,columns+1
         temperature_last(0,j) = 0.0
         temperature_last(rows+1,j) = ((100.0)/columns) * j
      enddo

end subroutine initialize


!print diagonal in bottom corner where most action is
subroutine track_progress(temperature, iteration)
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,iteration

      double precision, dimension(0:rows+1,0:columns+1) :: temperature

      print *, '---------- Iteration number: ', iteration, ' ---------------'
      do i=5,0,-1
         write (*,'("("i4,",",i4,"):",f6.2,"  ")',advance='no') &
                   rows-i,columns-i,temperature(rows-i,columns-i)
      enddo
      print *
end subroutine track_progress
~~~
{: .language-fortran}

### Bad OpenACC Laplace

~~~
program serial
      implicit none

      !Size of plate
      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      double precision, parameter    :: max_temp_error=0.01

      integer                        :: i, j, max_iterations, iteration=1
      double precision               :: dt=100.0
      real                           :: start_time, stop_time

      double precision, dimension(0:rows+1,0:columns+1) :: temperature, temperature_last

      print*, 'Maximum iterations [100-4000]?'
      read*,   max_iterations

      call cpu_time(start_time)      !Fortran timer

      call initialize(temperature_last)

      !do until error is minimal or until maximum steps
      do while ( dt > max_temp_error .and. iteration <= max_iterations)

         !$acc kernels
         do j=1,columns
            do i=1,rows
               temperature(i,j)=0.25*(temperature_last(i+1,j)+temperature_last(i-1,j)+ &
                                      temperature_last(i,j+1)+temperature_last(i,j-1) )
            enddo
         enddo
         !$acc end kernels

         dt=0.0

         !copy grid to old grid for next iteration and find max change
         !$acc kernels
         do j=1,columns
            do i=1,rows
               dt = max( abs(temperature(i,j) - temperature_last(i,j)), dt )
               temperature_last(i,j) = temperature(i,j)
            enddo
         enddo
         !$acc end kernels

         !periodically print test values
         if( mod(iteration,100).eq.0 ) then
            call track_progress(temperature, iteration)
         endif

         iteration = iteration+1

      enddo

      call cpu_time(stop_time)

      print*, 'Max error at iteration ', iteration-1, ' was ',dt
      print*, 'Total time was ',stop_time-start_time, ' seconds.'

end program serial


! initialize plate and boundery conditions
! temp_last is used to to start first iteration
subroutine initialize( temperature_last )
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,j

      double precision, dimension(0:rows+1,0:columns+1) :: temperature_last

      temperature_last = 0.0

      !these boundary conditions never change throughout run

      !set left side to 0 and right to linear increase
      do i=0,rows+1
         temperature_last(i,0) = 0.0
         temperature_last(i,columns+1) = (100.0/rows) * i
      enddo

      !set top to 0 and bottom to linear increase
      do j=0,columns+1
         temperature_last(0,j) = 0.0
         temperature_last(rows+1,j) = ((100.0)/columns) * j
      enddo

end subroutine initialize


!print diagonal in bottom corner where most action is
subroutine track_progress(temperature, iteration)
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,iteration

      double precision, dimension(0:rows+1,0:columns+1) :: temperature

      print *, '---------- Iteration number: ', iteration, ' ---------------'
      do i=5,0,-1
         write (*,'("("i4,",",i4,"):",f6.2,"  ")',advance='no') &
                   rows-i,columns-i,temperature(rows-i,columns-i)
      enddo
      print *
end subroutine track_progress
~~~
{: .language-fortran}

### Efficient OpenACC Laplace

~~~
program serial
      implicit none

      !Size of plate
      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      double precision, parameter    :: max_temp_error=0.01

      integer                        :: i, j, max_iterations, iteration=1
      double precision               :: dt=100.0
      real                           :: start_time, stop_time

      double precision, dimension(0:rows+1,0:columns+1) :: temperature, temperature_last

      print*, 'Maximum iterations [100-4000]?'
      read*,   max_iterations

      call cpu_time(start_time)      !Fortran timer

      call initialize(temperature_last)

      !do until error is minimal or until maximum steps
      !$acc data copy(temperature_last), create(temperature)
      do while ( dt > max_temp_error .and. iteration <= max_iterations)

         !$acc kernels
         do j=1,columns
            do i=1,rows
               temperature(i,j)=0.25*(temperature_last(i+1,j)+temperature_last(i-1,j)+ &
                                      temperature_last(i,j+1)+temperature_last(i,j-1) )
            enddo
         enddo
         !$acc end kernels

         dt=0.0

         !copy grid to old grid for next iteration and find max change
         !$acc kernels
         do j=1,columns
            do i=1,rows
               dt = max( abs(temperature(i,j) - temperature_last(i,j)), dt )
               temperature_last(i,j) = temperature(i,j)
            enddo
         enddo
         !$acc end kernels

         !periodically print test values
         if( mod(iteration,100).eq.0 ) then
            call track_progress(temperature, iteration)
         endif

         iteration = iteration+1

      enddo
      !$acc end data

      call cpu_time(stop_time)

      print*, 'Max error at iteration ', iteration-1, ' was ',dt
      print*, 'Total time was ',stop_time-start_time, ' seconds.'

end program serial


! initialize plate and boundery conditions
! temp_last is used to to start first iteration
subroutine initialize( temperature_last )
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,j

      double precision, dimension(0:rows+1,0:columns+1) :: temperature_last

      temperature_last = 0.0

      !these boundary conditions never change throughout run

      !set left side to 0 and right to linear increase
      do i=0,rows+1
         temperature_last(i,0) = 0.0
         temperature_last(i,columns+1) = (100.0/rows) * i
      enddo

      !set top to 0 and bottom to linear increase
      do j=0,columns+1
         temperature_last(0,j) = 0.0
         temperature_last(rows+1,j) = ((100.0)/columns) * j
      enddo

end subroutine initialize


!print diagonal in bottom corner where most action is
subroutine track_progress(temperature, iteration)
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,iteration

      double precision, dimension(0:rows+1,0:columns+1) :: temperature

      print *, '---------- Iteration number: ', iteration, ' ---------------'
      do i=5,0,-1
         write (*,'("("i4,",",i4,"):",f6.2,"  ")',advance='no') &
                   rows-i,columns-i,temperature(rows-i,columns-i)
      enddo
      print *
end subroutine track_progress
~~~
{: .language-fortran}

### Updating Temperature

~~~
program serial
      implicit none

      !Size of plate
      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      double precision, parameter    :: max_temp_error=0.01

      integer                        :: i, j, max_iterations, iteration=1
      double precision               :: dt=100.0
      real                           :: start_time, stop_time

      double precision, dimension(0:rows+1,0:columns+1) :: temperature, temperature_last

      print*, 'Maximum iterations [100-4000]?'
      read*,   max_iterations

      call cpu_time(start_time)      !Fortran timer

      call initialize(temperature_last)

      !do until error is minimal or until maximum steps
      !$acc data copy(temperature_last), create(temperature)
      do while ( dt > max_temp_error .and. iteration <= max_iterations)

         !$acc kernels
         do j=1,columns
            do i=1,rows
               temperature(i,j)=0.25*(temperature_last(i+1,j)+temperature_last(i-1,j)+ &
                                      temperature_last(i,j+1)+temperature_last(i,j-1) )
            enddo
         enddo
         !$acc end kernels

         dt=0.0

         !copy grid to old grid for next iteration and find max change
         !$acc kernels
         do j=1,columns
            do i=1,rows
               dt = max( abs(temperature(i,j) - temperature_last(i,j)), dt )
               temperature_last(i,j) = temperature(i,j)
            enddo
         enddo
         !$acc end kernels

         !periodically print test values
         if( mod(iteration,100).eq.0 ) then
            !$acc update host(temperature)
            call track_progress(temperature, iteration)
         endif

         iteration = iteration+1

      enddo
      !$acc end data

      call cpu_time(stop_time)

      print*, 'Max error at iteration ', iteration-1, ' was ',dt
      print*, 'Total time was ',stop_time-start_time, ' seconds.'

end program serial


! initialize plate and boundery conditions
! temp_last is used to to start first iteration
subroutine initialize( temperature_last )
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,j

      double precision, dimension(0:rows+1,0:columns+1) :: temperature_last

      temperature_last = 0.0

      !these boundary conditions never change throughout run

      !set left side to 0 and right to linear increase
      do i=0,rows+1
         temperature_last(i,0) = 0.0
         temperature_last(i,columns+1) = (100.0/rows) * i
      enddo

      !set top to 0 and bottom to linear increase
      do j=0,columns+1
         temperature_last(0,j) = 0.0
         temperature_last(rows+1,j) = ((100.0)/columns) * j
      enddo

end subroutine initialize


!print diagonal in bottom corner where most action is
subroutine track_progress(temperature, iteration)
      implicit none

      integer, parameter             :: columns=1000
      integer, parameter             :: rows=1000
      integer                        :: i,iteration

      double precision, dimension(0:rows+1,0:columns+1) :: temperature

      print *, '---------- Iteration number: ', iteration, ' ---------------'
      do i=5,0,-1
         write (*,'("("i4,",",i4,"):",f6.2,"  ")',advance='no') &
                   rows-i,columns-i,temperature(rows-i,columns-i)
      enddo
      print *
end subroutine track_progress
~~~
{: .language-fortran}


{% include links.md %}
