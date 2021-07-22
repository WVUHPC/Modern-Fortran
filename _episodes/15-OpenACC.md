---
title: "OpenACC"
teaching: 30
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

## OpenACC in Fortran

OpenACC is another directive-based approach for parallel programming with a more general scope than the original OpenMP.
Before version 4.0, OpenMP was designed to provide parallelism to multicore systems, an alternative to more tedious alternatives like programming with pthreads.
With the advent of accelerators, like GPUs, a more general paradigm was needed that preserve the nice features that OpenMP bring to parallel computing.

An accelerator is, in general, a device that is able to perform certain classes of computations with more performance than the CPU.
Accelerators such as the GPUs come with a high bandwidth memory that is separated from the main CPU RAM.
Any processing that happens in an accelerator must first transfer the data from the CPU memory into the device memory and at the end of the calculation, move the data back.

## First steps with OpenACC

Consider this small code (``example_1_acc.f90``):

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
$> qsub -X -I -q comm_gpu_inter -l nodes=1:ppn=8:gpus=1
~~~
{: .language-bash}

The command above request 1 GPU card, 8 CPU cores on 1 node.
The default walltime for ``comm_gpu_inter`` queue is 4 hours, so that is the amount of time that is given when the job start running.
The reason for selecting 8 hours is that most GPU compute nodes offer 3 GPU cards and those machines have 24 cores, the ratio then is 8 CPU cores for each GPU card.
There are ways of using more than one GPU card but 1 GPU is enough during these examples and exercises.
The extra argument ``-X`` will be necessary if you want to use the NVIDIA Profiler which is a GUI application. In that case you need to log into the cluster with X11 Forwarding using the argument ``-X`` on each ssh connection.

Once you get assigned one of the GPU compute nodes. You need to load the module for the NVIDIA HPC SDK and change to the folder with the code examples:

~~~
$> module load lang/nvidia/nvhpc
$> cd Modern_Fortran_Data/OpenACC
~~~
{: .language-bash}

The NVIDIA compilers offer some environment variables that can be used for performance analysis during executions.
Execute this to enable the timing when running with OpenACC on GPUs:

~~~
$> export PGI_ACC_TIME=1
~~~
{: .language-bash}

For our first compilation we will compile without any special argument for the compiler.
By default the compiler will ignore entirely any OpenACC directive and compile a pure Fortran code.

~~~
$> nvfortran example_1_acc.f90
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
We have use a few functions like ``exp``, ``sin`` and their inverses.
Those are simple mathematical operations but computing with them takes a few CPU cycles for each value, making them particularly expensive for a large array like ``a``.
Compiling the code like above, the resulting executable will only run on one core.
The timing says that 27 seconds are needed on the machine.


{% include links.md %}