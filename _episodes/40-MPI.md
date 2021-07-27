---
title: "Distributed Computing: MPI"
start: 840
teaching: 30
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

# Message Passing Interface

Parallelism in important in Scientific Computing today because almost all computers are multicore these days.
OpenMP and OpenACC are solutions for Parallelism that require minimal changes to the source code.
OpenMP was originally created to take advantage of multiple cores being able to see the same memory.
openACC extend the idea to machines with accelerators where those accelerators can have its own memory and data must be transferred in and out the device in order to do computations.

The next level in complexity is when the problem is so big that a single machine is not enough and we need to parallelize using multiple machines.
This strategy is called ditributed parallelism.
MPI is the standard *"de facto"* for distruibuted computing in High Performance Computing.
MPI has been around for 30 years with the first efforts to create an standard dating from 1991.

Many of the codes that run on the most powerful supercomputers today, use MPI.
Many libraries have been created that suppport MPI.
Several implementations exists, several open-source and widely available.
MPI is used to Benchamark the most powerful HPC clusters using Linpack, a benchmark package that computes dense linear algebra operations for benchmarks.

In contrast to OpenMP and OpenACC, the downside of MPI is that it forces the programmer to completely rethink the algorithms.
Parallelization in MPI is part of the design of a code instead of an added feature to a serial code.

MPI can be integrated with other paradigms of parallel programming in what is called hybrid parallelism.
MPI is in charge of distributed computing, while OpenMP is in charge or multithreading and OpenACC for using a GPU or any other accelerator.

This lesson will present a brief introduction to MPI, the main concepts which are point to point communication, collectives, barriers and synchronization.
MPI is much more than that but these basic elements are the core of many of the advance concepts on the most recent standard called *MPI-3*.

## Essential concepts in MPI

MPI stands for Message Passing Interface.
The first step is to clarify what that means.
An interface is just a declaration about how something should work and what is the appearance that it should have.
MPI is not a single product.
There are several implementations MPI, some of the most prominents are OpenMPI, MPICH, MVAPICH.
Commercial vendors like Intel have its own version.

### MPI is a paradigm for distributed computing

Message Passing is called a *"paradigm"* for distributed computing.
It is just the answer to the question: How I move data between computers that need to perform calculations in an orchestrated way?
If the question were just *moving data*, there are many ways of doing so, from a distributed parallel system to FTP or the web.
If the point is having machines *work together*, something like work sharing, even a queue system could answer that.
Here we are talking about processors working together including cases were they need to share information to accomplish steps in the tasks that have been assign to them.
Processor could be on the same machine or could be on a different machine.
The data could be a big array of data every few hours or could be a constant flow of data between processors.
Message Passing is one solution to embrace all those possibilities in a solution that is abstract enough so the fine details about how the communication actually happens are left for each implementation to decide.

### MPI is a standard for a library

MPI is a library, more precisely, an standard for an API.
The MPI standard declares the names and arguments of the functions, routines and constants and the arguments that must be used to for any implementation to offer a consistent library such that any code that is written for MPI can use the library from different implementations during compilation and execution.

### MPI is not a Programming language

MPI is not a programming language or even an extension of any programming language.
CUDA in contrast is a superset of C and OpenMP and OpenACC are directives that are not part of a particular language, but implemented in compilers which are compliant with those standards.
When you program in MPI you are using a library. In the case of Fortran a program that uses MPI needs to load the module called ``mpi``:

~~~
use mpi
~~~
{: .language-fortran}

In this lesson we are just considering the way you program with MPI in Fortran.
There are official standards for C and C++ as well.
In those languages the names of the functions are basically the same and the arguments are mostly the same with some small particularities for C and C++ due to the way those languages manage functions.
In the case of Fortran most of the operations are subroutines.


### Communication is an abstraction

In MPI there is nothing that declares what are the physical means for moving the data, it could be using an USB drive (Nobody uses that), could be a normal Ethernet connection from a local network or could be a very high bandwidth, low latency network completely dedicated for MPI.
Over time, specialized networks have been created focusing on optimize collective messages and point to point communications, we will see those concepts later on.

### MPI not necessary needs multiple nodes

MPI is general enough for declaring that multiple processes can run to solve a problem.
Those processes can run on the same machine or multiple machines.
If you have a computer with 4 or 8 cores, you can run all the examples and exercises on your own machine.
Even if you have just one core, you can still execute MPI programs.
Running the code will create multiple cores that will share the same CPU, but that is just an efficiency issue, MPI should still work.

## MPI Machinery: Library, compiler wrappers and Runtime utility

A code that uses MPI needs more than just a library.
When the code runs multiple processes need to be created, potentially on different machines, they need to communicate and there is a need for an infraestructure that decides how data is transferred.

In practice a program that uses MPI needs several pieces from a MPI implementation.

1. **Compiler wrapper**

A MPI implementation will provide wrappers for the compilers.
A wrapper is an executable that is put in the middle between the sources and an actual compiler such as ``gfortran``, ``nvfortran`` or ``ifort``.

The wrapper for Fortran is usually called ``mpif90`` or in the case of Intel MPI you have ``mpiifort``.
To compile a code with MPI use:

~~~
mpif90 mpi_01.f90
~~~
{: .language-bash}

The wrapper ``mpif90`` internally will call the compiler used to build the MPI version you are using.
We will compile a simple code very soon and see how that works in practice.

2. **Runtime MPI execution**

Running a MPI code is different from running any other code that you compile.
Normally when you are testing small codes, you compile them with your compiler (for example ``gfortran``) and you produce an executable like ``a.out`` and execute it:

~~~
$> gfortran example_01.f90
$> ./a.out
~~~
{: .language-bash}

In MPI we need an intermediary, an executable that will be in charge of launching processes in all the machines that will execute the code. The name of the executable is ``mpirun`` or ``mpiexec``  

You can use either one.
``mpiexec`` is defined in the MPI standard.
``mpirun`` is a command implemented by many MPI implementations.
As this one is not standardized, there are often subtle differences between implementations.

## First example of an MPI process

The example below is a fairly simple code that is bit more elaborated than a dumb "Hello World" program.

The code will on a number of processes.
We are using 8 for the output.
Lets see first the code and compile it and after we discuss the calls that are introduced here (``mpif90 example_01.f90``).

~~~
module time_mgt

contains

   subroutine timestamp()

      use, intrinsic :: iso_fortran_env

      implicit none

      character(len=8) :: ampm
      integer(kind=int32) :: d
      integer(kind=int32) :: h
      integer(kind=int32) :: m
      integer(kind=int32) :: mm
      character(len=9), parameter, dimension(12) :: month = (/ &
                                                    'January  ', 'February ', &
                                                    'March    ', 'April    ', &
                                                    'May      ', 'June     ', &
                                                    'July     ', 'August   ', &
                                                    'September', 'October  ', &
                                                    'November ', 'December '/)
      integer(kind=4) :: n
      integer(kind=4) :: s
      integer(kind=4) :: values(8)
      integer(kind=4) :: y

      call date_and_time(values=values)

      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)

      if (h < 12) then
         ampm = 'AM'
      else if (h == 12) then
         if (n == 0 .and. s == 0) then
            ampm = 'Noon'
         else
            ampm = 'PM'
         end if
      else
         h = h - 12
         if (h < 12) then
            ampm = 'PM'
         else if (h == 12) then
            if (n == 0 .and. s == 0) then
               ampm = 'Midnight'
            else
               ampm = 'AM'
            end if
         end if
      end if

      write (*, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)') &
         d, trim(month(m)), y, h, ':', n, ':', s, '.', mm, trim(ampm)

      return
   end subroutine

end module

program main

   use, intrinsic :: iso_fortran_env
   use time_mgt
   use mpi_f08

   implicit none

   integer, parameter :: n=1000000
   integer(kind=int32) :: i
   integer(kind=int32) :: ierror ! To control errors in MPI calls
   integer(kind=int32) :: rank   ! Unique number received by each process
   integer(kind=int32) :: num_proc ! Total number of processes
   real(kind=real64)   :: wtime
   real(kind=real64), allocatable, dimension(:) :: array

   ! Initialize MPI. This must be the first MPI call
   call MPI_Init(ierror)

   ! Get the number of processes
   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)

   ! Get the individual process rank
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   if (rank == 0) then
      wtime = MPI_Wtime()

      call timestamp()
      ! Only rank = 0 print this
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a,i3)') 'RANK:', rank, &
         ' The number of MPI processes is ', num_proc

   else

      ! Every MPI process will print this message.
      write (*, '(a,i2,2x,a,i8)') 'RANK:', rank, &
      ' Allocating array of size:', rank*n

      ! Each rank will allocate an array of a different size
      allocate (array(rank*n))
      do i = 1, size(array)
         array(i) = log(real(rank))+sqrt(real(i))
      end do

      ! Reporting sum of array
      write (*, '(a,i2,2x,a,e12.3)') 'RANK:', rank, ' Sum of array:', sum(array)

   end if

   if (rank == 0) then
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a)') 'RANK:', rank, &
      ' Normal end of execution for master'

      wtime = MPI_Wtime() - wtime
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a,g14.6,a)') &
         'RANK:', rank, ' Elapsed wall clock time = ', wtime, ' seconds.'
      write (*, '(a)') ''

   end if

   ! No more MPI calls after Finalize
   call MPI_Finalize(ierror)

   ! Ranks are intrinsic to each process and this conditional is legal
   if (rank == 0) then
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Normal end of execution for all'

      call timestamp()
   end if

   stop

end program
~~~
{: .language-fortran}

### Compiling the example

To compile this code we need a fortran compiler and a MPI implementation.
We can choose among several combinations of compilers and MPI implementations:

Using GCC 11 and OpenMPI 4.1.1 use:

~~~
$> module load lang/gcc/11.1.0 parallel/openmpi/4.1.1_gcc111
$> mpif90 example_01.f90
$> mpirun -mca btl ofi -np 8 ./a.out
~~~
{: .language-bash}

Using NVIDIA HPC which includes OpenMPI 3.15 the line below.
The extra arguments are optional and they prevent some messages from being displayed.
The messages showing ``ieee_inexact`` occurs even if if arrays are null.

~~~
$> module load lang/nvidia/nvhpc/21.3
$> mpif90 -Kieee -Ktrap=none example_01.f90
$> mpirun -np 8 --mca mpi_cuda_support 0 -mca mtl_base_verbose 1 -mca orte_base_help_aggregate 0 -mca btl openib ./a.out
~~~
{: .language-bash}

Another alternative is using Intel MPI compilers and Intel MPI.

~~~
$> module load compiler/2021.2.0 mpi/2021.2.0
$>  mpiifort example_01.f90
$>  mpirun -np 8 ./a.out
~~~
{: .language-bash}

The output varies but in general you will see something like:

~~~
26 July 2021   5:51:47.252 PM

RANK: 0   Master process reporting:
RANK: 0   The number of MPI processes is   8

RANK: 0   Master process reporting:
RANK: 0   Normal end of execution for master
RANK: 1   Allocating array of size: 1000000
RANK: 1   Sum of array:   0.667E+09
RANK: 2   Allocating array of size: 2000000
RANK: 3   Allocating array of size: 3000000
RANK: 4   Allocating array of size: 4000000
RANK: 5   Allocating array of size: 5000000
RANK: 6   Allocating array of size: 6000000
RANK: 7   Allocating array of size: 7000000
RANK: 2   Sum of array:   0.189E+10
RANK: 4   Sum of array:   0.534E+10
RANK: 3   Sum of array:   0.347E+10
RANK: 5   Sum of array:   0.746E+10
RANK: 6   Sum of array:   0.981E+10
RANK: 7   Sum of array:   0.124E+11

RANK: 0   Master process reporting:
RANK: 0   Normal end of execution for all

RANK: 0   Elapsed wall clock time =   0.145266     seconds.

26 July 2021   5:51:47.397 PM
~~~
{: .output}

Notice that the output does not necessary follows a clear order, each process runs independent and how the output is collected and displayed depends on the MPI runtime executable.

### The first MPI lines

Any Fortran code that uses MPI needs to load the mpi module

~~~
use mpi_f08
~~~

We are using the modern module ``mpi_f08``

This code will expose all the constants, variables and routines that are declared in the MPI 3.1 standard.

MPI defines three methods of Fortran support:

  1. ``use mpi_f08``: It requires compile-time argument checking with unique MPI handle types and provides techniques to fully solve the optimization problems with nonblocking calls. This is the only Fortran support method that is consistent with the Fortran standard (Fortran 2008 + TS 29113 and later). This method is highly recommended for all MPI applications.

  2. ``use mpi``: It requires compile-time argument checking. Handles are defined as INTEGER. This Fortran support method is inconsistent with the Fortran standard, and its use is therefore not recommended.

  3. ``INCLUDE 'mpif.h'``: The use of the include file mpif.h is strongly discouraged starting with MPI-3.0, because this method neither guarantees compile-time argument checking nor provides sufficient techniques to solve the optimization problems with nonblocking calls, and is therefore inconsistent with the Fortran standard.

All modern compilers support most of Fortran 2008 and MPI implementations support most MPI 3.0. In older examples you can find these other methods of accessing MPI from Fortran.

Regard
The first 3 calls that we are using are:

~~~
! Initialize MPI. This must be the first MPI call
call MPI_Init(ierror)

! Get the number of processes
call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)

! Get the individual process rank
call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
~~~
{: .language-fortran}

``MPI_Init`` must be the first MPI call ever in the code.
It prepares the environment including the variables that we will collect in the next two calls.

``MPI_Comm_size`` and ``MPI_Comm_rank`` return the number of processes in the MPI execution and the particular rank assigned to each process.

All processes created after ``mpirun`` are almost identical, start executing the same code.
The only difference is the rank, an integer that starts in zero as it is commonly used in C and ends in N-1 where N is the number of processes requested in the command line.
Notice that an MPI code does not know in compile time, how many processes it will run, programmers need to code knowing that the code could run with one rank or thousands even millions of processes.

Another MPI call is ``MPI_Wtime()`` that returns an elapsed time on the calling processor, the return in fortran is a float in ``DOUBLE PRECISION``. We use this to compute the wall clock for rank zero.
Notice that rank zero is not allocating any array and will finish very quickly.

Other ranks will allocate matrices of increasing size based on their rank.
The arrays are populated and the sum computed.
Using 8 processes, ranks 1 to 7 will be allocating independent arrays that are populated and the sum computed.

This very simple example is no doing any communication between processes and each rank is following the the execution until it encounters the last call.
`` MPI_Finalize`` is the last call of any MPI program.
No more MPI calls can appear after this.




{% include links.md %}
