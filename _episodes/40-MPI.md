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
Lets see first the code and compile it and after we discuss the calls that are introduced here (``example_01.f90``).

~~~
program main

   use, intrinsic :: iso_fortran_env
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
      deallocate(array)

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

These two subroutines use as first parameter something called a **communicator**.
In this particular case and for small codes the generic ``MPI_COMM_WORLD`` will include **all** processes created with ``mpirun``.
More complex codes could use multiple communicators and we can decide which ranks go to each of them.

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

All calls start with ``MPI_`` so it is easy to identify them.
In MPI-3 the final argument that we are calling ``ierror`` is optional.
Previous versions ask for this extra argument in Fortran calls and it is missing in C and C++ as the error code comes in the function value.
Normally the ``ierror`` should be compared with ``MPI_SUCCESS`` for proper error management.
we are skipping this at least for now.

Now we have a code that when executed will run with multiple processes each of them doing different things but no communication is happening between them.
Our next example will explore this.

## First example with communication

In our first example we consider a code that allocate arrays of various sizes and produce a calculation.
Each process was in fact independent of each other and not communication was involved.
For our next example we will consider a simple communication between processes.

Consider the following code (``example_012.f90``)

~~~
module mod_ring

contains

   subroutine ring(num_proc, rank)

      use, intrinsic :: iso_fortran_env
      use mpi_f08

      implicit none

      integer(kind=int32), intent(in) :: num_proc
      integer(kind=int32), intent(in) :: rank

      integer(kind=int32), parameter :: n_test_num = 7
      integer(kind=int32), parameter :: test_num = 20
      integer(kind=int32), parameter :: decimal_precision = 5
      integer(kind=int32), parameter :: exponent_range = 300
      integer(kind=int32), parameter :: dp = &
      selected_real_kind(decimal_precision, exponent_range)

      integer(kind=int32) :: source, destin
      integer(kind=int32) :: ierror
      integer(kind=int32) :: i, j, n
      integer(kind=int32), dimension(n_test_num) :: n_test
      integer(kind=int32) :: test
      real(kind=real64)   :: tave, tmax, tmin
      real(kind=real64)   :: wtime
      real(kind=dp), allocatable, dimension(:) :: x

      !type(MPI_Status)   :: mpistatus(MPI_STATUS_SIZE)
      type(MPI_Status)   :: mpistatus
      type(MPI_Datatype) :: realtype


      call MPI_Type_Create_F90_real(decimal_precision, exponent_range, &
                                    realtype, ierror)
      !real(kind=real64), allocatable :: x(:)

      ! Old way of creating arrays
      !n_test = (/ 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 /)
      n_test = [ (10**i, i=1, n_test_num) ]

      if (rank == 0) then
         write (*, '(a)') ' '
         write (*, '(a,i3)') ' RANK 0: Number of cases:    ', test_num
         write (*, '(a,i3)') ' RANK 0: Arrays of KIND:     ', real64
         write (*, '(a,i3)') ' RANK 0: Number of processes:', num_proc
         write (*, '(a)') ' '
         write (*, '(a11,a14,a14,a14)') 'N', 'T min', 'T max', 'T ave'
         write (*, '(a)') ' '
      end if

      ! Testing with arrays of increasing size
      do i = 1, n_test_num

         n = n_test(i)

         allocate (x(1:n))

         ! RANK 0 sends very first message,
         ! then waits to receive the "echo" that has gone around the world.
         if (rank == 0) then

            destin = 1
            source = num_proc - 1

            tave = 0.0D+00
            tmin = huge(1.0D+00)
            tmax = 0.0D+00

            ! Loop to collect statistics of time to cycle
            do test = 1, test_num
               ! Set the entries of X in a way that identifies the test
               ! There is an implicit loop to populate the array
               x = [(real(test + j - 1, kind=real64), j=1, n)]

               wtime = MPI_Wtime()
               call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
               call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
               wtime = MPI_Wtime() - wtime

               ! Record the time it took.
               tave = tave + wtime
               tmin = min(tmin, wtime)
               tmax = max(tmax, wtime)

            end do

            tave = tave/real(test_num, kind=real64)

            write (*, '(2x,i9,2x,f12.7,2x,f12.7,2x,f12.7)') n, tmin, tmax, tave

         ! All ranks > 0: Receive first from rank-1,
         ! then send to rank+1 or 0 if it is the last rank
         else

            source = rank - 1
            destin = mod(rank + 1, num_proc)

            ! Loop to collect statistics of time to cycle
            do test = 1, test_num
               ! ---> Recv ++++ Send --->
               call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
               call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
            end do

         end if

         deallocate (x)

      end do

      return
   end

end module

program main

   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use mod_ring

   implicit none

   integer(kind=int32) :: ierror
   integer(kind=int32) :: rank
   integer(kind=int32) :: num_proc

   ! First MPI call
   call MPI_Init(ierror)

   ! Get the number of processes (num_proc)
   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)

   ! Get the individual process (rank)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

   if (rank == 0) then
      write (*, '(a)') ' '
      write (*, '(a)') ' RANK 0: MPI prepared for ring of messages'
   end if

   ! Routine that will transfer messages in a ring
   call ring(num_proc, rank)

   ! Last MPI call
   call MPI_Finalize(ierror)

   ! Terminate.
   if (rank == 0) then
      write (*, '(a)') ' '
      write (*, '(a)') ' RANK 0: Execution complete'
      write (*, '(a)') ' '
   end if

   stop
end
~~~
{: .language-fortran}

The code above has one module that is needed by the main program.

On the main program we see the elements that we already introduced.
We initialize with ``MPI_Init``.
We got the total size of the communicator (total number of processes) and individual rank with ``MPI_Comm_size`` and ``MPI_Comm_rank``
We are calling the subroutine ``ring`` that is defined in the module ``mod_ring`` and finally we end close the MPI calls with ``MPI_Finalize``.
Everything in the main program should be known by now.

The purpose of the subroutine ring is to test the amount of time needed to sent a message in a ring starting from rank 0 going across every rank and returning to rank 0.
For example, if you run the code with 3 processes, rank 0 will sent an array to rank 1, rank 1 will sent the same array to rank 2 and rank 2 will return the message to rank 0.

We are testing arrays as small as 10 ``real64`` numbers up to arrays millions of numbers.
This communication in ring is happening ``test_num = 20`` times so we can collect some statistics of how much time is needed in average to complete the ring.

Before we discuss more in detail how process send and receive messages lets compile and run the code:

Using GCC 11.1 and OpenMPI 4.1.1:

~~~
$> module purge
$> module load lang/gcc/11.1.0 parallel/openmpi/4.1.1_gcc111
$> mpif90 example_02.f90
$> mpirun -np 8 -mca btl ofi ./a.out
~~~
{: .language-bash}

Using NVIDIA HPC 21.3:

~~~
$> module purge
$> module load lang/nvidia/nvhpc/21.3
$> mpif90 example_02.f90
$> mpirun -np 8 --mca mpi_cuda_support 0 -mca btl self,vader  -mca mtl_base_verbose 1 ./a.out
~~~
{: .language-bash}

Using the Intel compilers 2021 and Intel MPI 2021 we need to force arrays to be allocated on the heap always with ``-heap-arrays 1``

~~~
$> module purge
$> module load compiler/2021.2.0 mpi/2021.2.0
$> mpiifort -check all -heap-arrays 1 example_02.f90
$> mpirun -np 8 ./a.out
~~~
{: .language-bash}

This code will not compile with older compilers such as GCC 4.8 or old versions of OpenMPI such as 2.x

The output of the code will look like this:

~~~
 RANK 0: MPI prepared for ring of messages

 RANK 0: Number of cases:     20
 RANK 0: Arrays of KIND:       8
 RANK 0: Number of processes:  8

          N         T min         T max         T ave

         10     0.0000205     0.0008759     0.0000642
        100     0.0000229     0.0000302     0.0000237
       1000     0.0000470     0.0000585     0.0000495
      10000     0.0001982     0.0004318     0.0002199
     100000     0.0016798     0.0032272     0.0017779
    1000000     0.0165497     0.0238534     0.0169813
   10000000     0.1658572     0.2326462     0.1697777

 RANK 0: Execution complete
~~~
{: .output}

There are two sections in the code that are in charge of sending and receiving data:
Rank 0 is the first to send and the last to receive.
The code for it will be like this:

~~~
call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
~~~
{: .language-fortran}

Other ranks will first receive and after send.
The code for them will be:

~~~
call MPI_Recv(x, n, realtype, source, 0, MPI_COMM_WORLD, mpistatus, ierror)
call MPI_Send(x, n, realtype, destin, 0, MPI_COMM_WORLD, ierror)
~~~
{: .language-fortran}

The arguments of ``MPI_Recv`` and ``MPI_Send`` provide information about *what*, *where*, and information about error conditions (optional in MPI-3.0).

The Fortran 2008 syntax for both calls is:

~~~
USE mpi_f08

MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: buf
    INTEGER, INTENT(IN) :: count, dest, tag
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
    TYPE(*), DIMENSION(..) :: buf
    INTEGER, INTENT(IN) :: count, source, tag
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
~~~
{: . language-fortran}

The first 3 arguments deal with the *what* are we sending or receiving.
The first one is a buffer *buf*, it could be a single value, an array or even a derived type.
Second is the number of elements (*count*) that will be transferred.
Third is the datatype for the data.
Before Fortran 2008, the datatype was an integer, starting with MPI 3.0 the datatype has its own type ``MPI_Datatype``.
We will discuss MPI data types later on.

The next 3 arguments declare the *where* the data comes or goes.
Argument number 4 is the source or destination, the rank where the data is going in the case of ``MPI_Send`` or the rank where it is coming in the case of ``MPI_Recv``. Those are integers.
Next is an identifier or tag for the message.
For a simple code like this each rank is sending and receiving a single kind of message, we are just using zero as tag.
Argument number 6 is the communicator.
We are just working with one communicator `MPI_COMM_WORLD`.
Notice that communicators has its own type too ``MPI_Comm``.

Up to this point the arguments for ``MPI_Recv`` has an equivalent  for ``MPI_Send``.
``MPI_Recv`` uses an extra argument ``status`` that in previous versions use to be an array of integers in Fortran 2008 and MPI 3.0 receives its own datatype ``MPI_Status`` and it is not longer an array.
The final argument is optional in MPI 3.0 and contains an integer indicating of any error condition with the call.
A good practice is to compare its value with ``MPI_SUCCESS``, on the code in the source folder we are comparing ``ierror`` with ``MPI_SUCCESS`` but the lines are commented.
Try to uncomment those lines and see the effect.

### Blocking Recvs and Standard Sends

All of the receives that we are using here are **blocking**.
Blocking receives means that rank will wait until a message matching their requirements for source and tag has been received into
the queue.

Sends try not to block, at least for very small messages.
However, there is no guarantee of non-blocking behavior.
Large data structures will require to be copied in the queue by chunks and a blocking behavior could result.

Consider this simple exercise (This is one of our exercises indeed).
Swaping some array between 2 ranks.

Imagine that your code looks like this:

~~~
if (rank == 0) then
   call MPI_Send(A, ... )
   call MPI_Recv(A, ... )
else
   call MPI_Send(A, ... )
   call MPI_Recv(A, ... )
end if
~~~
{: .language-fortran}

What is wrong with this?
Well if by change both ``MPI_Send`` are blocking the communication, the ``MPI_Recv`` calls will never be reached.
Both processes will stay forever trying to send a message that the other is not ready to receive.
We have a *deadlock* in the code.

The best way is to first think that both ``MPI_Recv`` and ``MPI_Send`` are blocking calls.

Non-blocking communication will open opportunities for better performance, but it is more complex to manage too.
We can plan an algorithm using blocking communications and relax those restrictions with proper care.

Just as small window on the possibilities of Sending data, this table summarizes 4 different kinds of Send subroutines that can be called

| Mode | Subroutine | Description |
|------|------------|-------------|
| **Standard** | ``MPI_Send`` | Send will usually not block even if a receive for that message has not occurred. Exception is if there are resource limitations (buffer space). |
|------|------------|-------------|
| **Buffered Mode** | ``MPI_Bsend`` | Similar to above, but will never block (just return error). |
|------|------------|-------------|
| **Synchronous Mode** | ``MPI_Ssend`` | Will only return when matching receive has started. No extra buffer copy needed, but also can’t do any additional computation. Good for checking code safety |
|------|------------|-------------|
| **Ready Mode** | ``MPI_Rsend`` | Will only work if matching receive is already waiting. Must be well synchronized or behavior is undefined.



{% include links.md %}
