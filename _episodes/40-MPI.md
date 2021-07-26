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




{% include links.md %}
