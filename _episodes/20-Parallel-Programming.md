---
title: "Parallel Programming in Fortran"
start: 840
teaching: 60
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

# Parallel Computing in Fortran

OpenMP and OpenACC are directive-based standards for parallel programming. In the case of OpenMP the original intention was to provide efficient threaded parallelism on shared memory computers. For OpenACC the objective is to use a similar idea but for external accelerators such as GPUs.

With OpenMP and OpenACC the programming is carried out by adding some extra lines to the serial code that are comments from the point of view of the language but interpreted by a compliant language that is capable of interpreting those lines and parallelize the code according to them. The lines of code added to the original sources are called directives. An ignorant compiler will just ignore those lines and the code will execute as before the introduction of the directives.

With OpenMP and OpenACC you can go beyond the directives and use an API, using methods and routines that explicitly demand the compiler to be compliant with those parallel paradigms.

<a href="{{ page.root }}/fig/GPU_compute_node.png">
  <img src="{{ page.root }}/fig/GPU_compute_node.png" alt="GPU_compute_node" />
</a>


## First example

Consider the following code that computes pi using a series converging to its numerical value.
The filed will be called ``pi.f90`` and the content follows:

~~~
program converge_pi

use iso_fortran_env
implicit none

integer, parameter :: r15 = selected_real_kind(15)
integer, parameter :: n=huge(1)
real(kind=r15), parameter :: pi_ref=3.1415926535897932384626433_real64

integer :: i
real(kind=r15) :: pi=0.0,t

do i=0, n-1
   t = (real(i)+0.05)/n
   pi = pi + 4.0/(1.0+t*t)
end do

print *,"Number of terms in the series",n
print *,"Computed value",pi/n
print *,"abs difference with reference",abs(pi_ref-pi/n)

end program converge_pi
~~~
{: .language-fortran}

We will compile this code using 3 different compilers, GCC 11.1,

Load the following modules to access all those 3 compilers

~~~
$> module load lang/gcc/11.1.0 lang/nvidia/nvhpc compiler/2021.2.0
~~~
{: .language-bash}

Loading these modules we will get access to ``gfortran``, ``ifort`` and ``nvfortran``, the names of the Fortran compilers from GNU, Intel and NVIDIA respectively.

The code above can be compiled with each compiler executing:

~~~
$> gfortran pi.f90 -o pi
~~~
{: .language-bash}

~~~
$> ifort pi.f90 -o pi
~~~
{: .language-bash}

~~~
$> nvfortran pi.f90 -o pi
~~~
{: .language-bash}

This code can be made parallel by adding a single line before the do loop. For OpenMP the line will be:

~~~
...
!$OMP PARALLEL DO
do i=0, n-1
  t = (real(i)+0.05)/n
  pi = pi + 4.0/(1.0+t*t)
end do
!$OMP END PARALLEL DO
...
~~~
{: .language-fortran}

For OpenACC the line to be added is:

~~~
...
!$ACC KERNELS
do i=0, n-1
  t = (real(i)+0.05)/n
  pi = pi + 4.0/(1.0+t*t)
end do
!$ACC END KERNELS
...
~~~
{: .language-fortran}

Both OpenMP and OpenACC operate by adding special directives to the original sources. I want to stress the similarities before we analyze the differences.

Edit the original file ``pi.f90`` to create a new file ``pi_omp.f90`` with the change above for OpenMP. Do the same to create the file ``pi_acc.f90`` for the file using OpenACC.

Compiling the OpenMP versions will be on each compiler:

~~~
$> gfortran -fopenmp pi_omp.f90 -o pi_omp
~~~
{: .language-bash}

~~~
$> ifort -qopenmp pi_omp.f90 -o pi_omp
~~~
{: .language-bash}

~~~
$> nvfortran -mp -Minfo=all pi_omp.f90 -o pi_omp
~~~
{: .language-bash}

For the OpenACC version of the code the compilation line is:

~~~
$> gfortran -fopenacc pi_acc.f90 -o pi_acc
~~~
{: .language-bash}

Intel compilers do not support OpenACC

~~~
$> nvfortran -acc -Minfo=all pi_acc.f90 -o pi_acc
~~~
{: .language-bash}

Using directives instead of extending the language offers several advantages:

  * The original source code continues to be valid for those compilers that do know about OpenMP or OpenACC, there is no need to fork the code. If you stay at the level of directives, the code will continue to work without the parallelization.

  * The directives provides a very high level abstraction for parallelism. Other solutions involve deep knowledge about the architecture where the code will run reducing the portability to other systems. Architectures evolve over time and hardware specifics will become a burden for maintainers of the code when it need to be adapted to newer technologies.

  * The parallelization effort can be done step by step. There is no need to start from scratch planning important rewrites of large portions of the code with will come with more bugs and development costs.

An alternative to OpenMP could be pthreads, and alternative to OpenACC could be CUDA. Both alternatives suffer from the items mentioned above.

Despite of the similarities at this point, OpenMP and OpenACC were created targeting very different architectures. The idea being that eventually both will converge into a more general solution. We will now discuss each approach separately, at the end of this discussion we will talk about the efforts to converge both approaches.


{% include links.md %}
