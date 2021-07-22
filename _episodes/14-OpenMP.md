---
title: "OpenMP"
teaching: 30
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

## OpenMP

The first specs for OpenMP are from 1997. Today most modern compilers support OpenMP 3.0 from 2008 and the 3 compilers that we will be using (GNU, Intel and NVIDIA) support most of the OpenMP 4.0 or OpenMP 4.5

OpenMP was created for parallelizing code for shared memory machines. Shared memory machines are those with multiple cores where the cores are able to see, ie address the entire memory of the machine.

Consider now a simple example, one step back from calculating `pi` and start with a simple print program (``hello_omp.f90``) to demonstrate how OpenMP works.

~~~
program hello

!$OMP PARALLEL

print *, "Hello electron"

!$OMP END PARALLEL

end program hello
~~~
{: .language-fortran}

Lets compile this code using ``gfortran``

~~~
$> gfortran -fopenmp hello_omp.f90 -o hello_omp
~~~
{: .language-bash}

Execute the resulting binary:

~~~
$> OMP_NUM_THREADS=4 ./hello_omp
~~~
{: .language-bash}
~~~
Hello electron
Hello electron
Hello electron
Hello electron
~~~
{: .output}

To produce a result that is consistent on all machines, we have define the variable ``OMP_NUM_THREADS=4`` so the code at runtime will create 4 threads regardless of the number of cores on the machine. Not defining this variable and the executable will create by default as many threads as cores it founds on the machine running the code.

In Fortran the directives ``!$OMP PARALLEL`` and ``!$OMP END PARALLEL`` will create a block, in this case a single line block, that will run in parallel. The number of threads that will be created is decided at runtime, at least for this simple parallel block, there is no information about how many threads will be generated.

Notice the how two lines of code made this code from running on a single core to run on multiple cores, without any provisions about how many cores it will run.
This level of abstraction is one of the big advantages of the approach.

Before OpenMP the alternative for multithreading in fortran could be something like Pthreads, there is a library for pthreads for IBM AIX machines, the Pthreads Library Module (f_pthread). In this case we will demonstrate the code in C which is shown below:

~~~
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#define PTH_NUM_THREADS 4

void *HelloElectron(void *threadid)
{
	printf("Hello electron\n");
	pthread_exit(NULL);
}

int main (int argc, char *argv[])
{
   pthread_t threads[PTH_NUM_THREADS];
   int rc;
   long t;
   for(t=0; t<PTH_NUM_THREADS; t++){
	rc = pthread_create(&threads[t], NULL, HelloElectron, (void *)t);
   	if (rc) exit(-1);
   }
   pthread_exit(NULL);
   return 0;
}
~~~
{: .language-c}

Notice how this code demands more coding for the same output.
The print function is now a function, the creation of each thread is explicit inside a for loop and the number of threads is known during compilation. This last restriction can be removed by adding more extra coding for the allocation of the threads array.

Parallelizing even a simple code will ask for changes to the original structure of the code. That makes the code harder to develop and harder to maintain for those who are not used to multithreading programming. Now the code depends on pthreads even if we change the preprocessor variable to run it serial.

## Processes vs threads

OpenMP works by creating threads inside a process and for that reason is important to know the difference between processes and threads. The difference is even more important when we consider MPI where processes potentially running on different machines communicate with each other via exchange of messages.

A process is an instance of a program.
Each process has a separate memory address space, which means that a process runs independently and is isolated from other processes.
A process cannot directly access shared data in other processes.
The kernel of the Operating System manages the execution of all processes, as machines today have multiple cores, that means that at most n processes can be running concurrently on a machine with n cores. As the number of processes is typically larger than the number of cores, each process is receiving CPU time during a certain time interval before the OS switching the core to another process that is demanding attention.
Switching from one process to another requires some time (relatively) for saving and loading registers, memory maps, and other resources.

From the other side, a thread is the unit of execution within a process.
A process can have just one thread or having many threads.
Contrary to multiple processes on a machine, each thread in the process shares memory and resources from that process. In single-threaded processes, the process contains one thread. The process and the thread are one and the same, and there is only one thing happening.

What OpenMP does is to create new threads on the same process and as those processes shared resources, the can read and write on the memory addresses assigned to the variables of a process, using this technique multiple threads can effectively complete tasks in parallel when there is data independence in the algorithm being executed.

Many applications today are multithreading, but the way multithreading and in particular OpenMP is used in scientific applications is very different from the multithreading found on desktop applications or games.

For HPC applications, the usual case is for multiple threads work concurrently of portions of big arrays, reducing values such as sums or products in in some cases dividing efforts in separate tasks. The life cycle of threads in OpenMP is more consistent compared for example to multiple threads on a game process. On a game new threads are generated and destroyed continuously based on actions of the user.

One of the reasons why parallelization is no a trivial task that can be automatically execute by a compiler by for example adding ``parallel`` directives to every loop in a program is due to a non trivial difficulty to identify **data dependencies** and **data races**.

### Data dependencies

Consider a ``DO`` loop like this:

~~~
A(0)=1
DO i=1, 100000
   A(i) = 4*B(i-1) + 3
ENDDO
~~~
{: .language-fortran}

Assuming that A and B are different arrays, on this loop you can notice that every element of the loop can be  computed independent from the others, in fact there is nothing that forces a given order in the calculation.
You can split the loop in 2 or 100 pieces and compute the smaller loops on arbitrary order without affecting the final result. That is very different from:

~~~
A(0)=1
DO i=1, 100000
   A(i) = 4*A(i-1) + 3
ENDDO
~~~
{: .language-fortran}

You cannot longer split the loop as computing every element in the loop requires the computation of the value on the previous index.

Sometimes data dependencies can be solved by reordering operations, separating loops. A data dependency like shown above cannot be eliminated without changing the algorithm itself or this portion of the code serial.

In many cases there are many loops in a code that do not have data dependencies, some of them worth of being parallelized.
Consider this simple initialization of an array (``simple_omp.f90``):

~~~
program simple

integer,parameter :: n=huge(1)
integer :: array(n)

print *,n
!$omp parallel do
do i = 1, n
   array(i) = sqrt(real(i))
enddo
!$omp end parallel do

end program
~~~
{: .language-fortran}

Compile and run this program with and without OpenMP.
For example with NVIDIA compilers:

~~~
$> nvgfortran -mp -Minfo=all simple_omp.f90
~~~
{: .language-bash}

You can notice than running this code on a modern computer could be
5 times faster with OpenMP that without it.
The size of the array is one reason, but also the fact that computing the square root of a number takes several CPU cycles to compute.
There are many cases with smaller arrays but more complex operations where the lack of data dependencies make those loops good candidates for parallelization.

In this case there is just one variable inside the loop and the variable ``array`` is shared among all the threads that are filling it. In more complex situations there are more variables and we need to decide which variables will be shared and which variables need private copies to prevent a single variable being overwritten by multiple threads.

### Shared and private variables

Consider this example code:

~~~
program ratio

   implicit none

   integer, parameter :: n = 40
   real, parameter :: pi = 3.14159265359

   integer :: i, cube_side
   real :: area_to_volume_S(n), area_to_volume_P(n), surface_sphere, volume_cube, diameter

   do i = 1, n
      cube_side = i
      diameter = sqrt(3*real(cube_side)**2)
      surface_sphere = pi*diameter**2
      volume_cube = real(cube_side)**3
      area_to_volume_S(i) = surface_sphere/volume_cube
   end do

!$OMP PARALLEL DO
   do i = 1, n
      cube_side = i
      diameter = sqrt(3*real(cube_side)**2)
      print *, i, cube_side
      surface_sphere = pi*diameter**2
      volume_cube = real(cube_side)**3
      area_to_volume_P(i) = surface_sphere/volume_cube
   end do
!$OMP END PARALLEL DO

   do i = 1, n
      if (abs(area_to_volume_S(i) - area_to_volume_P(i)) > 1e-2) then
         print *, "Wrong value for i=", i, area_to_volume_S(i), area_to_volume_P(i)
      end if
   end do

end program ratio
~~~
{: .language-fortran}

This code is computing an array ``area_to_volume`` that stores the ratio between the surface area of a sphere and the volume of the biggest cube that can be inside the sphere.

The code is doing computing all those values twice.
The same loop is computing  ``area_to_volume_S``, the serial version of the loop.
The second loop computes ``area_to_volume_P`` and has OpenMP directives which will effective parallelize the code if the compilation activates those directives.
Except for the ``print`` the content of both loops is the same and the content of the arrays should be identical.

Lets compile this code with nvfortran:

~~~
$> nvfortran -mp -Minfo=all race-loop_omp.f90 -o race-loop_omp
$> ./race-loop_omp
~~~
{: .language-bash}

~~~
1             2
9             2
38            2
6             2
11            2
21            2
31            2
34            2
5             2
20            2
26           28
30           28
12           28
23           28
28           28
10           28
18           28
16           28
2            28
33           28
40           28
14           28
3            28
37           28
7            28
39           28
4            28
36           28
17           28
27           28
24           28
29           28
13           28
35           28
25           28
15           28
19           28
32           28
8            28
22           28
Wrong value for i=            1    9.424778        4.712389    
Wrong value for i=            2    4.712389       0.3365992    
Wrong value for i=            3    3.141593       0.3365992    
Wrong value for i=            4    2.356194       0.3365992    
Wrong value for i=            5    1.884956        4.712389    
Wrong value for i=            6    1.570796        4.712389    
Wrong value for i=            7    1.346397       0.3365992    
Wrong value for i=            8    1.178097       0.3365992    
Wrong value for i=            9    1.047198        4.712389    
Wrong value for i=           10   0.9424779       0.3365992    
Wrong value for i=           11   0.8567981        4.712389    
Wrong value for i=           12   0.7853981       0.3365992    
Wrong value for i=           13   0.7249829       0.3365992    
Wrong value for i=           14   0.6731984       0.3365992    
Wrong value for i=           15   0.6283185       0.3365992    
Wrong value for i=           16   0.5890486       0.3365992    
Wrong value for i=           17   0.5543987       0.3365992    
Wrong value for i=           18   0.5235988       0.3365992    
Wrong value for i=           19   0.4960410       0.3365992    
Wrong value for i=           20   0.4712389       0.3365992    
Wrong value for i=           21   0.4487989        4.712389    
Wrong value for i=           22   0.4283990       0.3365992    
Wrong value for i=           23   0.4097730       0.3365992    
Wrong value for i=           24   0.3926991       0.3365992    
Wrong value for i=           25   0.3769911       0.3365992    
Wrong value for i=           26   0.3624915       0.3365992    
Wrong value for i=           27   0.3490659       0.3365992    
Wrong value for i=           29   0.3249924       0.3365992    
Wrong value for i=           30   0.3141593       0.3365992    
Wrong value for i=           31   0.3040251        4.712389    
Wrong value for i=           32   0.2945243       0.3365992    
Wrong value for i=           33   0.2855993       0.3365992    
Wrong value for i=           34   0.2771994        4.712389    
Wrong value for i=           35   0.2692794       0.3365992    
Wrong value for i=           36   0.2617994       0.3365992    
Wrong value for i=           37   0.2547237       0.3365992    
Wrong value for i=           38   0.2480205        4.712389    
Wrong value for i=           39   0.2416610       0.3365992    
Wrong value for i=           40   0.2356195       0.3365992    
~~~
{: .output}

Execute multiple times and different discrepancies will appear.
The reason for these errors is that variables such as ``cube_side``, ``diameter``, ``volume_cube``, ``surface_sphere`` are all scalars that are shared among all the threads created by OpenMP. One thread can rewrite the value that will be used by another thread evaluating values different from the serial loop.

What we want is to select some values to be **private**.
For that reason there is a clause called **private** where you can list all the variables that need to be copied on every thread.

The solution is to set the scalars as **private**.
New copies of those variables will be created for each thread created.

~~~
!$OMP PARALLEL DO PRIVATE(cube_side, diameter, volume_cube, surface_sphere)
   do i = 1, n
      cube_side = i
      diameter = sqrt(3*real(cube_side)**2)
      print *, i, cube_side
      surface_sphere = pi*diameter**2
      volume_cube = real(cube_side)**3
      area_to_volume_P(i) = surface_sphere/volume_cube
   end do
!$OMP END PARALLEL DO
~~~
{: .language-fortran}

### Data Sharing Attribute Clauses

These Data-sharing attribute clauses apply to parallel blocks.

| **clause** | Description |
|------------|-------------|
| *default(private \| firstprivate \|shared \| none)* | Explicitly determines the default data-sharing attributes of variables that are referenced in a parallel, teams, or task generating construct, causing all variables referenced in the construct that have implicitly determined data- sharing attributes to be shared.|
| *shared(list)* | Declares one or more list items to be shared by tasks generated by a parallel, teams, or task generating construct. The programmer must ensure that storage shared by an explicit task region does not reach the end of its lifetime before the explicit task region completes its execution.|
| *private(list)* | Declares one or more list items to be private to a task or a SIMD lane. Each task that references a list item that appears in a private clause in any statement in the construct receives a new list item.|
| *firstprivate(list)* | Declares list items to be private to a task, and initializes each of them with the value that the corresponding original item has when the construct is encountered.|
| *lastprivate(list)* | Declares one or more list items to be private to an implicit task or to a SIMD lane, and causes the corresponding original list item to be updated after the end of the region.|

These 4 clauses can be better understood with a simple example.
Consider the code below (``parallel_clauses_omp.f90``)

~~~
program clauses

   implicit none
   integer, parameter :: n=10
   integer :: i, a, b, c, d
   character(len=*), parameter :: layout_1 = "(a, 4(a, i12))"
   character(len=*), parameter :: layout_2 = "(a, i4, 4(a, i12))"

   a = 1
   b = 1
   c = 1
   d = 1

   print layout_1, "BEFORE", " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d

!$OMP PARALLEL DO SHARED(a) PRIVATE(b) FIRSTPRIVATE(c) LASTPRIVATE(d)
   do i = 1, n

      if (n<11) print layout_2, "I=", i, " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d
      a = a + i
      b = b + i
      c = c + i
      d = d + i
   end do
!$OMP END PARALLEL DO

   print layout_1, "AFTER ", " SHARED:", a, " PRIVATE:", b, " FIRSTPRIVATE:", c, " LASTPRIVATE:", d

end program clauses
~~~
{: .language-fortran}

We have 4 scalar variables, and we have assigned a value of 1 to each of them.
The loop will increase the value on each iteration.
Notice that the variable ``a`` receives all increments but as several threads are reading and writing the variable, race conditions could happen, rare events where one thread reads one value, computes the operation and writes back when another thread have done the same.
Variable ``b`` is private, but the value assigned before the loop is lost, each thread receives a new memory location and whatever is on that location will be the initial value.
Variables ``c`` and ``d`` are also private, but they differ if the initial value inside the loop is initialized or the final value is stored after the loop finishes.

Change the value of n to a very large value, and notice that a produces a different result on each run, those are very rare events so only with a very large loop is possible to trigger them.

For beginners is a good practice to force an explict declaration of each variable.
Use *default(none)* and you have to decide explictly if a variable is shared or private.

The default rules in Fortran for sharing are:

Default is shared, except for things that can not possibly be:

  * Outer loop index variable
  * Inner loop index variables (Fortran)
  * Local variables in any called subroutine, unless using *save* (Fortran).

Consider this example in Fortran:

~~~
!$OMP PARALLEL DO
do i=1, n
   do j=1, m
      a(j,i) = b(j,i) + c(j,i)
   end do
end do
!$OMP END PARALLEL DO
~~~

The variable ``i`` is always private by default,
For OpenMP in Fortran the variable ``j`` is also private but this is not true for OpenMP in C.

It is good to remember that the most optimal way of traverse index on arrays is by operating on adjacent elements in memory. In the case of Fortran the fast index is the first one, contrary to C where the adjacent elements run on the second index.

## Reductions

Private variables die at the end of the thread. We can use ``lastprivate`` but the value that is copied to the master thread is only the value for the last iteration in the loop. What if we need to collect somehow the values from all the threads at the end of the loop. Here is where reductions became important.
Consider this example where we are using a series converging to Euler's number:

~~~
program reduction

   implicit none
   integer, parameter :: r15 = selected_real_kind(15)
   integer, parameter :: n = 40
   integer :: i, j

   real(kind=r15) :: euler = 1.0
   real(kind=r15) :: facto
   real(kind=r15), parameter :: e_ref = 2.71828182845904523536028747135266249775724709369995_r15

   !$OMP PARALLEL DO default(none) private(facto) reduction(+:euler)
   do i = 1, n

      facto = real(1)

      ! Not need to run this internal loop in the serial case
      ! Can you remove it and still use OpenMP?
      do j = 1, i
         facto = facto*j
      end do
      euler = euler + real(1)/facto
      print *, "I= ", i, "Euler Number: ", euler, "Error: ", abs(euler - e_ref)

   end do
   !$OMP END PARALLEL DO

   print *, "Final Euler Number: ", euler

end program
~~~
{: .language-fortran}

We need the variable ``euler`` to be private to avoid data races, where the variable is modified inconsistently by threads in the loop.
We also need the variable be initialized with zero as terms of the series will be accumulated there.
Finally, we need the partial sums for ``euler`` been added all together to obtain the correct value for the constant.

All that is done by adding ``reduction(+:euler)``.
This is a reduction clause that automatically make the variable private, initialize with zero for the child threads and the partial results added with the master thread value for ``euler=1.0``.

Reductions are very important in many cases with loops. These are some reductions available for do loops:

### Implicitly Declared Fortran reduction-identifiers

| **Identifier** | **Initializer** | **Combiner** |
|----------------|-----------------|--------------|
| ``+`` | omp_priv = 0 | omp_out = omp_in + omp_out |
| ``*`` | omp_priv = 1 | omp_out = omp_in * omp_out |
| ``-`` | omp_priv = 0 | omp_out = omp_in + omp_out |
| ``.and.`` | omp_priv = .true. | omp_out = omp_in .and. omp_out |
| ``.or.`` | omp_priv = .false. | omp_out = omp_in .or. omp_out |
| ``.eqv.`` | omp_priv = .true. | omp_out = omp_in .eqv. omp_out |
| ``.neqv.`` | omp_priv = .false. | omp_out = omp_in .neqv. omp_out |
| ``max`` | omp_priv = Least representable number in the reduction list item type | omp_out = max( omp_in, omp_out) |
| ``min`` | omp_priv = Largest representable number in the reduction list item type | omp_out = min( omp_in, omp_out) |
| ``iand`` | omp_priv = All bits on | omp_out = iand( omp_in, omp_out) |
| ``ior`` | omp_priv = 0 | omp_out = ior( omp_in, omp_out) |
| ``ieor`` | omp_priv = 0 | omp_out = ieor( omp_in, omp_out) |

## A more general Parallel construct

So far we have been using ``!$OMP PARALLEL DO``.
This is a convenient construct for parallelizing ``DO`` loops in fortran.
OpenMP offers a more general construct ``!$OMP PARALLEL`` that ask the system to span a number of threads and leave how those therads are used to other constructs that can use those threads.


{% include links.md %}
