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

# OpenACC

OpenACC is another directive-base model for parallel programming.
It is similar to OpenMP but focused more on Accelerators rather than Multicore processors as OpenMP.

Similar to OpenMP, the idea is to take the original code and introduce directives in places that could benefit from parallel programming.
If you use only directives, the code still compiles with compilers that do not support OpenACC and the code will run serially as usual.

Currently, the best support for OpenACC comes from NVIDIA compilers (nvfortran), followed by GCC compilers (gfortran). Intel compilers do not support OpenACC and there is no plan for support it in the near future.

In this lesson we will follow a similar path as we did for OpenMP.
First we parallelize a loop and later we generalize the operation to more abstract parallel entities, such as tasks.
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


Beyond using OpenACC directives you could prefer to have  a fine detail on the parallel execution.
OpenACC offers an API similar to OpenMP which also offer one.
Using them and the code will only be able to work with the OpenACC runtime environment.

In fortran the API is activated by including the module ``openacc``

~~~
use openacc
~~~
{: .source}



{% include links.md %}
