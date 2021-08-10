---
title: "Exercises"
teaching: 0
exercises: 60
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

> ## Exercise 1:
>
> 1. Write a Fortran program that writes "Hello OpenMP".
>    Compile and execute.
>
> 2. Convert the previous Fortran program in multithreaded with a ``paralell``
>    directive
>
> 3. Load the omp_lib module and print also the thread number using the function ``omp_get_thread_num()`` and ``omp_get_num_threads()``
>
> 4. Store the values for the number of threads and thread ID on variables before printing
>
> 5. What is the difference bettween making those variables public or private.
>
{: .challenge}

> ## Exercise 2:
>
> 1. Write a Fortran code that computes the integral of function
> f(x)=log(pi*x)*sin(pi*x)**2 + x in the range [1,10]
>
> 2. Use rectangles to approximate the integral and use an OpenMP parallel loop.
>    (*) An alternative could be using trapezoids instead of triangles.
>
> 3. Compare the performance of the serial code compared with the parallel version.
>
> 4. Use the function omp_get_wtime() to get the time spend on the loop.
>    The function returns a real and can be used before and after the loop.
{: .challenge}

> ## Exercise 3:
>
> Use the following product to approximate pi
>
> <img src="https://latex.codecogs.com/svg.image?\frac{\pi}{2}&space;=&space;\left(\frac{2}{1}&space;\cdot&space;\frac{2}{3}\right)&space;\cdot&space;\left(\frac{4}{3}&space;\cdot&space;\frac{4}{5}\right)&space;\cdot&space;\left(\frac{6}{5}&space;\cdot&space;\frac{6}{7}\right)&space;\cdot&space;\left(\frac{8}{7}&space;\cdot&space;\frac{8}{9}\right)&space;\cdot&space;\;&space;\cdots&space;=&space;\prod_{n=1}^{\infty}&space;\left(&space;\frac{&space;4n^2&space;}{&space;4n^2&space;-&space;1&space;}&space;\right)" title="\frac{\pi}{2} = \left(\frac{2}{1} \cdot \frac{2}{3}\right) \cdot \left(\frac{4}{3} \cdot \frac{4}{5}\right) \cdot \left(\frac{6}{5} \cdot \frac{6}{7}\right) \cdot \left(\frac{8}{7} \cdot \frac{8}{9}\right) \cdot \; \cdots = \prod_{n=1}^{\infty} \left( \frac{ 4n^2 }{ 4n^2 - 1 } \right)" />
>
> 1. Use the reduction for a product to the an approximation of pi
>
> 2. How many terms are needed to get 4 good digits?
>
{: .challenge}


{% include links.md %}
