---
title: "Exercises"
teaching: 0
exercises: 60
---

>## Exercise 1
>
> Write a Fortran code that print its rank and total number of processes in
> the communicator.
>
{: .challenge}

> ## Exercise 2:
>
> Write a Fortran code that computes the integral of function
>
>    f(x)=log(pi*x)*sin(pi*x)**2 + x in the range [1,10]
>
> distributing the rectangles across multiple processes using MPI
>
{: .challenge}

> ## Exercise 3:
>
> Write a Fortran code that distributes a long array of random numbers and use
> each process will compute the average value of the section of the array
> allocated.
>
> Use collective operations to return the maximum and minimum of those averages.
>
{: .challenge}




{% include links.md %}
