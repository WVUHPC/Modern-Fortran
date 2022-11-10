---
title: "Exercises"
teaching: 0
exercises: 60
---

> ## Exercise 1: Valid Fortran Lines
>
> Which of these lines are not valid in Fortran
>
> ~~~
> x = y
> a = b+c ! add
> word = ’electron’
> a = 1.0; b = 2.0
> a = 15. ! initialize a; b = 22. ! and b
> quarks = "Top, Bottom &
            up, down &
>          & charm and strange"
> bond = ’Covalent or,
>         Ionic!’
> c(3:4) = ’up"
> d = `très grave`
> ~~~
> {: .source}
>
{: .challenge}

> ## Exercise 2: Valid Fortran literals
>
> Which of these is a valid literal constant (One of the 5 intrinsic types)
>
> ~~~
> -43
> ’word’
> 4.39
> 1.9-4
> 0.0001e+20
> ’stuff & nonsense’
> 4 9
> (0.,1.)
> (1.e3,2)
> ’I can’’t’
> ’(4.3e9, 6.2)’
> .true._1
> e5
> ’shouldn’ ’t’
> 1_2
> "O.K."
> z10
> ~~~
> {: .source}
>
{: .challenge}


> ## Exercise 3: Valid Fortran names
>
> Which of these are valid names in Fortran
>
> ~~~
> name
> program
> TRUE
> 123
> a182c3
> no-go
> stop!
> burn_
> no_go
> long__name
> ~~~
> {: .source}
>
{: .challenge}

> ## Exercise 4: Array definition and constructor
>
> Write an array definition for complex elements in the range [-100,100].
> How many elements the array will have?
> Write a constructor where the value of each element in the array is the square root of the corresponding index. Use a compact constructor (implicit loop).
>
>
{: .challenge}

> ## Exercise 5: Derived type
>
> Write a derived type appropriated for a point-like classical particle.
> The particle has a mass and charge.
> The derived type should be able to store the position and velocity (3D) of the particle. Write an example of a derived type constant for this particle.
>
>
{: .challenge}

> ## Exercise 6: Reals with larger precision
> Write the declaration of a real variable with a minimum of 12 decimal digits of precision and a range of 10^{−100} to 10^{100}
>
>
{: .challenge}

> ## Exercise 7: Center of mass
>
> Write a program that reads a file or randomly creates an array that stores the position of N particles in a 3D space.
> Initially assume for all the particles have the same mass.
> Write a subroutine to compute the center of mass of these particles.
>
{: .challenge}

> ## Exercise 8: Lennard-Jones Potential
>
> Write a program that reads a file or randomly creates an array that stores the position of N particles in a 3D space.
> A very simple potential for Molecular Dynamics consists in assuming a potential like:
> <img src="https://latex.codecogs.com/svg.image?V_\text{LJ}(r)&space;=&space;4\varepsilon&space;\left[&space;\left(\frac{\sigma}{r}\right)^{12}&space;-&space;\left(\frac{\sigma}{r}\right)^6&space;\right]" title="V_\text{LJ}(r) = 4\varepsilon \left[ \left(\frac{\sigma}{r}\right)^{12} - \left(\frac{\sigma}{r}\right)^6 \right]" />
>
> Write a program that computes the potential and forces for the set of particles.
>
> (*) Add simple dynamics, moving the particles according to the direction of the force (Gradient of the Potential).
>
>
{: .challenge}

> ## Exercise 9: Quadratic Equation
>
> Write a program to calculate the roots (real or complex) of the quadratic equation ax2 + bx + c = 0 for any real values of a, b, and c.
> Read the values of these three constants and print the results.
> Check the
Use should be made of the appropriate intrinsic functions.
>
> Check if a is zero and return the appropriate solution for the linear case.
{: .challenge}


> ## Exercise 10: Mean and Variance
>
>  In this exercise we will explore the use of subroutines random numbers and optional arguments.
>
> The average of a sample of values is:
>
> <img src="https://latex.codecogs.com/svg.image?\mu&space;=&space;\frac{1}{n}&space;\sum_{i=1}^n&space;x(i)" title="\mu = \frac{1}{n} \sum_{i=1}^n x(i)" />
>
> And variance is computed as:
>
> <img src="https://latex.codecogs.com/svg.image?\operatorname{Var}(X)&space;=&space;\frac{1}{n}&space;\sum_{i=1}^n&space;(x_i&space;-&space;\mu)^2&space;&space;" title="\operatorname{Var}(X) = \frac{1}{n} \sum_{i=1}^n (x_i - \mu)^2 " />
>
> Write a program that:
>
> 1. Ask the size N of the array, via command line arguments or read from standard input.
>    
> 2. After reading N, allocate an array filled with random numbers.
>
> 3. Write subroutines for computing the average and variance.
>
> 4. (*) Reconvert the program to use modules and extend the capabilities of the code to work with multidimensional arrays and be capable of returning averages along specific dimensions.
{: .challenge}

> ## Exercise 11: Toeplitz decomposition
>
> Any square matrix can be decomposed into a symmetric and skew-symmetric matrix, following the prescription:
>
> <img src="https://latex.codecogs.com/svg.image?X&space;=&space;\frac{1}{2}\left(X&space;&plus;&space;X^\textsf{T}\right)&space;&plus;&space;\frac{1}{2}\left(X&space;-&space;X^\textsf{T}\right)&space;" title="X = \frac{1}{2}\left(X + X^\textsf{T}\right) + \frac{1}{2}\left(X - X^\textsf{T}\right) " />
>
> Write a program that creates a random matrix NxN (fix N or ask for it)
> Write a subroutine that takes the matrix and returns two arrays, one for the symmetric and another for the skew-symmetric matrix.
>
> (hint): You can write your own transpose function or use the intrinsic ``transpose``
>  
> (*) Similar case but for complex matrices. The decomposition using the conjugate transpose.
>
{: .challenge}

> ## Exercise 12: Matrix Multiplication
>
> Write a code that computes the matrix multiplication.
> Compare your code with the intrinsic function ``matmul(a,b)``
>
>
{: .challenge}

> ## Exercise 13: Quaternions
>
> Create a derived type for working with quaternions.
> Quaternions are generally represented in the form:
>
> <img src="https://latex.codecogs.com/svg.image?a&space;&plus;&space;b\&space;\mathbf&space;i&space;&plus;&space;c\&space;\mathbf&space;j&space;&plus;d\&space;\mathbf&space;k&space;" title="a + b\ \mathbf i + c\ \mathbf j +d\ \mathbf k " />
>
> The values of a, b, c, and d define a particular quaternion.
> Quaternions can operate under addition and subtraction.
> The multiplication is special and obeys a table shown below
>
> |   | 1 | i | j | k |
> |---|---|---|---|---|
> | 1 | 1 | i | j | k |
> | i	| i	| −1|	k	| −j|
> | j	| j	| −k|	−1|	i |
> | k	| k	| j |	−i|	−1|
>
> Write a program that defines a derived type for quaternions and write functions to operate with them.
> Write a function to compute the inverse:
>
> <img src="https://latex.codecogs.com/svg.image?(a&space;&plus;&space;b\,\mathbf&space;i&space;&plus;&space;c\,\mathbf&space;j&space;&plus;&space;d&space;\,\mathbf&space;k)^{-1}&space;=&space;\frac{1}{a^2&space;&plus;&space;b^2&space;&plus;&space;c^2&space;&plus;&space;d^2}\,(a&space;-&space;b\,\mathbf&space;i&space;-&space;c\,\mathbf&space;j-&space;d\,\mathbf&space;k)" title="(a + b\,\mathbf i + c\,\mathbf j + d \,\mathbf k)^{-1} = \frac{1}{a^2 + b^2 + c^2 + d^2}\,(a - b\,\mathbf i - c\,\mathbf j- d\,\mathbf k)" />
>
> (***) Use a module with an interface operator star (``*``) to produce a clean implementation of the multiplication of quaternions.
>
>
> > ## Hint
> >
> > This code shows how to create an interface for the operator (``+``). Implement the product using the table for multiplication.
> >
> > ~~~
> > module mod_quaternions
> >
> >   type quaternion
> >      real :: a, b, c, d
> >   end type quaternion
> >
> >   interface operator(+)
> >      module procedure sum_qt
> >   end interface
> >
> > contains
> >
> >   function sum_qt(x, y)
> >      type(quaternion)             :: sum_qt
> >      type(quaternion), intent(in) :: x, y
> >      sum_qt%a = x%a + y%a
> >      sum_qt%b = x%b + y%b
> >      sum_qt%c = x%c + y%c
> >      sum_qt%d = x%d + y%d
> >   end function sum_qt
> >
> > end module mod_quaternions
> >
> > program main
> >
> >   use mod_quaternions
> >
> >   implicit none
> >
> >   type(quaternion) :: x = quaternion(1.0, 2.0, 3.0, 4.0)
> >   type(quaternion) :: y = quaternion(5.0, 6.0, 7.0, 8.0)
> >   type(quaternion) :: z
> >
> >   z = x + y
> >
> >   print *, z%a, z%b, z%c, z%d
> >
> > end program
> > ~~~
> > {: .language-fortran}
> >
> >
>{: .solution}
>
{: .challenge}


{% include links.md %}
