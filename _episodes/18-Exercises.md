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

> ## Exercise 01: Valid Fortran Lines
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

> ## Exercise 02: Valid Fortran literals
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


> ## Exercise 03: Valid Fortran names
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

> ## Exercise 04: Array definition and constructor
>
> Write an array definition for complex elements in the range [-100,100].
> How many elements the array will have?
> Write a constructor where the value of each element in the array is the square root of the corresponding index. Use an compact constructor (implicit loop).
>
>
{: .challenge}

> ## Exercise 05: Derived type
>
> Write a derived type appropriated for a point-like classical particle.
> The particle has a mass and charge.
> The derived type should be able to store position and velocity (3D) for the particle. Write an example of a derived type constant for this particle.
>
>
{: .challenge}

> ## Exercise 06:
> Write the declaration of a real variable with a minimum of 12 decimal digits of precision and a range of 10^{−100} to 10^{100}
>
>
{: .challenge}


{% include links.md %}
