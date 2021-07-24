---
title: "Best Practices in Modern Fortran"
teaching: 30
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

# Best Practices in Modern Fortran

Fortran is a language with a long history.
Fortran is one of the oldest Programming Languages still in active use.
That old history means that the language have evolved in expression and scope in more than 60 years.

The way you programmed Fortran back in the 60s, 70s and 80s is completely inadequate to modern computers.
Consider for example the length of the lines.
Back in the 70s and 80s people programmed computers using punched cards
and later dumb terminals connected to mainframes.
Those terminals have a limited number of characters in one line.
It is from that time that a limit of 72 characters on one line and characters beyond column 72 were ignored.
The first five columns must be blank or contain a numeric label.
Continuation lines were identified by a nonblank, nonzero in column 6.
All these limitations were present on what is called Standard Fixed Format of Fortran 77.

Over time, these limitations and many others in the language became obsolete but were still in use for decades even after Fortran 90 was the new Standard.
Back in the 90s, there were no free Fortran 90 compilers, people continue to use Fortran 77 with restrictions relaxed and some characteristics of Fortran 90 integrated little by little in the compilers.

The fact is that there are a many codes written in Fortran 77.
If you search for Fortran tutorials and documentation, there are many pages in academic that still teach Fortran 77.
Fortran 90 was gaining space and later Fortran 95.
Today almost main Fortran compilers support Fortran 2003 entirely and Fortran 2008 to a fairly complete degree. New features in Fortran 2018 are still not implemented in the latest version of major compilers.

In previous sections we have discussed the basic elements so you can read and write code in Fortran.
We learn about variables assignments subroutines and modules and we explored those elements using the modern ways of writing code in Fortran.
In this section we will have to learn a bit the "old ways" is such a way that when you encounter those pieces of code, you can recognize them knowing which are the alternatives and best practices used today.
This is in this way and intermediate class in Modern Fortran.
I assume you know the basics and is able to write useful code in Fortran.
The idea is to recognize old Fortran 77 and Fortran 90 styles and being able to translate those into modern Fortran 2003 and 2008 Standards.
By doing that you are moving the code into more flexible, readable, extensible and potentially with more performance.

## The old Fortran 77

Fortran 77 was a powerful language back in its time.
It was so powerful that stay as the standard *de facto* well in the 90s.
Many elements of the language still in use today were added to Fortran 77 to address the limitations of FORTRAN 66.
For example the ``IF`` and ``END IF`` statements including ``ELSE`` and ``ELSE IF`` were the first steps into structured programming.
These statements were an important move to move ``GOTO`` into oblivion.
``GOTO`` were one of the reasons why codes were so difficult to follow and almost unpredictable when were the source of a bug.
Another powerful element of the language were intrinsic functions such as ``SQRT`` that accept complex or double precision numbers as arguments.  
All these niceties today are taken for granted but back them were big improvements to the language.
However, the language has important flaws.
One important is the lack of dynamic memory allocation.
The code still promote a code that is cluttered, with missing blanks and in general hard to read.

Now the question is, if a code works in Fortran 77 why to change it?
Investing some time now in rewriting a code in a new language could return in big gains later on.
A new structure could emerge that use better the memory (with dynamic allocation) structure the code in ways that make it easier to read and to extend.
There are good reasons and rewards for better and cleaner code.

## Free Format

Back to the first versions of Fortran, the first 5 columns of characters were dedicated as label fields.
A C in column 1 means that the line were treated as a comment and any character in column 6 means that the line was a continuation of the previous statement.

Today those restrictions are no longer needed.
Statements can start at the first column.
An exclamation mark starts a comment and comments could be inserted any place in the line, except literal strings.
Blanks help readability of the code.
Variables cannot have spaces, it is a better practice to use underscore for multi word variables.
Use ``&`` as the last character for continuing on the next line.
Declaring multiple statements on the same line is possible with semicolon (``;``) but use it only for very short statements.

Example (``example_1.f90``):

~~~
program free_form
   print *, 'This statement starts in column 4 (with indentation)'
   x = 0.1; y = 0.7 ! Two small statements in one line
                    ! Comment with an exclamation mark
   tan_x_plus_y = tan(x) + tan(y) / &  ! Line with continuation
           (1- tan(x)*tan(y))
end program free_form
~~~
{: .language-fortran}

Use blank characters, blank lines and comments to improve readability of your code.
A cluttered code is hard to read and you are not doing any good trying to fit most of your code in one screen page.

Instead of this (``example_2_bad.f90``):

~~~
!BAD CODE
program log_sqrt
x=99.9
y=log10(sqrt(x))
if(y.lt.1.0) print *,'x is less than 100'
end program
~~~
{: .language-fortran}

This code is too compact, it is not hard to read because is too small, but for bigger codes became really hard to follow.

An alternative is like this (``example_2_bad.f90``):

~~~
!GOOD
program log_sqrt

   x = 99.9
   y = log10(sqrt(x))

   if (y .lt. 1.0) &
           print *, 'x is less than 100'

end program log_sqrt
~~~
{: .language-fortran}

We include spaces to clarify variable assignments from the conditional.
Indentations inside the ``program`` and ``end program`` also help to visualize the scope of blocks.

## Old style DO loops

In old FORTRAN 77, do loops have a number identifier to jump to a ``continue`` statement to cycle the loop, that is completely obsolete and must be avoided in modern coding (``example_3_bad.f90``):

~~~
      PROGRAM square
      DO 100 I=1,100
      X=I
      X2=X*X
      IF(X2.LT.100) print *, 'X=', I, ' X^2 wil have less than 3 digits'
100   CONTINUE
      END
~~~
{: .language-fortran}

This old style coding waste 6 columns of space, uses a labeled statement for cycling and is written with full capitalization.

An alternative is something like this (``example_3_good.f90``):

~~~
program square

   implicit none

   real :: x, x2
   integer :: i

   do i = 1, 100
      x = i
      x2 = x*x
      if (x2 < 100) &
         print *, 'X=', I, ' X^2 wil have less than 3 digits'
   end do

end program
~~~
{: .language-fortran}

This code uses indentations, and has an ``end do`` that is clearer for the user when the loops grows or became nested. We will discuss the ``implicit`` down below

## Variable Attributes

Fortran 95 introduce variable attributes and they were extended in Fortran 2003 and 2008. The most common attributes are
``parameter``, ``dimension``, ``allocatable``, ``intent``, ``pointer``, ``target``, ``optional``, ``private``, ``public``, ``value`` and ``bind``

From those we will demonstrate the first 3.
``parameter`` is important for declaring constants that otherwise will require going beyond the language and use the preprocessor.
``dimension`` is use to define the length of arrays or the dimension using colon and commas to declare it.
``allocatable`` is used for dynamic allocation of arrays an important feature introduced in Fortran 90 and expanded in Fortran 95 and beyond.
This is an example using these 3 attributes that should be frequently used (``example_4.f90``).

~~~
program f95attributes

   implicit none

   integer :: i, j
   integer, parameter :: n = 100
   real :: x
   real, parameter :: pi = 3.141592653
   real, dimension(n) :: array
   real, dimension(:, :), allocatable :: dyn_array2d

   allocate (dyn_array2d(2*n, 2*n))

   do i = 1, 2*n
      do j = 1, 2*n
         dyn_array2d(j, i) = j + 0.0001*i
      end do
   end do

   do i = 1, 10
        print '(10(F9.4))', dyn_array2d(i,1:10)
   end do

end program
~~~
{: .language-fortran}

## implicit none

By default all variables starting with ``i``, ``j``, ``k``, ``l``, ``m`` and ``n`` are integers.
All others are real.

Despite of this being usually true for small codes, it is better to turn those defaults off with ``implicit none``
Variables in large codes will have names with scientific meaning and the defaults can bring bugs that are hard to catch.

~~~
program implicit
   ! use to disable the default
   implicit none

   real :: J_ij !Interaction factor in Ising Model

end program
~~~
{: .language-fortran}

## Loops, exit and cycle

Use ``exit`` to abandon a loop and ``cycle`` to jump to the next iteration.
These are better replacements to complicated ``GOTO`` statements from old FORTRAN.
You can use ``exit`` and ``cycle`` in bounded and unbounded loops.

The file ``example_5.f90`` and ``example_6.f90`` show the effect of ``exit`` and ``cycle`` respectively.

~~~
program do_exit

   implicit none

   integer :: i
   real, parameter :: pi = 3.141592653
   real :: x, y

   do i = 0, 360, 10
      x = cos(real(i)*pi/180)
      y = sin(real(i)*pi/180)

      if (abs(x) < 1E-7) then
         print *, "Small denominator (exit)"
         exit
      end if
      print *, i, x, y, y/x
   end do

   print *, 'Final values:', i, x

end program
~~~
{: .language-fortran}

~~~
program do_cycle

   implicit none

   integer :: i
   real, parameter :: pi = 3.141592653
   real :: x, y

   do i = 0, 360, 10
      x = cos(real(i)*pi/180)
      y = sin(real(i)*pi/180)

      if (abs(x) < 1E-7) then
         print *, "Small denominator (cycle)"
         cycle
      end if
      print *, i, x, y, y/x
   end do

   print *, 'Final values:', i, x

end program
~~~
{: .language-fortran}

In the case of nested loops, the solution is to name the loop.
Constructs such as ``do``, ``if`` as ``case`` can be named and you can use the name to leave outer loops from inside a inner loop.

~~~
...
outer: do i = 0, 360, 10
   inner: do j = 0, 360, 10
      x = cos(real(i)*pi/180)
      y = sin(real(j)*pi/180)

      if (abs(x) < 1E-7) then
         print *, "Small denominator (exit)"
         exit outer
      end if
      print *, i, x, y, y/x
   end do inner
end do outer
...
~~~
{: .language-fortran}

~~~
...
outer: do i = 0, 360, 10
   inner: do j = 0, 360, 10
      x = cos(real(i)*pi/180)
      y = sin(real(j)*pi/180)

      if (abs(x) < 1E-7) then
         print *, "Small denominator (cycle)"
         cycle outer
      end if
      print *, i, x, y, y/x
   end do inner
end do outer
...
~~~
{: .language-fortran}

## The ``case`` construct

The ``case`` construct in Fortran works different from its homologous in C language.
In C you need to use a break, otherwise the code will test every single case.
In Fortran once one case passes the condition, the others will be skipped.
In Fortran case take ranges apart from a single element as other languages.
Example:

~~~
integer :: temp_gold

! Temperature in Kelvin!
select case (temp_gold)
case (:1337)
write (*,*) ’Solid’
case (1338:3243)
write (*,*) ’Liquid’
case (3244:)
write (*,*) ’Gas’
case default
write (*,*) ’Invalid temperature’
end select
~~~
{: .language-fortran}

## The ``kind`` of variables

There are at least 2 kinds of reals: 4-byte, 8-byte.
Some compilers offer a third kind with 16-byte reals.
The kind-numbers are usually 4, 8, and 16, but this is just a tradition of several languages and not mandatory by the language.
The kind values could perfectly (1, 2 and 4).

There is an intrinsic module called ``iso_fortran_env`` that provide the kind values for logical, character, integer and real data types.

Consider this example to get the values used (``example_7.f90``)

~~~
program kinds

   use iso_fortran_env

   implicit none

   print *, 'Logical  : ', logical_kinds
   print *, 'Character: ', character_kinds
   print *, 'Integer  : ', integer_kinds
   print *, 'Real     : ', real_kinds

end program kinds
~~~
{: .language-fortran}

Different compilers will respond with different kinds values.
For example, ``gfortran`` will return this:

~~~
Logical  :            1           2           4           8          16
Character:            1           4
Integer  :            1           2           4           8          16
Real     :            4           8          10          16
~~~
{: .output}

As Fortran have evolve over the years, several ways were created to declare the *storage size* of different kinds and consequently the *precision* of them. This example explores some of those old ways that you can still encounter in codes.

Consider this example illustrative of the multiple ways of declaring REAL variables (``example_8.f90``):

~~~
program kinds

   use iso_fortran_env

   implicit none

   integer :: i, my_kind

   real :: x_real              ! the default
   real*4 :: x_real4           ! Real with 4 bytes
   real*8 :: x_real8           ! Real with 8 bytes
   DOUBLE PRECISION :: x_db    ! Old way from FORTRAN 66

   integer, parameter :: k9 = selected_real_kind(9)
   real(kind=k9) :: r

   print *, 'Kind for integer            :', kind(i)
   print *, 'Kind for real               :', kind(x_real)
   print *, 'Kind for real*4             :', kind(x_real4)
   print *, 'Kind for real*8             :', kind(x_real8)
   print *, 'Kind for DOUBLE PRECISION   :', kind(x_db)
   print *, ''

   my_kind = selected_real_kind(9)
   print *, 'Which is the "kind" I should use to get 9 significant digits?  ', my_kind

   my_kind = selected_real_kind(15)
   print *, 'Which is the "kind" I should use to get 15 significant digits? ', my_kind

   r = 2._k9;
   print *, 'Value for k9', k9
   print *, 'Square root of 2.0 for default real      :', sqrt(2.0)   ! prints 1.41421354
   print *, 'Square root of 2.0 for DOUBLE PRECISION  :', sqrt(2.0d0) ! prints 1.41421354
   print *, 'Square root of 2.0 for numer of kind(k9) :', sqrt(r)     ! prints 1.4142135623730951

end program
~~~
{: .language-fortran}

The original REAL data type have received multiple variations for declaring floating point numbers based on rather ambiguous terms such as ``DOUBLE PRECISION`` which actually does not mean what literally says.
Other variations use the kind assuming that the numbers 4, 8, and 16 represent the number of bytes used by each data type, this is not standard and salford f95 compiler used kinds 1,2, and 3 to stand for 2- 4- and 8-byte.

Notice that storage size is not the same as precision. Those terms are related and you expect that more bytes will end up giving more precision, but REALS have several internal components such as the size of mantissa and exponent.
The 24 bits (including the hidden bit) of mantissa in a 32-bit floating-point number represent about 7 significant decimal digits.

Even though, this is not the same across all the real space.
We are using the same number of bits to represent all normalized numbers, the smaller the exponent, the greater the density of truncated numbers.
For example, there are approximately 8 million single-precision numbers between 1.0 and 2.0, while there are only about 8 thousand numbers between 1023.0 and 1024.0.  

Beyond the standard representation, you can also change the *storage size* during compilation.
Below, the same code was compiled using arguments that change the storage size of different variables.

~~~
$> gfortran example_8.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 Kind for integer            :           4
 Kind for real               :           4
 Kind for real*4             :           4
 Kind for real*8             :           8
 Kind for DOUBLE PRECISION   :           8

 Which is the "kind" I should use to get 9 significant digits?             8
 Which is the "kind" I should use to get 15 significant digits?            8
 Value for k9           8
 Square root of 2.0 for default real      :   1.41421354    
 Square root of 2.0 for DOUBLE PRECISION  :   1.4142135623730951     
 Square root of 2.0 for numer of kind(k9) :   1.4142135623730951     
 ~~~
 {: .output}
 ~~~
$> gfortran -fdefault-real-16 example_8.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 Kind for integer            :           4
 Kind for real               :          16
 Kind for real*4             :           4
 Kind for real*8             :           8
 Kind for DOUBLE PRECISION   :          16

 Which is the "kind" I should use to get 9 significant digits?             8
 Which is the "kind" I should use to get 15 significant digits?            8
 Value for k9           8
 Square root of 2.0 for default real      :   1.41421356237309504880168872420969798      
 Square root of 2.0 for DOUBLE PRECISION  :   1.41421356237309504880168872420969798      
 Square root of 2.0 for numer of kind(k9) :   1.4142135623730951     
~~~
{: .output}
~~~
$> gfortran -fdefault-real-16 -fdefault-double-8 example_8.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 Kind for integer            :           4
 Kind for real               :          16
 Kind for real*4             :           4
 Kind for real*8             :           8
 Kind for DOUBLE PRECISION   :           8

 Which is the "kind" I should use to get 9 significant digits?             8
 Which is the "kind" I should use to get 15 significant digits?            8
 Value for k9           8
 Square root of 2.0 for default real      :   1.41421356237309504880168872420969798      
 Square root of 2.0 for DOUBLE PRECISION  :   1.4142135623730951     
 Square root of 2.0 for numer of kind(k9) :   1.4142135623730951      
~~~
{: .output}

Fortran 2008 includes standard kinds ``real32``, ``real64``, ``real128`` to specify a REAL type with a storage size of 32, 64, and 128 bits. In cases where target platform does not support the particular kind a negative value is returned.

This example shows the new kind parameters (``example_9.f90``).

~~~
program newkinds

   use iso_fortran_env

   implicit none

   real(kind=real32) :: x32
   real(kind=real64) :: x64
   real(kind=real128) :: x128

   print *, 'real32  : ', real32, achar(10), &
           ' real64  : ', real64, achar(10), &
           ' real128 : ', real128

   x32 = 2.0
   x64 = 2.0
   x128 = 2.0

   print *, 'SQRT(2.0) using kind=real32  :', sqrt(x32)
   print *, 'SQRT(2.0) using kind=real64  :', sqrt(x64)
   print *, 'SQRT(2.0) using kind=real128 :', sqrt(x128)

end program
~~~
{: .language-fortran}

The storage size of these variables is no longer affected by the compiler arguments used above.

~~~
$> gfortran example_9.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 real32  :            4
 real64  :            8
 real128 :           16
 SQRT(2.0) using kind=real32  :   1.41421354    
 SQRT(2.0) using kind=real64  :   1.4142135623730951     
 SQRT(2.0) using kind=real128 :   1.41421356237309504880168872420969818      
 ~~~
 {: .output}
 ~~~
$> gfortran -fdefault-real-16 -fdefault-double-8 example_9.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 real32  :            4
 real64  :            8
 real128 :           16
 SQRT(2.0) using kind=real32  :   1.41421354    
 SQRT(2.0) using kind=real64  :   1.4142135623730951     
 SQRT(2.0) using kind=real128 :   1.41421356237309504880168872420969818      
~~~
{: .output}

You can still change those kinds during compile time using command line arguments ``-freal-4-real-10``, ``-freal-8-real-10`` and similar ones.

~~~
$> gfortran -freal-4-real-10 -freal-8-real-10 example_9.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 real32  :            4
 real64  :            8
 real128 :           16
 SQRT(2.0) using kind=real32  :   1.41421356237309504876      
 SQRT(2.0) using kind=real64  :   1.41421356237309504876      
 SQRT(2.0) using kind=real128 :   1.41421356237309504880168872420969818      
~~~
{: .output}

~~~
$> gfortran -freal-4-real-16 -freal-8-real-16 example_9.f90
$> ./a.out
~~~
{: .language-bash}
~~~
 real32  :            4
 real64  :            8
 real128 :           16
 SQRT(2.0) using kind=real32  :   1.41421356237309504880168872420969818      
 SQRT(2.0) using kind=real64  :   1.41421356237309504880168872420969818      
 SQRT(2.0) using kind=real128 :   1.41421356237309504880168872420969818      
~~~
{: .output}

Changing kinds during compile time could have unintended consequences, for example using external libraries as could be the case with MPI.

## Allocatable arrays

There are two types of memory for a program: The *stack* and the *heap*.
Scalars and static arrays live in the *stack* but the size of that space is very limited and some sysadmins and queue systems limits its value even more.
Most other variables including allocatable arrays live on the heap.

Before allocatable arrays were part of Fortran 90, Arrays were created with fixed size.
Programs used arrays with sizes that overstimated the actual needs for storage or require being recompiled every time the size of those arrays changed.
Still some scientific codes work with fixed arrays and need recompilation before any simulation.
Modern written codes (Since Fortran 90) used allocatable arrays.
Declarations and allocation happens in two steps instead of a single step with fixed arrays.
As allocation takes time, it is not good idea to allocate and deallocate very often.
Allocate once and use the space as much as possible.
Fortran 90 introduce ``ALLOCATABLE`` attributes and ``allocate`` and ``deallocate`` functions.
Fotran 95 added ``DIMENSION`` attribute as an alternative to specify the dimension of arrays. Otherwise, the array shape must be specified after array-variable name. For example:

~~~
REAL:: a(10)
REAL, DIMENSION(0:100, -50:50) :: b
~~~
{: language-fortran}



{% include links.md %}
