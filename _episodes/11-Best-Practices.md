---
title: "Best Practices in Modern Fortran"
teaching: 60
exercises: 0
questions:
- "How to move out of Fortran 77 into Modern Fortran styles?"
objectives:
- "Learn the difference between deprecated styles and the modern ways of Fortran coding."
keypoints:
- "Avoid old Fortran 77 style even if correct the code looks better if you use Modern Fortran"
---

# Best Practices in Modern Fortran

Fortran is a language with a long history.
Fortran is one of the oldest Programming Languages still in active use.
That old history means that the language has evolved in expression and scope over more than 60 years.

The way you programmed Fortran back in the 60s, 70s, and 80s is completely inadequate for modern computers.
Consider for example the length of the lines.
Back in the 70s and 80s, people programmed computers using punched cards
and later dumb terminals connected to mainframes.
Those terminals have a limited number of characters in one line.
It is from that time that a limit of 72 characters on one line and characters beyond column 72 were ignored.
The first five columns must be blank or contain a numeric label.
Continuation lines were identified by a nonblank, nonzero in column 6.
All these limitations were present in what is called the Standard Fixed Format of Fortran 77.

Over time, these limitations and many others in the language became obsolete but were still in use for decades even after Fortran 90 was the new Standard.
Back in the 90s, there were no free Fortran 90 compilers, people continue to use Fortran 77 with restrictions relaxed and some characteristics of Fortran 90 were integrated little by little into the compilers.

The fact is that there are many codes written in Fortran 77.
If you search for Fortran tutorials and documentation, there are many pages in academics that still teach Fortran 77.
Fortran 90 was gaining space and later Fortran 95.
Today almost main Fortran compilers support Fortran 2003 entirely and Fortran 2008 to a fairly complete degree. New features in Fortran 2018 are still not implemented in the latest version of major compilers.

In previous sections, we have discussed the basic elements so you can read and write code in Fortran.
We learn about variables assignments subroutines and modules and we explored those elements using the modern ways of writing code in Fortran.
In this section we will have to learn a bit about the "old ways" in such a way that when you encounter those pieces of code, you can recognize them knowing which are the alternatives and best practices used today.
This is in this way an intermediate class in Modern Fortran.
I assume you know the basics and are able to write useful code in Fortran.
The idea is to recognize old Fortran 77 and Fortran 90 styles and be able to translate those into modern Fortran 2003 and 2008 Standards.
By doing that you are moving the code into more flexible, readable, extensible, and potentially with more performance.

## The old Fortran 77

Fortran 77 was a powerful language back in its time.
It was so powerful that stay as the standard *de facto* well in the 90s.
Many elements of the language still in use today were added to Fortran 77 to address the limitations of FORTRAN 66.
For example the ``IF`` and ``END IF`` statements including ``ELSE`` and ``ELSE IF`` were the first steps into structured programming.
These statements were an important move to move ``GOTO`` into oblivion.
``GOTO``was one of the reasons why codes were so difficult to follow and almost unpredictable when were the source of a bug.
Another powerful element of the language were intrinsic functions such as ``SQRT`` that accept complex or double precision numbers as arguments.  
All these niceties today are taken for granted but back then were big improvements to the language.
However, the language has important flaws.
One important is the lack of dynamic memory allocation.
The code still promotes a code that is cluttered, with missing blanks, and in general hard to read.

Now the question is if a code works in Fortran 77 why change it?
Investing some time now in rewriting a code in a new language could return in big gains later on.
A new structure could emerge that uses better memory (with dynamic allocation) to structure the code in ways that make it easier to read and extend.
There are good reasons and rewards for better and cleaner code.

## Free Format

Back to the first versions of Fortran, the first 5 columns of characters were dedicated as label fields.
A C in column 1 means that the line was treated as a comment and any character in column 6 means that the line was a continuation of the previous statement.

Today those restrictions are no longer needed.
Statements can start in the first column.
An exclamation mark starts a comment and comments could be inserted any place in the line, except literal strings.
Blanks help the readability of the code.
Variables cannot have spaces, it is a better practice to use underscore for multi-word variables.
Use ``&`` as the last character for continuing on the next line.
Declaring multiple statements on the same line is possible with a semicolon (``;``) but use it only for very short statements.

Example (``example_01.f90``):

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

Use blank characters, blank lines and comments to improve the readability of your code.
A cluttered code is hard to read and you are not doing any good trying to fit most of your code in one screen page.

Instead of this (``example_02_bad.f90``):

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

An alternative is like this (``example_02_bad.f90``):

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

In old FORTRAN 77, do loops have a numerical identifier to jump to a ``continue`` statement to cycle the loop, that is completely obsolete and must be avoided in modern coding (``example_03_bad.f90``):

~~~
      PROGRAM square
      DO 100 I=1,100
      X=I
      X2=X*X
      IF(X2.LT.100) print *, 'X=', I, ' X^2 will have less than 3 digits
100   CONTINUE
      END
~~~
{: .language-fortran}

This old style of coding wastes 6 columns of space, uses a labeled statement for cycling and is written with full capitalization.

An alternative is something like this (``example_03_good.f90``):

~~~
program square

   implicit none

   real :: x, x2
   integer :: i

   do i = 1, 100
      x = i
      x2 = x*x
      if (x2 < 100) &
         print *, 'X=', I, ' X^2 will have less than 3 digits
   end do

end program
~~~
{: .language-fortran}

This code uses indentations and has an ``end do`` that is clearer for the user when the loops grow or became nested. We will discuss the ``implicit`` down below

## Variable Attributes

Fortran 95 introduce variable attributes and they were extended in Fortran 2003 and 2008. The most common attributes are
``parameter``, ``dimension``, ``allocatable``, ``intent``, ``pointer``, ``target``, ``optional``, ``private``, ``public``, ``value`` and ``bind``

From those, we will demonstrate the first 3.
``parameter`` is important for declaring constants that otherwise will require going beyond the language and using the preprocessor.
``dimension``is used to define the length of arrays or the dimension using colons and commas to declare it.
``allocatable`` is used for the dynamic allocation of arrays an important feature introduced in Fortran 90 and expanded in Fortran 95 and beyond.
This is an example using these 3 attributes that should be frequently used (``example_04.f90``).

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

The file ``example_05.f90`` and ``example_06.f90`` show the effect of ``exit`` and ``cycle`` respectively.

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
Constructs such as ``do``, and ``if`` as ``case`` can be named and you can use the name to leave outer loops from inside an inner loop.

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
Some compilers offer the third kind with 16-byte reals.
The kind numbers are usually 4, 8, and 16, but this is just a tradition of several languages and not mandatory by the language.
The kind values could be 1, 2 and 4.

There is an intrinsic module called ``iso_fortran_env`` that provides the kind values for logical, character, integer, and real data types.

Consider this example to get the values used (``example_07.f90``)

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

Consider this example illustrative of the multiple ways of declaring REAL variables (``example_08.f90``):

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
Other variations use the kind assuming that the numbers 4, 8, and 16 represent the number of bytes used by each data type, this is not standard and Salford f95 compiler used kinds 1,2, and 3 to stand for 2- 4- and 8-byte.

Notice that storage size is not the same as precision. Those terms are related and you expect that more bytes will end up giving more precision, but REALS have several internal components such as the size of mantissa and exponent.
The 24 bits (including the hidden bit) of mantissa in a 32-bit floating-point number represent about 7 significant decimal digits.

Even though, this is not the same across all the real space.
We are using the same number of bits to represent all normalized numbers, the smaller the exponent, the greater the density of truncated numbers.
For example, there are approximately 8 million single-precision numbers between 1.0 and 2.0, while there are only about 8 thousand numbers between 1023.0 and 1024.0.  

Beyond the standard representation, you can also change the *storage size* during compilation.
Below, the same code was compiled using arguments that change the storage size of different variables.

~~~
$> gfortran example_08.f90
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
$> gfortran -fdefault-real-16 example_08.f90
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
$> gfortran -fdefault-real-16 -fdefault-double-8 example_08.f90
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

This example shows the new kind parameters (``example_09.f90``).

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
$> gfortran example_09.f90
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
$> gfortran -fdefault-real-16 -fdefault-double-8 example_09.f90
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
$> gfortran -freal-4-real-10 -freal-8-real-10 example_09.f90
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
$> gfortran -freal-4-real-16 -freal-8-real-16 example_09.f90
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
Scalars and static arrays live in the *stack* but the size of that space is very limited and some sysadmins and queue systems limit its value even more.
Most other variables including allocatable arrays live on the heap.

Before allocatable arrays were part of Fortran 90, Arrays were created with a fixed size.
Programs used arrays with sizes that overestimated the actual needs for storage or require being recompiled every time the size of those arrays changed.
Still, some scientific codes work with fixed arrays and need recompilation before any simulation.
Modern written codes (Since Fortran 90) used allocatable arrays.
Declarations and allocation happen in two steps instead of a single step with fixed arrays.
As allocation takes time, it is not a good idea to allocate and deallocate very often.
Allocate once and use the space as much as possible.
Fortran 90 introduces ``ALLOCATABLE`` attributes and ``allocate`` and ``deallocate`` functions.
Fortran 95 added ``DIMENSION`` attribute as an alternative to specifying the dimension of arrays. Otherwise, the array shape must be specified after the array-variable name. For example (``example_10.f90``):

~~~
program alloc_array

   implicit none

   integer, parameter :: n = 10, m = 20
   integer :: i, j, ierror

   real :: a(10)
   real, dimension(10:100, -50:50) :: b
   real, allocatable :: c(:, :)

   real, dimension(:), allocatable :: x_1d
   real, dimension(:, :), allocatable :: x_2d

   allocate (c(-9:10, -4:5))

   allocate (x_1d(n), x_2d(n, m), stat=ierror)
   if (ierror /= 0) stop 'error in allocation'

   do i = 1, n
      x_1d(i) = i
      do j = 1, m
         c(j - 10, i - 5) = j-10 + 0.01*(i-5) ! contiguous operation
         x_2d(i, j) = i + 0.01*j              ! non-contiguous operation
      end do
   end do

   print *,''
   print '(A, 10(F6.2))', 'x_1d:', x_1d(:)

   print *,''
   do j = -9, 10
      print '(A, 10(F6.2))', 'c   :', c(j, :)
   end do

   print *,''
   do i = 1, n
      print '(A, 20(F6.2))', 'x_2d:', x_2d(i, :)
   end do

   deallocate (c, x_1d, x_2d)

end program
~~~
{: language-fortran}

## Array Allocations: heap vs stack

Static arrays could be allocated on the stack instead of using the heap.
The stack has a limited size and under some circumstances, static arrays could exhaust the stack space allowed for a process.
Consider this example (``example_12.f90``):

~~~
program arraymem

   use iso_fortran_env

   implicit none

   integer, parameter :: n = 2*1024*1024 - 4096

   print *, 'Numeric Storage (bits): ', numeric_storage_size
   print *, 'Creating array N ', n

   call meanArray(n)

contains

   subroutine meanArray(n)
      integer, intent(in) :: n
      integer :: dumb
      integer, dimension(n) :: a
      a = 1
      !print *, sum(a)
      read *, dumb
   end subroutine meanArray

end program arraymem
~~~
{: language-fortran}

You can check the limit for stack memory on the machine:

~~~
$> ulimit -S  -s
8192
$> ulimit -H  -s
unlimited
~~~
{: .language-bash}

We have a soft limit of 8MB for stack and the user can raise its value to an unlimited value.

As the limit is 8MB we will create an integer array that takes a bit under 8MB.
The array will be of integers and each integer takes by default 4 Bytes. This is not a safe value but will work for the purpose of demonstrating the effect:

~~~
n = 2*1024*1024 - 4096
~~~
{: .source}

Now we will compile the code above with ``gfortran``, forcing the static arrays to be on stack.
Some compilers move large arrays to heap automatically so we are bypassing this protection.

~~~
$> gfortran -fstack-arrays example_12.f90
$> ./a.out
~~~
{: .language-bash}

Sometimes you will get an output like this:

~~~
 Numeric Storage (bits):           32
 Creating array N      2093056
Segmentation fault (core dumped)
~~~
{: .source}

We are so close to filling the stack that small variations in loading libraries could cross the limit.
Try a few times until the code stops when asking for input from the keyboard.

Once the code is waiting for any input, execute this command on a separate terminal:

~~~
$> pmap -x `ps ax | grep a.ou[t] | awk '{print $1}' | head -1`
~~~
{: .language-bash}

The command ``pmap`` will print a map of the memory and the stack is marked there.

~~~
Address           Kbytes     RSS   Dirty Mode  Mapping
0000000000400000       4       4       0 r---- a.out
0000000000401000       4       4       0 r-x-- a.out
0000000000402000       4       4       0 r---- a.out
0000000000403000       4       4       4 r---- a.out
0000000000404000       4       4       4 rw--- a.out
0000000001a3e000     132      16      16 rw---   [ anon ]
00007f8fb9948000    1808     284       0 r-x-- libc-2.17.so
00007f8fb9b0c000    2044       0       0 ----- libc-2.17.so
00007f8fb9d0b000      16      16      16 r---- libc-2.17.so
00007f8fb9d0f000       8       8       8 rw--- libc-2.17.so
00007f8fb9d11000      20      12      12 rw---   [ anon ]
00007f8fb9d16000     276      16       0 r-x-- libquadmath.so.0.0.0
00007f8fb9d5b000    2048       0       0 ----- libquadmath.so.0.0.0
00007f8fb9f5b000       4       4       4 r---- libquadmath.so.0.0.0
00007f8fb9f5c000       4       4       4 rw--- libquadmath.so.0.0.0
00007f8fb9f5d000      92      24       0 r-x-- libgcc_s.so.1
00007f8fb9f74000    2044       0       0 ----- libgcc_s.so.1
00007f8fba173000       4       4       4 r---- libgcc_s.so.1
00007f8fba174000       4       4       4 rw--- libgcc_s.so.1
00007f8fba175000    1028      64       0 r-x-- libm-2.17.so
00007f8fba276000    2044       0       0 ----- libm-2.17.so
00007f8fba475000       4       4       4 r---- libm-2.17.so
00007f8fba476000       4       4       4 rw--- libm-2.17.so
00007f8fba477000    2704     220       0 r-x-- libgfortran.so.5.0.0
00007f8fba71b000    2048       0       0 ----- libgfortran.so.5.0.0
00007f8fba91b000       4       4       4 r---- libgfortran.so.5.0.0
00007f8fba91c000       8       8       8 rw--- libgfortran.so.5.0.0
00007f8fba91e000     136     108       0 r-x-- ld-2.17.so
00007f8fbab29000      16      16      16 rw---   [ anon ]
00007f8fbab3d000       8       8       8 rw---   [ anon ]
00007f8fbab3f000       4       4       4 r---- ld-2.17.so
00007f8fbab40000       4       4       4 rw--- ld-2.17.so
00007f8fbab41000       4       4       4 rw---   [ anon ]
00007ffd65b94000    8192    8192    8192 rw---   [ stack ]
00007ffd663d3000       8       4       0 r-x--   [ anon ]
ffffffffff600000       4       0       0 r-x--   [ anon ]
---------------- ------- ------- -------
total kB           24744    9056    8324
~~~
{: .output}

Notice that in this case, we have fully consumed the stack.
Compilers could take decisions of moving static arrays to the heap, but even with these provisions, is very easy that multiple arrays combined could cross the limit.

Consider arrays to be always ``allocatable``, so they are allocated on the heap always.

## Derived Types and Structures

Beyond the variables of a simple type (real, integer, character, logical, complex) new data types can be created by grouping them into a derived type.
Derived types can also include other derived types
Arrays can also be included in both static and allocatable.
Structures can be made allocatable.

Consider this example that shows the use of *derived types* and its instances called *structures* (``example_13.f90``).

~~~
program use_structs

   implicit none

   integer :: i

   ! Electron Configuration
   ! Example: [Xe] 6s1 4f14 5d10
   type electron_configuration

      character(len=3) :: base_configuration
      integer, allocatable, dimension(:) :: levels
      integer, allocatable, dimension(:) :: orbitals
      integer, allocatable, dimension(:) :: n_electrons

   end type electron_configuration

   ! Information about one atom
   type atom

      integer :: Z
      character(len=3) symbol
      character(len=20) name
      integer, allocatable, dimension(:) :: oxidation_states
      real :: electron_affinity, ionization_energy
      type(electron_configuration) :: elec_conf ! structure

   end type atom

   ! Structures (Variables) of the the derived type my_struct
   type(atom) :: gold
   type(atom), dimension(15) :: lanthanide

   gold%Z = 79
   gold%symbol = 'Au'
   gold%name = 'Gold'
   allocate (gold%oxidation_states(2))
   gold%oxidation_states = [3, 1]
   gold%electron_affinity = 2.309
   gold%ionization_energy = 9.226
   allocate (gold%elec_conf%levels(3))
   allocate (gold%elec_conf%orbitals(3))
   allocate (gold%elec_conf%n_electrons(3))
   gold%elec_conf%base_configuration = 'Xe'
   gold%elec_conf%levels = [6, 4, 5]
   gold%elec_conf%orbitals = [1, 4, 3]
   gold%elec_conf%n_electrons = [1, 14, 10]

   print *, 'Atom name', gold%name
   print *, 'Configuration:', gold%elec_conf%base_configuration
   do i = 1, size(gold%elec_conf%levels)
      print '(3(I4))', &
         gold%elec_conf%levels(i), &
         gold%elec_conf%orbitals(i), &
         gold%elec_conf%n_electrons(i)
   end do
end program
~~~
{: .language-fortran}

## Functions and Subroutines

A simple program will have only variable declarations and statements.
The next level of organization is to encapsulate variable declarations and statements as an entity (function or subroutine) that can be reused inside the main program, other programs or other
function or subroutine.

Another advantage of moving code inside functions or subroutines is that you hide variables inside the scope of the function, allowing the reuse of the names in other routines without collisions.

Fortran makes the distinction between ``function`` and ``subroutine``.
This is different from other programming languages such as C/C++, Python, Java.
In purely functional programming languages (e.g. Haskell) only functions are allowed.
Subroutines have arguments that make no distinction between input and output and input variables can be modified as side-effects.

Fortran Functions are simpler compared to subroutines.
A function return a single value that is predefined.
Functions can be invoked from within expressions, like a write statement, inside an if declaration and other statements.

A subroutine does not return a value, but can return many values via its changing one or more arguments.
Subroutines can only be used using the keyword ``call``.

This is one example of a code with one function and one subroutine.
(``example_14.f90``)

~~~
subroutine sub_one(ix, oy, ioz)
   real, intent(in) :: ix ! input
   real, intent(out)  :: oy ! output
   real, intent(inout) :: ioz ! input and output

   oy = sqrt(real(ix)) + ioz
   ioz = max(ix, oy)

end subroutine

function func(i) result(j)
   integer, intent(in) :: i ! input
   integer              :: j ! output

   j = sqrt(real(i)) + log(real(i))
end function

program main

   implicit none

   integer :: i
   integer :: func
   real :: ix, oy, ioz

   ix = 10.0
   ioz = 20.0

   i = huge(1)
   print *, "i=", i, char(10), " sqrt(i) + log(i) =", func(i)

   print *, 'Before:', ix, oy, ioz
   call sub_one(ix, oy, ioz)
   print *, 'After :', ix, oy, ioz

end program
~~~
{: .language-fortran}

It is a good practice to declare the intent of variable with ``ìn``, ``out`` or ``inout``.
Using them could help during debugging in case variables became modified unintentionally.

## Modules

Modules is the next natural level of abstraction.
Modules can contain various kinds of things like

  * Variables: Scalars, arrays, parameters, structures.
  * Derived Types
  * Subprograms like *Functions* and *Subroutines*
  * Objects (Instances of other modules)
  * Submodules (Fortran 2008)

This small example uses a module to contain constants.
(``example_15.f90``)

~~~
module mod_constants

   use iso_fortran_env

   implicit none

   real, parameter :: pi = 3.1415926536
   real, parameter :: e = 2.7182818285

   real(kind=real64), parameter :: elementary_charge = 1.602176634D-19
   real(kind=real64), parameter :: G_grav = 6.67430D-11     ! Gravitational constant
   real(kind=real64), parameter :: h_plank = 6.62607015D-34  ! Plank constant
   real(kind=real64), parameter :: c_light = 299792458       ! Light Speed
   real(kind=real64), parameter :: vacuum_electric_permittivity = 8.8541878128D-12
   real(kind=real64), parameter :: vacuum_magnetic_permeability = 1.25663706212D-6
   real(kind=real64), parameter :: electron_mass = 9.1093837015D-31
   real(kind=real64), parameter :: fine_structure = 7.2973525693D-3
   real(kind=real64), parameter :: Josephson = 483597.8484
   real(kind=real64), parameter :: Rydberg = 10973731.568160
   real(kind=real64), parameter :: von_Klitzing = 25812.80745

contains

   subroutine show_consts()
      print *, "G = ", G_grav
      print *, "h = ", h_plank
      print *, "c = ", c_light
   end subroutine show_consts

end module mod_constants

program physical_constants

   use mod_constants

   implicit none

   print *, sqrt(2.0_real64)
   print *, sqrt(2.0_real128)
   print *, 'Inverse of Fine Structure constant = ', 1.0_real64 / fine_structure

   call show_consts()

end program
~~~
{: .language-}

The content of a module is accessible after the statement ``use <module name>``
By default all variables, subroutines and functions inside a module are visible, but restrictions can be made using the ``private`` statement before the routine and it can only be used by other subroutines on the same module but not by routines that ``use`` the module. The variable attribute public can also be used to make exceptions after the private statement

Example of ``public`` and ``private`` variables and functions.
It will be very similar for subroutines and data types.
 (``example_16.f90``):

~~~
module mod_public_private

   implicit none

   public

   real, parameter :: pi = 3.141592653, &
                      c = 299792458, &
                      e = 2.7182818285

   real, private :: rad_2_deg = 180.0/pi
   real, private :: deg_2_rad = pi/180.0

   private :: sin_deg, cos_deg

   public :: tan_deg

contains

   function sin_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = sin(x*deg_2_rad)
   end function

   function cos_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = cos(x*deg_2_rad)
   end function

   function tan_deg(x) result(y)
      real, intent(in) :: x ! input
      real             :: y ! output
      y = sin_deg(x)/cos_deg(x)
   end function

end module mod_public_private

program priv_pub_module

   use mod_public_private

   implicit none

   real :: r = 2.0

   print *, 'Area = ', pi*r**2

   ! This print will not work
   ! The variables rad_2_deg and deg_2_rad are private
   !print *, rad_2_deg, deg_2_rad

   print *, 'Tan(45) ', tan_deg(45.0)

   ! These lines will not work as functions are private
   !print *, 'Sin(45) ', sin_rad(45.0)
   !print *, 'Cos(45) ', cos_rad(45.0)

end program
~~~
{: .language-fortran}

## Modules: attribute ``protected`` and renaming of variables

A public variable is visible by routines that use the module.
Private variables will not.
There are cases were the value needs to be visible but not changed outside.
That is the purpose of ``private`` attribute.

Another option is for variables be renamed when the module is loaded allowing codes outside to use the name of the variable for other purposes.

This example combine both features (``example_17.f90``):

~~~
module module_privs

   implicit none

   integer, parameter, private :: sp = selected_real_kind(6, 37)
   integer, parameter, private :: dp = selected_real_kind(15, 307)
   integer, parameter, private :: qp = selected_real_kind(33, 4931)

   real(kind=sp), protected :: pi_sigl = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_sp
   real(kind=dp), protected :: pi_dble = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_dp
   real(kind=qp), protected :: pi_quad = &
   3.141592653589793238462643383279502884197169399375105820974944592307&
   &8164062862089986280348253421170679821480865132823066470938446_qp
   real :: x = 50.25
   real, protected :: x_prot = 512.125
   real :: y = 3.0

contains

   subroutine show_pi_3()
      print *, 'PI with  6 digits', kind(pi_sigl), pi_sigl
      print *, 'PI with 15 digits', kind(pi_dble), pi_dble
      print *, 'PI with 33 digits', kind(pi_quad), pi_quad
   end subroutine show_pi_3

end module module_privs

program main

   use module_privs, my_y => y

   implicit none

   call show_pi_3()

   print *, 'x from inside the module : ', x
   x = 25.50
   print *, 'x changed outside module : ', x

   print *, 'x_prot:', x_prot
   ! This variable is protected and cannot be changed
   ! Uncommenting the line below will not compile
   !x_prot = 125.512
   print *, 'x_prot:', x_prot

   ! The variable 'y' is not visible as it was renamed
   !print *, y
   print *, 'my_y:', my_y

end program main
~~~
{: .language-fortran}

## Optional arguments

In Modern Fortran (since Fortran 90), optional arguments can be part of a subroutine.
They must be declared as optional in the calling function (either through a module or an explicit interface).
In several implementations of old Fortran 77 it was possible to simply leave out the last argument, if it was a scalar number
This behavior, was not part of the Fortran standard but used by some programmers.
Now the ``optional`` attribute must be declared explicitly.

This example explores several options for optional variables.
(``example_19.f90``):

~~~
module my_module

   implicit none

   real :: d_default = -1.125

contains

   subroutine calc(a, b, c, d, e)
      real :: a, b, c, e
      real, optional :: d
      real :: dd
      if (present(d)) then
         dd = d
      else
         print *, 'd argument not present, using default'
         dd = d_default
      end if

      print '(5(F7.3))', a,b,c,dd,e

   end subroutine

end module

program main

   use my_module

   implicit none

   call calc(1., 2., 3., 4., 5.)
   call calc(1., 2., 3., e=5.)
   call calc(a=1., b=2., c=3., d=4., e=5.)
   call calc(b=2., d=4., a=1., c=3., e=5.)
   call calc(1., 2., 3., d=4., e=5.)
   call calc(1., 2., d=4., c=3., e=5.)

end program
~~~
{: .language-fortran}

## Array syntax

One of the strengths of Fortran is scientific computing and High-Performance Computing.
Central to HPC programming are arrays and Fortran is particularly good in expressing operations with arrays.

Many operations that require loops in C are done in one operation in Fortran, those are implicit loops hidden in the language expression.

Conditions for operations with arrays are:

  * The left and right sides of an assignment must be conformable.
     That means that the same number of elements are involved and they match in dimension

  * Operations with strides are valid if they are also conformable.

Example (``example_20.f90``):



{% include links.md %}
