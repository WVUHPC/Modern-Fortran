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


{% include links.md %}
