---
layout: lesson
root: .  # Is the only page that doesn't follow the pattern /:path/index.html
permalink: index.html  # Is the only page that doesn't follow the pattern /:path/index.html
---
Fortran is one of the three most commonly used compiled programming languages for scientific computing.
The other two are C and C++.
Fortran has a long tradition of being the first programming language widely adopted by scientists for numerical programming.
With this long tradition also comes a legacy in the structure of programming and style that have evolved over the years.
Today is possible to see codes written in the now-old Fortran 77 style.
Plenty of important codes were developed during the 70s and 80s.
Even during the 90s when Fortran 90 became the new standard for the Fortran language, the lack of affordable implementation of the new specification made many scientists still rely on the old Fortran 77.
By the early 2000's all that has changed; most Fortran 77 codes were translated to Fortran 90 or Fortran 95 standard, compilers are still capable of compiling codes using the old style but its usage has been discouraged since.
After Fortran 90 came Fortran 95 which fixed some problematic issues with Fortran 90 standard and established itself as the base for what we call today Modern Fortran.
Today Fortran 2003 and 2008 are implemented by several vendors some of them offering compilers including open-source versions such as GCC.
This is a tutorial on Modern Fortran, understanding it as code produced for Fortran 95, 2003, 2008, or even 2018, the latest major revision of the language.

Modern Scientific Programming is also a synonym for Parallel Programming.
We have dedicated 3 days of this workshop to discuss several Parallel Programming paradigms and their usage on Fortran.
The four models of scientific parallel programming are OpenMP, OpenACC, CUDA Fortran and MPI. 

<!-- this is an html comment -->

{% comment %} This is a comment in Liquid {% endcomment %}

> ## Prerequisites
> Programming a computer is the process of translating ideas into code that can be objectively interpreted by a computer and perform the operations that lead to a result.
> 
> To follow this tutorial, it is always helpful to understand how most programming languages operated. 
> Most programming languages use the concept of variables and operations executed on those variables. Operations on variables are called functions or methods. 
> The flow of operations is organized in sections that in some languages are also called functions, structures, and classes.
>
> The code follows a path, starting from an initial line of execution it continues line after line.
> Sometimes they follow a different path according to the result of an operation. On other occasions, the path repeats itself over and over a certain number of times or until another condition is met. All this we will learn here in the context of a particular programming language called Fortran.
> 
> For parallel programming multiple instructions happen concurrently, potentially using the also multiple CPU cores on a machine a numerical accelerator such as a GPU, or even multiple machines. Some of our examples, in particular, those for OpenACC and CUDA Fortran can only be executed if you have a machine with an NVIDIA GPU. For distributed parallel programming with MPI, you can still execute the codes using a single machine even if the scope of MPI programming is to use the computational power of multiple independent machines.
>
>
{: .prereq}

**Testing LaTeX support in the (Lesson/Workshop) The Cauchy-Schwarz Inequality**

$$\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)$$

<p id="This_Is_What_I_Want"> $$ (a-b)^2 $$</p>
<p id="First_Input"> <input id="Value_A"></p>
<p id="Second_Input"> <input id="Value_B"></p>
<p id="Output"></p>
<p id="Activate"><button onclick="RUN()">Test This out</button></p>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML,http://myserver.com/MathJax/config/local/local.js">
        function RUN() {
            var a = document.getElementById("Value_A").value
            var b = document.getElementById("Value_B").value
            document.getElementById("Output").innerHTML = "$$ (" + a + "-" + b + ")^2 $$";
            MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
        }
</script>

{% include links.md %}
