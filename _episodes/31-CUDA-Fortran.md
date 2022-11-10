---
title: "CUDA Fortran"
teaching: 60
exercises: 0
questions:
- "How to program in CUDA Fortran?"
objectives:
- "Learn about the CUDA Fortran interface"
keypoints:
- "CUDA Fortran is a direct approach for controlling GPUs for numerical processing"
---

## CUDA Fortran

Graphic processing units or GPUs have evolved into programmable, highly
parallel computational units with very high memory bandwidth, and potential
for scientific applications.
GPU designs are optimized for the kind of computations found in graphics
rendering, but are general enough to be useful in many cases involving
data-parallelism, linear algebra, and other common use cases in scientific
computing.


CUDA Fortran is the Fortran interface to the CUDA parallel computing platform.  
If you are familiar with CUDA C, then you are already well on your way to using
CUDA Fortran is based on the CUDA C runtime API.  
There are a few differences in how CUDA concepts are expressed using
Fortran 90 constructs, but the programming model for both CUDA Fortran and
CUDA C is the same.

CUDA Fortran is essentially Fortran with a few extensions that allow one to
execute subroutines on the GPU by many threads in parallel.

## CUDA Programming Model Basics

Before we jump into CUDA Fortran code, those new to CUDA will benefit from a
basic description of the CUDA programming model and some of the terminology
used.

The CUDA programming model is a heterogeneous model in which both the CPU
and GPU are used. In CUDA, the host refers to the CPU and its memory,
while the device refers to the GPU and its memory.
Code running on the host manages the memory on both the host and device
and also launches kernels which are subroutines executed on the device.
These kernels are executed by many GPU threads in parallel.

Given the heterogeneous nature of the CUDA programming model, a typical
sequence of operations for a CUDA Fortran code is:

 * Declare and allocate a host and device memory.
 * Initialize host data.
 * Transfer data from the host to the device.
 * Execute one or more kernels.
 * Transfer results from the device to the host.

Keeping this sequence of operations in mind, let’s look at a CUDA Fortran
 example.

### A First CUDA Fortran Program

SAXPY stands for “Single-precision A*X Plus Y”, and is a good “hello world”
example for parallel computation. In this post I want to dissect a similar
version of SAXPY, explaining in detail what is done and why.
The complete SAXPY code is:

~~~
module mathOps
contains
  attributes(global) subroutine saxpy(x, y, a)
    implicit none
    real :: x(:), y(:)
    real, value :: a
    integer :: i, n
    n = size(x)
    i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
    if (i <= n) y(i) = y(i) + a*x(i)
  end subroutine saxpy
end module mathOps

program testSaxpy
  use mathOps
  use cudafor
  implicit none
  integer, parameter :: N = 1024000
  real :: x(N), y(N), a
  real, device :: x_d(N), y_d(N)
  type(dim3) :: grid, tBlock

  tBlock = dim3(256,1,1)
  grid = dim3(ceiling(real(N)/tBlock%x),1,1)

  call random_number(x)
  call random_number(y)
  call random_number(a)

  !x = 1.0; y = 2.0; a = 2.0
  x_d = x
  y_d = y
  call saxpy<<<grid, tBlock>>>(x_d, y_d, a)
  y = y_d

  print *, "Size of arrays: ", N
  print *, 'Grid             : ', grid
  print *, 'Threads per block: ', tBlock

  print *, "Constant a:", a
  print *, 'Average values ', sum(abs(x))/N, sum(abs(y))/N
end program testSaxpy
~~~
{: .language-fortran}

The module mathOps above contains the subroutine saxpy, which is the kernel
that is performed on the GPU, and the program testSaxpy is the host code.
Let’s begin our discussion of this program with the host code.

### Host Code

The program testSaxpy uses two modules:

~~~
  use mathOps
  use cudafor
~~~
{: .language-fortran}

The first is the user-defined module mathOps which contains the saxpy kernel,
and the second is the cudafor module which contains the CUDA Fortran
definitions. In the variable declaration section of the code, two sets of
arrays are defined:

~~~
  real :: x(N), y(N), a
  real, device :: x_d(N), y_d(N)
~~~
{: .language-fortran}

The real arrays ``x`` and ``y`` are the host arrays, declared in the typical
fashion, and the ``x_d`` and ``y_d`` arrays are device arrays declared with
the device variable attribute. As with CUDA C, the host and device in CUDA
Fortran have separate memory spaces, both of which are managed from host code.
But while CUDA C declares variables that reside in device memory in a
conventional manner and uses CUDA-specific routines to allocate data on
the GPU and transfer data between the CPU and GPU, CUDA Fortran uses the
device variable attribute to indicate which data reside in device memory
and uses conventional means to allocate and transfer data. The arrays
``x_d`` and ``y_d`` could have been declared with the allocatable in addition
to the device attribute and allocated with the F90 allocate statement.

One consequence of the strong typing in Fortran coupled with the presence of
the device attribute is that transfers between the host and device can be
performed simply by assignment statements. The host-to-device transfers
prior to the kernel launch are done by:

~~~
  x_d = x
  y_d = y
~~~
{: .language-fortran}

while the device-to-host transfer of the result is done by:

~~~
  y = y_d
~~~
{: .language-fortran}

The saxpy kernel is launched by the statement:

~~~
  call saxpy<<<grid,tBlock>>>(x_d, y_d, a)
~~~
{: .language-fortran}

The information between the triple chevrons is the execution configuration,
which dictates how many device threads execute the kernel in parallel.
In CUDA there is a hierarchy of threads in software which mimics how thread
processors are grouped on the GPU. In CUDA we speak of launching a kernel
with a grid of thread blocks.
The second argument in the execution configuration specifies the number of
threads in a thread block, and the first specifies the number of thread
blocks in the grid. Threads in a thread block can be arranged in a
multidimensional manner to accommodate the multidimensional nature of the
underlying problem, and likewise thread blocks can be arranged as such in a grid.
It is for this reason that the derived type dim3, which contains x, y, and z
components, is used for these two execution configuration parameters.
In a one-dimensional case such as this, these two execution configuration
parameters could also have been specified as integers. In this case we
launch the kernel with thread blocks containing 256 threads, and use the
ceiling function to determine the number of thread blocks required to
process all N elements of the arrays:

~~~
  tBlock = dim3(256,1,1)
  grid = dim3(ceiling(real(N)/tBlock%x),1,1)
~~~
{: .language-fortran}

For cases where the number of elements in the arrays is not evenly divisible by
the thread block size, the kernel code must check for out-of-bounds memory
accesses.

### Device Code

We now move on to the kernel code, which once again is:

~~~
  attributes(global) subroutine saxpy(x, y, a)
    implicit none
    real :: x(:), y(:)
    real, value :: a
    integer :: i, n
    n = size(x)
    i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
    if (i <= n) y(i) = y(i) + a*x(i)
  end subroutine saxpy
~~~
{: .language-fortran}

The saxpy kernel is differentiated from host subroutines via the
``attributes(global)`` qualifier.
In the variable declarations, the device attribute is not needed as it is
assumed that in device code all arguments reside on the device.
This is the case for the first two arguments of this kernel, which
correspond to the device arrays ``x_d`` and ``y_d`` in host code. However, the
last argument, the parameter a, was not transferred to the device in
host code. Because Fortran passes arguments by reference rather than
value, accessing a host variable from the device would cause an error
unless the value variable attribute is used in such cases, which instructs
the compiler to pass such arguments by value.

After the variable declarations, there are only three lines in our saxpy kernel.
As mentioned earlier, the kernel is executed by multiple threads in parallel.
If we want each thread to process an element of the resultant array, then we
need a means of distinguishing and identifying each thread. This is
accomplished through the predefined variables ``blockDim``, ``blockIdx``, and
``threadIdx``. These predefined variables are of type dim3, and are analogous to
the execution configuration parameters in host code. The predefined variable
blockDim in the kernel is equivalent to the thread block specified in host
code by the second execution configuration parameter.
The predefined variables threadIdx and blockIdx give the identity of the
thread within the thread block and the thread block within the grid,
respectively. The expression:

~~~
    i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
~~~
{: .language-fortran}

generates a global index that is used to access elements of the arrays.
Note that in CUDA Fortran, the components of threadIdx and blockIdx have
unit offset, so the first thread in a block has threadIdx%x=1 and the first
block in the grid has blockIdx%x=1. This differs from CUDA C which has zero
offset for these built-in variables, where the equivalent expression for an
index used to access C arrays would be:

~~~
    i = blockDim.x*blockIdx.x + threadIdx.x;
~~~
{: .language-fortran}

Before this index is used to access array elements, its value is checked
against the number of elements, obtained from the size() intrinsic, to ensure
there are no out-of-bounds memory accesses. This check is required for cases
where the number of elements in an array is not evenly divisible by the thread
block size, and as a result the number of threads launched by the kernel is
larger than the array size.

### Compiling and Running the Code

The CUDA Fortran compiler is a part of the NVIDIA compilers.

Any code in a file with a ``.cuf`` or ``.CUF`` extension is compiled with CUDA
Fortran automatically enabled. Otherwise, the compiler flag -Mcuda can be
used to compile CUDA Fortran code with other extensions.
If this code is in a file named ``saxpy.cuf``, compilation and execution of
the code is as simple as:

~~~
$> nvfortran saxpy.cuf
$> ./a.out
Size of arrays:       1024000
Grid             :          4000            1            1
Threads per block:           256            1            1
Constant a:   0.1387444    
Average values    0.4998179       0.5694433   
~~~
{: .language-bash}

The actual values could differ as we are initializing the arrays with random numbers
in the range [0,1], the average value for the second array will be raised 
proportional to the value of ``a``.

### Summary and Conclusions

Through a discussion of a CUDA Fortran implementation of SAXPY, this post
explained the basic components of programming CUDA Fortran.
Looking back at the full code, there are only a few extensions to Fortran
required to “port” a Fortran code to CUDA Fortran: variable and function
attributes used to distinguish device from host counterparts, the execution
configuration when launching a kernel, and the built-in device variables
used to identify and differentiate GPU threads that execute the kernel in
parallel.

One advantage of having a heterogeneous programming model is that porting
an existing code from Fortran to CUDA Fortran can be done incrementally,
one kernel at a time. Contrast this to other parallel programming approaches,
such as MPI, where porting is an all-or-nothing endeavor.


<!--

## Fortran CUDA


Lets start with a simple case (``example_01``)

~~~
module mytests
contains
   attributes(global) &
      subroutine test1(a)
      integer, device :: a(*)
      i = threadIdx%x
      a(i) = i
      return
   end subroutine test1
end module mytests

program t1
   use cudafor
   use mytests
   integer, parameter :: n = 100
   integer, allocatable, device :: iarr(:)
   integer h(n)
   istat = cudaSetDevice(0)
   allocate (iarr(n))
   h = 0; iarr = h
   call test1 <<< 1, n >>> (iarr)
   h = iarr
   print *, &
      "Errors: ", count(h .ne. (/(i, i=1, n)/))
   deallocate (iarr)
end program t1
~~~
{: .language-fortran}

~~~
module kernels_m
contains
  attributes(global) subroutine kernel(a, offset)
    implicit none
    real :: a(*)
    integer, value :: offset
    integer :: i
    real :: c, s, x

    i = offset + threadIdx%x + (blockIdx%x-1)*blockDim%x
    x = i; s = sin(x); c = cos(x)
    a(i) = a(i) + sqrt(s**2+c**2)
  end subroutine kernel
end module kernels_m


program testAsync
  use cudafor
  use kernels_m
  implicit none
  integer, parameter :: blockSize = 256, nStreams = 4
  integer, parameter :: n = 4*1024*blockSize*nStreams
  real, pinned, allocatable :: a(:)
  real, device :: a_d(n)
  integer(kind=cuda_stream_kind) :: stream(nStreams)
  type (cudaEvent) :: startEvent, stopEvent, dummyEvent
  real :: time
  integer :: i, istat, offset, streamSize = n/nStreams
  logical :: pinnedFlag
  type (cudaDeviceProp) :: prop

  istat = cudaGetDeviceProperties(prop, 0)
  write(*,"(' Device: ', a,/)") trim(prop%name)

  ! allocate pinned  host memory
  allocate(a(n), STAT=istat, PINNED=pinnedFlag)
  if (istat /= 0) then
     write(*,*) 'Allocation of a failed'
     stop
  else
     if (.not. pinnedFlag) write(*,*) 'Pinned allocation failed'
  end if

  ! create events and streams
  istat = cudaEventCreate(startEvent)
  istat = cudaEventCreate(stopEvent)  
  istat = cudaEventCreate(dummyEvent)  
  do i = 1, nStreams
     istat = cudaStreamCreate(stream(i))
  enddo

  ! baseline case - sequential transfer and execute
  a = 0
  istat = cudaEventRecord(startEvent,0)
  a_d = a
  call kernel<<<n/blockSize, blockSize>>>(a_d, 0)
  a = a_d
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  write(*,*) 'Time for sequential transfer and execute (ms): ', time
  write(*,*) '  max error: ', maxval(abs(a-1.0))

  ! asynchronous version 1: loop over {copy, kernel, copy}
  a = 0
  istat = cudaEventRecord(startEvent,0)
  do i = 1, nStreams
     offset = (i-1)*streamSize
     istat = cudaMemcpyAsync(a_d(offset+1),a(offset+1),streamSize,stream(i))
     call kernel<<<streamSize/blockSize, blockSize, &
                   0, stream(i)>>>(a_d,offset)
     istat = cudaMemcpyAsync(a(offset+1),a_d(offset+1),streamSize,stream(i))
  enddo
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  write(*,*) 'Time for asynchronous V1 transfer and execute (ms): ', time
  write(*,*) '  max error: ', maxval(abs(a-1.0))

  ! asynchronous version 2:
  ! loop over copy, loop over kernel, loop over copy
  a = 0
  istat = cudaEventRecord(startEvent,0)
  do i = 1, nStreams
     offset = (i-1)*streamSize
     istat = cudaMemcpyAsync(a_d(offset+1),a(offset+1),streamSize,stream(i))
  enddo
  do i = 1, nStreams
     offset = (i-1)*streamSize
     call kernel<<<streamSize/blockSize, blockSize, &
                   0, stream(i)>>>(a_d,offset)
  enddo
  do i = 1, nStreams
     offset = (i-1)*streamSize
     istat = cudaMemcpyAsync(a(offset+1),a_d(offset+1),streamSize,stream(i))
  enddo
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  write(*,*) 'Time for asynchronous V2 transfer and execute (ms): ', time
  write(*,*) '  max error: ', maxval(abs(a-1.0))

  ! cleanup
  istat = cudaEventDestroy(startEvent)
  istat = cudaEventDestroy(stopEvent)
  istat = cudaEventDestroy(dummyEvent)
  do i = 1, nStreams
     istat = cudaStreamDestroy(stream(i))
  enddo
  deallocate(a)

end program testAsync
~~~
{: .language-fortran}

## Transpose

~~~
module kernels_m
  implicit none

  integer, parameter :: TILE_DIM = 32
  integer, parameter :: BLOCK_ROWS = 8
  integer, parameter :: NUM_REPS = 100  
  integer, parameter :: nx = 1024, ny = 1024
  integer, parameter :: mem_size = nx*ny*4

contains

  ! simple copy kernel
  !
  ! used as reference case representing best
  ! effictive bandwidth

  attributes(global) subroutine copy(odata, idata)
    implicit none
    real, intent(out) :: odata(nx,ny)
    real, intent(in) :: idata(nx,ny)
    integer :: x, y, j

    x = (blockIdx%x-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%y-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       odata(x,y+j) = idata(x,y+j)
    end do
  end subroutine copy

  ! copy kernel using shared memory
  !
  ! also used as reference case, demonstrating effect of
  ! using shared memory

  attributes(global) subroutine copySharedMem(odata, idata)
    implicit none
    real, intent(out) :: odata(nx,ny)
    real, intent(in) :: idata(nx,ny)
    real, shared :: tile(TILE_DIM, TILE_DIM)
    integer :: x, y, j

    x = (blockIdx%x-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%y-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       tile(threadIdx%x, threadIdx%y+j) = idata(x,y+j)
    end do

    call syncthreads()

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       odata(x,y+j) = tile(threadIdx%x, threadIdx%y+j)          
    end do
  end subroutine copySharedMem

  ! naive transpose
  !
  ! simplest transpose - doesn't use shared memory
  ! reads from global memory are coalesced but not writes

  attributes(global) subroutine transposeNaive(odata, idata)
    implicit none
    real, intent(out) :: odata(ny,nx)
    real, intent(in) :: idata(nx,ny)
    integer :: x, y, j

    x = (blockIdx%x-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%y-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       odata(y+j,x) = idata(x,y+j)     
    end do
  end subroutine transposeNaive

  ! coalesced transpose
  !
  ! uses shared memory to achieve coalesing in both reads
  ! and writes
  !
  ! tile size causes shared memory bank conflicts

  attributes(global) subroutine transposeCoalesced(odata, idata)
    implicit none
    real, intent(out) :: odata(ny,nx)
    real, intent(in) :: idata(nx,ny)
    real, shared :: tile(TILE_DIM, TILE_DIM)
    integer :: x, y, j

    x = (blockIdx%x-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%y-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       tile(threadIdx%x, threadIdx%y+j) = idata(x,y+j)
    end do

    call syncthreads()

    x = (blockIdx%y-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%x-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       odata(x,y+j) = tile(threadIdx%y+j, threadIdx%x)          
    end do
  end subroutine transposeCoalesced

  ! no bank-conflict transpose
  !
  ! same as transposeCoalesced except the first tile dimension is padded
  ! to avoid shared memory bank conflicts

  attributes(global) subroutine transposeNoBankConflicts(odata, idata)
    implicit none
    real, intent(out) :: odata(ny,nx)
    real, intent(in) :: idata(nx,ny)
    real, shared :: tile(TILE_DIM+1, TILE_DIM)
    integer :: x, y, j

    x = (blockIdx%x-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%y-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       tile(threadIdx%x, threadIdx%y+j) = idata(x,y+j)
    end do

    call syncthreads()

    x = (blockIdx%y-1) * TILE_DIM + threadIdx%x
    y = (blockIdx%x-1) * TILE_DIM + threadIdx%y

    do j = 0, TILE_DIM-1, BLOCK_ROWS
       odata(x,y+j) = tile(threadIdx%y+j, threadIdx%x)          
    end do
  end subroutine transposeNoBankConflicts
end module

program transposes
  use cudafor
  use kernels_m
  implicit none
  type (dim3) :: dimGrid, dimBlock
  type (cudaEvent) :: startEvent, stopEvent
  real :: time

  real :: h_idata(nx,ny), h_cdata(nx,ny), h_tdata(ny,nx), gold(ny,nx)
  real, device :: d_idata(nx,ny), d_cdata(nx,ny), d_tdata(ny,nx)
  type(cudaDeviceProp) :: prop

  integer :: i, j, istat

  ! check parameters and calculate execution configuration
  if (mod(nx, TILE_DIM) /= 0 .or. mod(ny, TILE_DIM) /= 0) then
     write(*,*) 'nx and ny must be a multiple of TILE_DIM'
     stop
  end if

  if (mod(TILE_DIM, BLOCK_ROWS) /= 0) then
     write(*,*) 'TILE_DIM must be a multiple of BLOCK_ROWS'
     stop
  end if

  dimGrid = dim3(nx/TILE_DIM, ny/TILE_DIM, 1)
  dimBlock = dim3(TILE_DIM, BLOCK_ROWS, 1)

  ! write parameters
  istat = cudaGetDeviceProperties(prop, 0)
  write(*,*)
  write(*,'(''Device: '', a)') trim(prop%name)
  write(*,'(''Matrix size:'', i5, i5, '',  Block size:'', i3, i3, '',  Tile size:'', i3, i3)') &
       nx, ny, TILE_DIM, BLOCK_ROWS, TILE_DIM, TILE_DIM

  write(*,'(''dimGrid:'', i4,i4,i4, '',   dimBlock:'', i4,i4,i4)') &
       dimGrid%x, dimGrid%y, dimGrid%z, dimBlock%x, dimBlock%y, dimBlock%z

  ! host
  do j = 1, ny
     do i = 1, nx
        h_idata(i,j) = i+(j-1)*nx
     enddo
  enddo
  gold = transpose(h_idata)

  ! device
  d_idata = h_idata
  d_tdata = -1.0
  d_cdata = -1.0

  ! events for timing
  istat = cudaEventCreate(startEvent)
  istat = cudaEventCreate(stopEvent)

  ! ------------
  ! time kernels
  ! ------------

  write(*,'(/,a25,a25, a25)') 'Routine', 'Bandwidth (GB/s)'

  ! ----
  ! copy
  ! ----

  write(*,'(a25)', advance='NO') 'copy'
  ! warmup
  call copy<<<dimGrid, dimBlock>>>(d_cdata, d_idata)
  istat = cudaEventRecord(startEvent, 0)
  do i=1, NUM_REPS
     call copy<<<dimGrid, dimBlock>>>(d_cdata, d_idata)
  end do
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  h_cdata = d_cdata
  call postprocess(h_idata, h_cdata, time)

  ! -------------
  ! copySharedMem
  ! -------------

  write(*,'(a25)', advance='NO') 'shared memory copy'
  d_cdata = -1.0
  ! warmup
  call copySharedMem<<<dimGrid, dimBlock>>>(d_cdata, d_idata)
  istat = cudaEventRecord(startEvent, 0)
  do i=1, NUM_REPS
     call copySharedMem<<<dimGrid, dimBlock>>>(d_cdata, d_idata)
  end do
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  h_cdata = d_cdata
  call postprocess(h_idata, h_cdata, time)

  ! --------------
  ! transposeNaive
  ! --------------

  write(*,'(a25)', advance='NO') 'naive transpose'
  d_tdata = -1.0
  ! warmup
  call transposeNaive<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  istat = cudaEventRecord(startEvent, 0)
  do i=1, NUM_REPS
     call transposeNaive<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  end do
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  h_tdata = d_tdata
  call postprocess(gold, h_tdata, time)

  ! ------------------
  ! transposeCoalesced
  ! ------------------

  write(*,'(a25)', advance='NO') 'coalesced transpose'
  d_tdata = -1.0
  ! warmup
  call transposeCoalesced<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  istat = cudaEventRecord(startEvent, 0)
  do i=1, NUM_REPS
     call transposeCoalesced<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  end do
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  h_tdata = d_tdata
  call postprocess(gold, h_tdata, time)

  ! ------------------------
  ! transposeNoBankConflicts
  ! ------------------------

  write(*,'(a25)', advance='NO') 'conflict-free transpose'
  d_tdata = -1.0
  ! warmup
  call transposeNoBankConflicts<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  istat = cudaEventRecord(startEvent, 0)
  do i=1, NUM_REPS
     call transposeNoBankConflicts<<<dimGrid, dimBlock>>>(d_tdata, d_idata)
  end do
  istat = cudaEventRecord(stopEvent, 0)
  istat = cudaEventSynchronize(stopEvent)
  istat = cudaEventElapsedTime(time, startEvent, stopEvent)
  h_tdata = d_tdata
  call postprocess(gold, h_tdata, time)

  ! cleanup
  istat = cudaEventDestroy(startEvent)
  istat = cudaEventDestroy(stopEvent)  

contains
  subroutine postprocess(ref, res, t)
    real, intent(in) :: ref(:,:), res(:,:), t          ! host reference, result and time
    if (all(res == ref)) then
       write(*,'(f20.2)') 2.*1000*mem_size/(10**9 * t/NUM_REPS)
    else
       write(*,'(a20)') '*** Failed ***'
    end if
  end subroutine postprocess
end program transposes
~~~
{: .language-fortran}

-->

{% include links.md %}
