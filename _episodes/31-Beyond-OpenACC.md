---
title: "Fortran CUDA"
teaching: 60
exercises: 0
questions:
- "Key question (FIXME)"
objectives:
- "First learning objective. (FIXME)"
keypoints:
- "First key point. Brief Answer to questions. (FIXME)"
---

## Fortran CUDA

Graphic processing units or GPUs have evolved into programmable, highly parallel computational units with very high memory bandwidth, and potential for scientific applications.
GPU designs are optimized for the kind of computations found in graphics rendering, but are general enough to be useful in many cases involving data-parallelism, linear algebra and other common use cases in scientific computing.

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



{% include links.md %}
