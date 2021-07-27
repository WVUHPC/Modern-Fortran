      program scatter_vector
      include 'mpif.h'
      integer ndims,xmax,ymax,nnodes,myid,totelem
      parameter(ndims=2)
      parameter(xmax=1000,ymax=1000)
      parameter(niters=1)
      parameter (totelem=xmax*ymax)
      integer comm,ierr
      integer status(MPI_STATUS_SIZE)

      double precision,allocatable,dimension(:,:) :: A
      double precision,allocatable,dimension(:)   :: V,dindex1,dindex2,gV
      integer,allocatable,dimension(:)            :: index1,index2
      integer,allocatable,dimension(:)            :: lindex1,lindex2
        
!distribute each array into nnodes processors except gV and index1 and index2
! these last 3 arrays are global arrays. gv will hold the all local V's
! index1 and index2 will hold all the local lindex1's and lindex2's 
! respectively 

      integer xb,yb,i,j,nitems,xbv

      comm=MPI_COMM_WORLD
 
      call MPI_init(ierr)
      call MPI_COMM_RANK(comm,myid,ierr)
      call MPI_COMM_SIZE(comm,nnodes,ierr)
  
!initialize the input matrixes A and allocate necessary spaces for A,B
!partition the matrix a in (block ,*) way

      xb  = xmax/nnodes
      xbv = totelem/nnodes
!allocate necessary arrays

      allocate(A(xb,ymax))
      allocate(V(xbv))
      allocate(gV(totelem))
      allocate(index1(totelem))
      allocate(index2(totelem))
      allocate(dindex1(totelem))
      allocate(dindex2(totelem)) 
      allocate(lindex1(xbv))
      allocate(lindex2(xbv))
!last processor is  generating random index vectors index1 and index2
! and partition them onto processors and send to correspoding processor
!also last processor is generating random data vector V for each processor

      if (myid == nnodes -1) then
          call random_number(dindex1)
          call random_number(dindex2)
          index1 = xmax*dindex1 + 1
          index2 = ymax*dindex2 + 1 
          do np = 0,nnodes-1
            call random_number(V)
            V = 1000000.0*V 
            if (np < nnodes-1) then
              call MPI_SEND(V,xbv,MPI_DOUBLE_PRECISION,np,0,comm,ierr)
              call MPI_SEND(index1(xbv*np+1),xbv,MPI_INTEGER,np,0,comm,ierr)
              call MPI_SEND(index1(xbv*np+1),xbv,MPI_INTEGER,np,0,comm,ierr)
            endif
          enddo
          lindex1 = index1((nnodes-1)*xbv+1:totelem)
          lindex2 = index2((nnodes-1)*xbv+1:totelem)
      else
          call MPI_RECV(V,xbv,MPI_DOUBLE_PRECISION,nnodes-1,0,comm,status,ierr)
          call MPI_RECV(lindex1,xbv,MPI_INTEGER,nnodes-1,0,comm,status,ierr)
          call MPI_RECV(lindex1,xbv,MPI_INTEGER,nnodes-1,0,comm,status,ierr)
      endif

!start timer .....
      time_begin =  MPI_Wtime()
      do iter = 1,niters
!collect all the local V's in gV
        call MPI_ALLGATHER(V,xbv,MPI_DOUBLE_PRECISION, &
         gV,xbv,MPI_DOUBLE_PRECISION,comm,ierr)

!collect all the local arrays index1's and index2's in index1 and index2
        call MPI_ALLGATHER(lindex1,xbv,MPI_INTEGER,index1,&
           xbv,MPI_INTEGER,comm,ierr)
        call MPI_ALLGATHER(lindex2,xbv,MPI_INTEGER,index2,&
           xbv,MPI_INTEGER,comm,ierr)
        ilb = myid*xbv+1
        iub = (myid+1)*xbv  
        do i=1,totelem
!If I am holding the vector index 'index1(i)' in my local array
!I will get the gV(i)
          if ((index1(i) .ge. ilb).and.(index1(i).le.iub)) then
             A(index1(i)-ilb+1,index2(i)) = gV(i)
          endif
        enddo           

      enddo
! Stop timer

      time_end =  MPI_Wtime()

      if (myid == 0) then
        print *,'Elapsed time ',niters,'iterations for scatter'
        print *,'For matrix with dimensions',xmax,ymax ,'is'
        print *,time_end-time_begin ,'seconds'   
       endif

      deallocate(a)
      deallocate(v)
      deallocate(gv)
      deallocate(index1)
      deallocate(index2)
      deallocate(lindex1)
      deallocate(lindex2)
      deallocate(dindex1)
      deallocate(dindex2)
     
      call MPI_FINALIZE(ierr)

      end

    SUBROUTINE all_to_all_int(myprocid,nnodes,comm,fx,global_fx,xbv,totelem)
    integer xbv,totelem
    integer fx(xbv),global_fx(totelem)
    integer myprocid,nnodes,comm

    include 'mpif.h'

    integer dest,source,nproc,dest_id,ierr
    integer status(MPI_STATUS_SIZE) 
   

    do j=1,xbv
      global_fx(xbv*myprocid+1+j) = fx(j)
    enddo

    nproc = nnodes
    kcnt = myprocid
    dest   = mod(myprocid+1,nproc)
    source = mod(myprocid-1+nproc,nproc)
 
    do i=1,nproc-1
      if (mod (myprocid,2) .eq. 0) then
         call MPI_SEND(global_fx(kcnt*xbv+1),xbv,&
         MPI_INTEGER,dest,0,comm,ierr)
      else
         ikcnt = mod(kcnt-1+nproc,nproc)
         call MPI_RECV(global_fx(ikcnt*xbv+1),xbv, &
           MPI_INTEGER,source,0,comm,status,ierr)
      endif

     if (mod (myprocid,2) .eq. 1) then
        call MPI_SEND(global_fx(kcnt*xbv+1),xbv,&
         MPI_INTEGER,dest,0,comm,ierr)
     else
        ikcnt = mod(kcnt-1+nproc,nproc)
        call MPI_RECV(global_fx(ikcnt*xbv+1),xbv, &
           MPI_INTEGER,source,0,comm,status,ierr)
     endif
     kcnt = ikcnt
   enddo
 
   return
   end
 
      
    SUBROUTINE all_to_all_float(myprocid,nnodes,comm,fx,global_fx,xbv,totelem)
    integer xbv,totelem
    double precision fx(xbv),global_fx(totelem)
    integer myprocid,nnodes,comm

    include 'mpif.h'

    integer dest,source,nproc,dest_id,ierr
    integer status(MPI_STATUS_SIZE) 
   

    do j=1,xbv
      global_fx(xbv*myprocid+1+j) = fx(j)
    enddo

    nproc = nnodes
    kcnt = myprocid
    dest   = mod(myprocid+1,nproc)
    source = mod(myprocid-1+nproc,nproc)
 
    do i=1,nproc-1
      if (mod (myprocid,2) .eq. 0) then
         call MPI_SEND(global_fx(kcnt*xbv+1),xbv,&
         MPI_DOUBLE_PRECISION,dest,0,comm,ierr)
      else
         ikcnt = mod(kcnt-1+nproc,nproc)
         call MPI_RECV(global_fx(ikcnt*xbv+1),xbv, &
           MPI_DOUBLE_PRECISION,source,0,comm,status,ierr)
      endif

     if (mod (myprocid,2) .eq. 1) then
        call MPI_SEND(global_fx(kcnt*xbv+1),xbv,&
         MPI_DOUBLE_PRECISION,dest,0,comm,ierr)
     else
        ikcnt = mod(kcnt-1+nproc,nproc)
        call MPI_RECV(global_fx(ikcnt*xbv+1),xbv, &
           MPI_DOUBLE_PRECISION,source,0,comm,status,ierr)
     endif
     kcnt = ikcnt
   enddo
 
   return
   end
 

