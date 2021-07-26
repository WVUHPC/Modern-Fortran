      program fft_with_MPI
      parameter(indx = 15)
      parameter(nx = 2**indx,maxiters=1000)
      integer comm,myid,nnodes,ierr
      double complex,allocatable,dimension(:)  :: f,g,sct,a,b,t
      integer,allocatable,dimension(:)  :: mixup
      double precision time_begin,time_end
      include 'mpif.h'

      comm = MPI_COMM_WORLD
      call MPI_init(ierr)
      call MPI_COMM_RANK(comm,myid,ierr)
      call MPI_COMM_SIZE(comm,nnodes,ierr)

      kxp = nx/nnodes
      allocate(f(kxp))
      allocate(g(kxp))
      allocate(sct(kxp))
      allocate(mixup(kxp))

      do i=1,kxp
        f(i) = cmplx(real(i),real(myid+1))
      enddo      


      isign = 1
      kblok = nnodes

      time_begin =  MPI_Wtime()
  
      call  bitrv1(mixup,isign,indx,kxp,kblok,sct,myid)
      !do iter =1,maxiters
         call permute(f,g,mixup,indx,kxp,kblok,comm)
         call fft(f,g,isign,mixup,sct,indx,kxp,kblok,comm,myid)
      !enddo
      time_end =  MPI_Wtime()
    
      print *,'fft of 1-d with size= ',nx,'  took '
      print *,time_end-time_begin,' seconds for ',maxiters,' iterations'

      call MPI_FINALIZE(ierr)

      end



!kblok is the number of processors
!g is the input vector whose fft will be taken
!kxp is the length of local data

        subroutine fft(f,g,isign,mixup,sct,indx,kxp,kblok,comm,myprocid)
        double complex f,g,sct
	dimension f(kxp),g(kxp)
        dimension sct(kxp),mixup(kxp)  
        integer myprocid,comm

        include 'mpif.h'

	integer dest,source,nproc,dest_id,lind,lb,ub,sign,info
	double complex t1
        integer status(MPI_STATUS_SIZE)
	
	nproc = kblok
	nx = 2**indx
	nxh = nx/2
	indx1 = indx !-1
	kbs = nxh/kxp
	dnx = 6.28318530717959/float(nx)
	sign = isign
	temp= log10(float(kxp))/log10(2.0)
        lind = temp

!C ---------------------------
!C inverse fourier transform
!C bit-reverse array elements to temporary
!C ---------------------------
           
         do  l = 1, indx1
            nxs = 2**(l - 1)
            kxs = nxs/kxp
!C ---------------------------
!C local calculation
!C ---------------------------

            if (kxs.eq.0) then
               do j = 1, kxp
                  lb = (j - 1)/nxs
                  if (lb.eq.(2*(lb/2))) then
                     f(j) = g(j+nxs)
                  else
                     f(j) = g(j-nxs)
                  endif
               enddo              
!C ---------------------------
!C perform reduction
!C ---------------------------
               km = kxp/nxs
               do j = 1, kxp
                   kb = j - 1
                   lb = kb/nxs
                   if (sign .eq. -1) then
                      t1 = sct(1+km*(kb-nxs*lb))
                   else if (sign .eq.1) then
                      t1 = conjg(sct(1+km*(kb-nxs*lb)) )
                   endif
                   
                   if (lb.eq.(2*(lb/2))) then
                      g(j) = g(j) + t1*f(j)
                   else
                      g(j) = f(j) - t1*g(j)
                   endif
                enddo
            else
!C ---------------------------
!C copy data
!C ---------------------------
                          
                 if (btest(myprocid,l-lind-1) .eqv. .false.) then
                    dest = ibset(myprocid,l-lind-1)
                    call MPI_SEND(g,kxp,MPI_DOUBLE_COMPLEX,dest,0,comm,ierr)
                 else
                    source = ibclr(myprocid,l-lind-1)
                    call MPI_RECV(f,kxp,MPI_DOUBLE_COMPLEX,source,0,comm,status,ierr)
                 endif
  
                 if (btest(myprocid,l-lind-1) .eqv.  .true.) then
                    dest = ibclr(myprocid,l-lind-1)
                    call MPI_SEND(g,kxp,MPI_DOUBLE_COMPLEX,dest,0,comm,ierr)
                 else
                    source = ibset(myprocid,l-lind-1)
                    call MPI_RECV(f,kxp,MPI_DOUBLE_COMPLEX,source,0,comm,status,ierr) 
                 endif
              
!C ------------------
!C perform reduction
!C ------------------
               km = nxh/nxs
               dns = dnx*float(km)
 
               kb = myprocid
               llb = kb/kxs
               kb = kxp*(kb - kxs*llb) - 1
               llb = llb - 2*(llb/2)
               do j = 1, kxp
                   arg = dns*float(j + kb)
                   t1 = cmplx(cos(arg),sign*sin(arg))
                   if (llb.eq.0) then
                      g(j ) = g(j) + t1*f(j)
                   else
                      g(j ) = f(j) - t1*g(j)
                   endif
               enddo
            endif

        enddo
!C do l=1,indx1
  
        return
        end 


        subroutine bitrv1(mixup,isign,indx,kxp,kblok,sct,myprocnum)

        double complex  sct(kxp)
        integer  mixup(kxp)
        integer isign,indx,kxp,kblok,myprocnum

        integer llb,ub
        real arg,dnx
       
        nx = 2**indx
        nxh = nx/2
        dnx = 6.28318530717959/float(nx)
      
!C ---------------------------------
!C prepare bit-reverse index table
!C ---------------------------------

        koff = kxp*myprocnum - 1
        do  j = 1, kxp
           lb = j + koff
           ll = 0
           do  l = 1, indx
              jb = lb/2
              it = lb - 2*jb
              lb = jb
              ll = 2*ll + it
           enddo
           mixup(j) = ll + 1
        enddo

        do j = 1, kxp
           arg = dnx*float((nxh*(j - 1))/kxp)
           sct(j) = cmplx(cos(arg),-sin(arg))
        enddo
        return
        end

        subroutine permute(f,g,mixup,indx,kxp,kblok,comm)

        double complex f,g
	dimension f(kxp),g(kxp)
        dimension mixup(kxp) 
        integer comm 

	double complex global_f(kxp,kblok)
        include 'mpif.h'

!C ---------------------------
!C inverse fourier transform
!C bit-reverse array elements to temporary
!C ---------------------------

         call MPI_ALLGATHER(f,kxp,MPI_DOUBLE_COMPLEX,global_f,kxp,&
              MPI_DOUBLE_COMPLEX,comm,ierr)
         
         do j = 1, kxp
            ll = mixup(j)
            kk = (ll - 1)/kxp + 1
            jj = ll - kxp*(kk - 1)
            g(j) = global_f(jj,kk)
         enddo

         return
         end


