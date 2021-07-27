c  This program finds the force on each of a set of particles interacting
c  via a long-range 1/r**2 force law.
c
c  The number of processes must be even, and the total number of points
c  must be exactly divisible by the number of processes.
c
c  This is the MPI version.
c
        program nbody
        implicit none
        include 'mpif.h'
        integer myrank, ierr, nprocs, npts, nlocal
        integer pseudohost, NN, MM, PX, PY, PZ, FX, FY, FZ
        real G
        parameter (pseudohost = 0)
        parameter (NN=10000, G = 1.0)
        parameter (MM=0, PX=1, PY=2, PZ=3, FX=4, FY=5, FZ=6)
        real dx(0:NN-1), dy(0:NN-1), dz(0:NN-1)
        real dist(0:NN-1), sq(0:NN-1)
        real fac(0:NN-1), tx(0:NN-1), ty(0:NN-1), tz(0:NN-1)
        real p(0:6,0:NN-1), q(0:6,0:NN-1) 
        integer i, j, k, dest, src
        double precision timebegin, timeend
        integer status(MPI_STATUS_SIZE)
        integer newtype
        double precision ran
        integer iran
c
c  Initialize MPI, find rank of each process, and the number of processes
c
        call mpi_init (ierr)
        call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr)
        call mpi_comm_size (MPI_COMM_WORLD, nprocs, ierr)
c
c  One process acts as the host and reads in the number of particles
c
        if (myrank .eq. pseudohost) then
           open (4,file='nbody.input',status='old',err=998)
           if (mod(nprocs,2) .eq. 0) then
              read  (4,*) npts
              if (npts .gt. nprocs*NN) then
                 print *,'Warning!! Size out of bounds!!'
                 npts = -1
              else if (mod(npts,nprocs) .ne. 0) then
                 print *,'Number of processes must divide npts'
                 npts = -1
              end if
           else
              print *,'Number of processes must be even'
              npts = -1
           end if
        end if
c
c  The number of particles is broadcast to all processes
c
        call mpi_bcast (npts, 1, MPI_INTEGER, pseudohost, 
     #                  MPI_COMM_WORLD, ierr)
c
c  Abort if number of processes and/or particles is incorrect
c
        if (npts .eq. -1) goto 999
c
c  Work out number of particles in each process
c
        nlocal  = npts/nprocs
c
c  The pseudocode hosts initializes the particle data and sends each 
c  process its particles.
c
        if (myrank .eq. pseudohost) then
           iran = myrank + 111
           do i=0,nlocal-1
              p(MM,i) = sngl(ran(iran))
              p(PX,i) = sngl(ran(iran))
              p(PY,i) = sngl(ran(iran))
              p(PZ,i) = sngl(ran(iran))
              p(FX,i) = 0.0
              p(FY,i) = 0.0
              p(FZ,i) = 0.0
           end do
           do k=0,nprocs-1
              if (k .ne. pseudohost) then
                 do i=0,nlocal-1
                    q(MM,i) = sngl(ran(iran))
                    q(PX,i) = sngl(ran(iran))
                    q(PY,i) = sngl(ran(iran))
                    q(PZ,i) = sngl(ran(iran))
                    q(FX,i) = 0.0
                    q(FY,i) = 0.0
                    q(FZ,i) = 0.0
                 end do
                 call mpi_send (q, 7*nlocal, MPI_REAL, 
     #                          k, 100, MPI_COMM_WORLD, ierr)
              end if
           end do
        else
           call mpi_recv (p, 7*nlocal, MPI_REAL, 
     #                    pseudohost, 100, MPI_COMM_WORLD, status, ierr)
        end if
c
c  Initialization is now complete. Start the clock and begin work.
c  First each process makes a copy of its particles.
c
        timebegin = mpi_wtime ()
        do i= 0,nlocal-1
            q(MM,i) = p(MM,i)
            q(PX,i) = p(PX,i)
            q(PY,i) = p(PY,i)
            q(PZ,i) = p(PZ,i)
            q(FX,i) = 0.0
            q(FY,i) = 0.0
            q(FZ,i) = 0.0
        end do
c
c  Now the interactions between the particles in a single process are
c  computed.
c
        do i=0,nlocal-1
          do j=i+1,nlocal-1
            dx(i) = p(PX,i) - q(PX,j)
            dy(i) = p(PY,i) - q(PY,j)
            dz(i) = p(PZ,i) - q(PZ,j)
            sq(i) = dx(i)**2+dy(i)**2+dz(i)**2
            dist(i) = sqrt(sq(i))
            fac(i) = p(MM,i) * q(MM,j) / (dist(i) * sq(i))
            tx(i) = fac(i) * dx(i)
            ty(i) = fac(i) * dy(i)
            tz(i) = fac(i) * dz(i)
            p(FX,i) = p(FX,i)-tx(i)
            q(FX,j) = q(FX,j)+tx(i)
            p(FY,i) = p(FY,i)-ty(i)
            q(FY,j) = q(FY,j)+ty(i)
            p(FZ,i) = p(FZ,i)-tz(i)
            q(FZ,j) = q(FZ,j)+tz(i)
          end do
        end do
c
c  The processes are arranged in a ring. Data will be passed in an
C  anti-clockwise direction around the ring.
c
        dest = mod (nprocs+myrank-1, nprocs)
        src  = mod (myrank+1, nprocs)
c
c  Each process interacts with the particles from its nprocs/2-1
c  anti-clockwise neighbors. At the end of this loop p(i) in each
c  process has accumulated the force from interactions with particles
c  i+1, ...,nlocal-1 in its own process, plus all the particles from its
c  nprocs/2-1 anti-clockwise neighbors. The "home" of the q array is
C  regarded as the process from which it originated. At the end of
c  this loop q(i) has accumulated the force from interactions with 
C  particles 0,...,i-1 in its home process, plus all the particles from the
C  nprocs/2-1 processes it has rotated to.
c
        do k=0,nprocs/2-2
          call mpi_sendrecv_replace (q, 7*nlocal, MPI_REAL, dest, 200,
     #                         src, 200, MPI_COMM_WORLD, status, ierr)
          do i=0,nlocal-1
            do j=0,nlocal-1
              dx(i) = p(PX,i) - q(PX,j)
              dy(i) = p(PY,i) - q(PY,j)
              dz(i) = p(PZ,i) - q(PZ,j)
              sq(i) = dx(i)**2+dy(i)**2+dz(i)**2
              dist(i) = sqrt(sq(i))
              fac(i) = p(MM,i) * q(MM,j) / (dist(i) * sq(i))
              tx(i) = fac(i) * dx(i)
              ty(i) = fac(i) * dy(i)
              tz(i) = fac(i) * dz(i)
              p(FX,i) = p(FX,i)-tx(i)
              q(FX,j) = q(FX,j)+tx(i)
              p(FY,i) = p(FY,i)-ty(i)
              q(FY,j) = q(FY,j)+ty(i)
              p(FZ,i) = p(FZ,i)-tz(i)
              q(FZ,j) = q(FZ,j)+tz(i)
            end do
          end do
        end do
c
c  Now q is rotated once more so it is diametrically opposite its home
c  process. p(i) accumulates forces from the interaction with particles
c  0,..,i-1 from its opposing process. q(i) accumulates force from the
c  interaction of its home particles with particles i+1,...,nlocal-1 in
c  its current location.
c
        if (nprocs .gt. 1) then
          call mpi_sendrecv_replace (q, 7*nlocal, MPI_REAL, dest, 300,
     #                          src, 300, MPI_COMM_WORLD, status, ierr)
          do i=nlocal-1,0,-1
            do j=i-1,0,-1
              dx(i) = p(PX,i) - q(PX,j)
              dy(i) = p(PY,i) - q(PY,j)
              dz(i) = p(PZ,i) - q(PZ,j)
              sq(i) = dx(i)**2+dy(i)**2+dz(i)**2
              dist(i) = sqrt(sq(i))
              fac(i) = p(MM,i) * q(MM,j) / (dist(i) * sq(i))
              tx(i) = fac(i) * dx(i)
              ty(i) = fac(i) * dy(i)
              tz(i) = fac(i) * dz(i)
              p(FX,i) = p(FX,i)-tx(i)
              q(FX,j) = q(FX,j)+tx(i)
              p(FY,i) = p(FY,i)-ty(i)
              q(FY,j) = q(FY,j)+ty(i)
              p(FZ,i) = p(FZ,i)-tz(i)
              q(FZ,j) = q(FZ,j)+tz(i)
            end do
          end do
c
c  In half the processes we include the interaction of each particle with
c  the corresponding particle in the opposing process.
c
          if (myrank .lt. nprocs/2) then
            do i=0,nlocal-1
              dx(i) = p(PX,i) - q(PX,i)
              dy(i) = p(PY,i) - q(PY,i)
              dz(i) = p(PZ,i) - q(PZ,i)
              sq(i) = dx(i)**2+dy(i)**2+dz(i)**2
              dist(i) = sqrt(sq(i))
              fac(i) = p(MM,i) * q(MM,i) / (dist(i) * sq(i))
              tx(i) = fac(i) * dx(i)
              ty(i) = fac(i) * dy(i)
              tz(i) = fac(i) * dz(i)
              p(FX,i) = p(FX,i)-tx(i)
              q(FX,i) = q(FX,i)+tx(i)
              p(FY,i) = p(FY,i)-ty(i)
              q(FY,i) = q(FY,i)+ty(i)
              p(FZ,i) = p(FZ,i)-tz(i)
              q(FZ,i) = q(FZ,i)+tz(i)
            end do
          endif
c
c  Now the q array is returned to its home process.
c
          dest = mod (nprocs+myrank-nprocs/2, nprocs)
          src  = mod (myrank+nprocs/2, nprocs)
          call mpi_sendrecv_replace (q, 7*nlocal, MPI_REAL, dest, 400,
     #                         src, 400, MPI_COMM_WORLD, status, ierr)
        end if
c
c  The p and q arrays are summed to give the total force on each particle.
c
        do i=0,nlocal-1
          p(FX,i) = p(FX,i) + q(FX,i)
          p(FY,i) = p(FY,i) + q(FY,i)
          p(FZ,i) = p(FZ,i) + q(FZ,i)
        end do
c
c  Stop clock and write out timings
c
        timeend = mpi_wtime ()
        print *,'Node', myrank,'  Elapsed time: ',
     #          timeend-timebegin,' seconds'
c
c  Do a barrier to make sure the timings are written out first
c
        call mpi_barrier (MPI_COMM_WORLD, ierr)
c
c  Each process returns its forces to the pseudohost which prints them out.
c
        if (myrank .eq. pseudohost) then
           open (7,file='nbody.output')
           write (7,100) (p(FX,i),p(FY,i),p(FZ,i),i=0,nlocal-1)
           call mpi_type_vector (nlocal, 3, 7, MPI_REAL, newtype, ierr)
           call mpi_type_commit (newtype, ierr)
           do k=0,nprocs-1
              if (k .ne. pseudohost) then
                 call mpi_recv (q(FX,0), 1, newtype,
     #                          k, 100, MPI_COMM_WORLD, status, ierr)
                 write (7,100) (q(FX,i),q(FY,i),q(FZ,i),i=0,nlocal-1)  
              end if
           end do
        else
           call mpi_type_vector (nlocal, 3, 7, MPI_REAL, newtype, ierr)
           call mpi_type_commit (newtype, ierr)
           call mpi_send (p(FX,0), 1, newtype, 
     #                    pseudohost, 100, MPI_COMM_WORLD, ierr)
        end if


c
c  Close MPI
c
 999       call mpi_finalize (ierr)

        stop

 100       format(3e15.6)

c
c  Abort if no input file
c
 998          print *,'input file nbody.input missing or invalid'
        call mpi_abort (ierr)

        end
