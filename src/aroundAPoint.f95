!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule
   use startParameters 
   implicit none
   integer, dimension(100000) :: units
   character(len=100) :: filename
   integer :: n, m, k, particles, iterations, io
   real :: force, gravity, fx, fy, fz, fxsum, fysum, fzsum, distpart
   type(particle), dimension(10) :: partarray
   
   call execute_command_line("rm -f /mnt/kdrive/*.dat")
  
   iterations = 5 
   particles = size(partarray,dim=1)
   write(*,*) particles
   call valuetest(partarray(1))
   write(*,*) partarray(1)%x
 
   ! get initial positions of particles
   do n = 1, particles
     write(*,*) "in loop", n ,particles
     write(filename, '(A,I8.8,A)') '/mnt/kdrive/file_', n, '.dat'
     open(newunit=units(n), file=filename, status="replace", action="write")
     if( n > 1 ) call getpartparm(partarray(n)) 
   end do

   !call printparticles(partarray, io, particles)
   
   write(*,*) "Before loop",particles
   !open(newunit=io, file="/mnt/kdrive/data.txt",status="replace", action="write")
   call printparticle( 1, partarray(1) )
   call printparticle( 2, partarray(2) )

   do n = 1, iterations
     write(*,*) "Test loop",particles
     !call printparticles(partarray, units(n), particles)
     call printparticles(partarray, units, particles)
     do m = 1, particles
       fxsum = 0
       fysum = 0
       fzsum = 0
       do k = 1, particles
         if( k /= m) then
           call forcevector(partarray(m),partarray(k), fx, fy, fz)
           fxsum = fxsum + fx
           fysum = fysum + fy
           fzsum = fzsum + fz
         end if
       end do
       call velocitychange(partarray(m), fxsum,fysum,fzsum)
       call positionchange(partarray(m))
     end do
     !call printparticles(partarray, io, particles)
     !call printparticle(1, partarray(n+1,1))
     !call printparticle(2, partarray(n+1,2))
   end do
   write(*,*) n, n*timedisp
   call printparticle( 1, partarray(1) )
   call printparticle( 2, partarray(2) )

   write(*,*) io
   close(io)
   write(*,*) io

   do n = 1, particles
      close(units(n))
   end do
End Program aroundAPoint
