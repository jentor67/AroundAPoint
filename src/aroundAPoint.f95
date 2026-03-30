!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule
   use startParameters 
   implicit none
   integer, dimension(100000) :: units
   character(len=100) :: filename
   integer :: n, m, k, particles, iterations, io, c, perCur
   real*8 :: force, gravity, fx, fy, fz, fxsum, fysum, fzsum, distpart
   real*8 :: startX, startY, startZ, r
   type(particle), dimension(2) :: partarray
   
   call execute_command_line("rm -f /mnt/kdrive/*.dat")
  
   iterations = 3600*24*365.25  ! one year
   !iterations = 315360  ! 1 % one year
   iterations = 3  ! 1 % one year
   
   particles = size(partarray,dim=1)
   !write(*,*) particles
   call valuetest(partarray(1))
   !write(*,*) partarray(1)%x
 
   ! get initial positions of particles
   do n = 1, particles
     !write(*,*) "in loop", n ,particles
     write(filename, '(A,I8.8,A)') '/mnt/kdrive/file_', n, '.dat'
     open(newunit=units(n), file=filename, status="replace", action="write")
     if( n > 1 ) call getpartparm(partarray(n)) 
   end do

   !call printparticles(partarray, io, particles)
   
   !write(*,*) "Before loop",particles
   !open(newunit=io, file="/mnt/kdrive/data.txt",status="replace", action="write")

   !write(*,*) " "
   !write(*,*) " "
   ! print initial values
   do n = 1, particles
     call printparticle( n, partarray(n) )
   end do

   startX = partarray(2)%x
   startY = partarray(2)%y
   startZ = partarray(2)%z
   !write(*,*) " "
   write(*,*) "Start of Iterations"
   do n = 1, iterations
     write(*,*) "Iteration---------------------------------------:", n
     !write(*,*) "Test loop",particles
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
           !write(*,*) "Partical Force:",k, fxsum, fysum, fzsum
         end if
       end do
       write(*,*) "Part",m
       write(*,*) "Force:",fxsum, fysum, fzsum
       write(*,*) "Loc:", partarray(m)%x,partarray(m)%y,partarray(m)%z
       write(*,*) "Spd:", partarray(m)%u,partarray(m)%v,partarray(m)%w
       call velocitychange(partarray(m), fxsum,fysum,fzsum)
       call positionchange(partarray(m))
       write(*,*) " "
     end do
     !call printparticles(partarray, io, particles)
     !call printparticle(1, partarray(n+1,1))
     !call printparticle(2, partarray(n+1,2))
   end do
   write(*,*) "End of Iterations"

   r = ( ((startX-partarray(2)%x)**2) + &
         ((startY-partarray(2)%y)**2) + &
         ((startZ-partarray(2)%x)**2) )**.5
   c = 2*pi*partarray(2)%a

   perCur = 100*r/c
   write(*,*) r, c, partarray(2)%a, perCur

   ! print final values
   do n = 1, particles
     call printparticle( n, partarray(n) )
   end do

   !write(*,*) io
   !close(io)
   !write(*,*) io

   do n = 1, particles
      close(units(n))
   end do
End Program aroundAPoint
