!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule
   use startParametersModule
   use constantsModule
   implicit none

   character(len=100) :: filename
   character(len=100) :: arg

   !integer, dimension(100000) :: units
   integer :: units(100000)
   integer :: n, m, k, particles, iterations, io

   real(kind=kind(1.0d0)) :: c, perCur
   real(kind=kind(1.0d0)) :: force, gravity, r
   real(kind=kind(1.0d0)) :: fx, fy, fz
   real(kind=kind(1.0d0)) :: fxsum, fysum, fzsum
   real(kind=kind(1.0d0)) :: startX, startY, startZ
   real(kind=kind(1.0d0)) :: distpart


   !call get_command_argument(1, arg)

   !n = command_argument_count()

   !write(*,*) "Arguments: ", n

   type(particle), dimension(10) :: partarray
  
   !call execute_command_line("rm -f /mnt/kdrive/*.dat")
  
   iterations = 3600*24*365.25  ! one year
   iterations = 315360  ! 1 % one year
   !iterations = 3  ! 1 % one year
   
   particles = size(partarray,dim=1)

   call valueLargeBody(partarray(1))
 
   ! get initial positions of particles
   do n = 1, particles
     write(*,*) "Partical", n ,particles
     write(filename, '(A,I8.8,A)') '/mnt/kdrive/file_', n, '.dat'
     !open(newunit=units(n), file=filename, status='replace', action='write')
     open(newunit=n, file=filename, status='replace', action='write')
     if( n > 1 ) call getpartparm(partarray(n)) 
     write(*,*) " "
   end do

   ! print initial values
   do n = 1, particles
     call printparticle( n, partarray(n) )
   end do

   startX = partarray(2)%x
   startY = partarray(2)%y
   startZ = partarray(2)%z
   write(*,*) "Start of Iterations"
   do n = 1, iterations
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
   end do
   write(*,*) "End of Iterations"

   r = ( ((startX-partarray(2)%x)**2) + &
         ((startY-partarray(2)%y)**2) + &
         ((startZ-partarray(2)%z)**2) )**.5
   c = 2.0*pie*partarray(2)%a

   perCur = 100*r/c

   ! print final values
   do n = 1, particles
     call printparticle( n, partarray(n) )
   end do


   do n = 1, particles
      close(units(n))
   end do
End Program aroundAPoint
