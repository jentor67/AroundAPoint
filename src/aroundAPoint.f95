!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none
   integer :: n, m, k, particles, iterations, io
   real :: force, gravity, fx, fy, fz, fxsum, fysum, fzsum, distpart
   type(particle), dimension(3) :: partarray
   iterations = 10
   particles = size(partarray,dim=1)

   call valuetest(partarray(1))

   ! get initial positions of particles
   do n = 2, particles
     call getpartparm(partarray(n)) 
   end do

   
   open(newunit=io, file="/home/jmajor/data.txt",status="replace", action="write")
   call printparticle( 1, partarray(1) )
   call printparticle( 2, partarray(2) )

   do n = 1, iterations
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
     call printparticles(partarray, io, particles)
     !call printparticle(1, partarray(n+1,1))
     !call printparticle(2, partarray(n+1,2))
   end do
   write(*,*) n, n*timedisp
   call printparticle( 1, partarray(1) )
   call printparticle( 2, partarray(2) )

   close(io)
End Program aroundAPoint
