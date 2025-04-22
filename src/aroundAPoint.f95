!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none
   integer :: n, m, k, particles, iterations
   real :: force, gravity, fx, fy, fz, fxsum, fysum, fzsum, distpart
   type(particle), dimension(1000000,100) :: partarray
   type(particle) :: sun
   iterations = size(partarray,dim=1)-1
   particles = size(partarray,dim=2)

   ! get initial positions of particles
   do n = 1, particles
     call getpartparm(partarray(1,n)) 
   end do

   ! pass mass to all elements
   do n = 1, iterations
     do m = 1, particles
       partarray(n, m)%mass = partarray(1,m)%mass
     end do
   end do 

   !  check distance
   do n = 1, particles
     do m = 1, particles
       distpart = distance( partarray(1,n), partarray(1,m) )
       if( distpart < .1 .and. n /= m) then
         write(*,*) "Close", distpart, n, m
       end if
     end do
   end do

   call valuessun(sun) ! get value of sun


   call printparticle( 1, partarray(1,1) )
   call printparticle( 2, partarray(1,2) )
   call printparticle( 3, partarray(1,3) )
   call printparticle( 4, partarray(1,4) )

   do n = 1, iterations
     !write(*,*) "Iteration: ", n
     do m = 1, particles
       fxsum = 0
       fysum = 0
       fzsum = 0
       do k = 1, particles
         if( k /= m) then
           call forcevector(partarray(n,m),partarray(n,k), fx, fy, fz)
           fxsum = fxsum + fx
           fysum = fysum + fy
           fzsum = fzsum + fz
         end if
       end do
       call velocitychange(partarray(n,m), partarray(n+1,m),fxsum,fysum,fzsum)
       call positionchange(partarray(n,m), partarray(n+1,m) )
     end do
     !call printparticle(1, partarray(n+1,1))
     !call printparticle(2, partarray(n+1,2))
   end do
   write(*,*) n, n*timedisp
   call printparticle( 1, partarray(n-1,1) )
   call printparticle( 2, partarray(n-1,2) )
   call printparticle( 3, partarray(n-1,3) )
   call printparticle( 4, partarray(n-1,4) )

End Program aroundAPoint
