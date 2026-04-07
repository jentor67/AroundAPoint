!> \\file main.f95
Program main
   use readconfigmodule
   use startparametersmodule
   use constantsmodule
   use gravitymodule
   implicit none

   character(len=100) :: filename
   character(len=256) :: config_file_path

   !integer, dimension(100000) :: units
   !integer :: units(100000)
   integer, allocatable :: units(:)
   integer :: n, m, k, particles, iterations, stat, temp_id

   real(kind=kind(1.0d0)) :: c, perCur
   real(kind=kind(1.0d0)) :: r
   real(kind=kind(1.0d0)) :: fx, fy, fz
   real(kind=kind(1.0d0)) :: fxsum, fysum, fzsum
   real(kind=kind(1.0d0)) :: startX, startY, startZ


   type(particle), dimension(10) :: partarray
 

   !************************************************* 
   
   ! clear working data
   call execute_command_line("rm -f /mnt/kdrive/*.dat")

   ! Get config file
   n = command_argument_count()
   print *, "Number of arguments:", n 
   call get_command_argument(1, config_file_path)
   config_file_path = trim(config_file_path)
   print *, "Config File: ", config_file_path
   

   call read_config_file(config_file_path)
  
   iterations = 3600*24*365.25  ! one year
   iterations = 315360  ! 1 % one year
   iterations = 3  ! 1 % one year
   
   particles = size(partarray,dim=1)

   call valueLargeBody(partarray(1))
 
   allocate(units(10))

   ! get initial positions of particles
   do n = 1, particles
     write(filename, '(A,I8.8,A)') '/mnt/kdrive/file_', n, '.dat'

     open(newunit=temp_id, file=filename, status='replace', &
             action='write', iostat=stat)

     if (stat /= 0) then
        print *, "Error opening file, iostat = ", stat
        stop
     end if

     units(n) = temp_id

     if( n > 1 ) call getpartparm(partarray(n)) 

     write(*,*) "Partical", n, particles, partarray(n)%omega, &
         partarray(n)%e, partarray(n)%i, partarray(n)%omegaBIG, &
         partarray(n)%mass, partarray(n)%a, partarray(n)%b, &
         partarray(n)%nue, partarray(n)%mue

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
End Program main
