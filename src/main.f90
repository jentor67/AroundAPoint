!> \\file main.f95
Program main
   use readconfigmodule
   use startparametersmodule
   use constantsmodule
   use gravitymodule
   implicit none

   character(len=100) :: filename
   character(len=100) :: filename_blender
   character(len=256) :: config_file_path

   !integer, dimension(100000) :: units
   !integer :: units(100000)
   integer, allocatable :: units(:)
   integer, allocatable :: units_blender(:)
   integer :: n, m, k, particles, stat, temp_id
   integer :: n_blender, n_blender_div, n_blender_limit

   logical :: blender

   real(kind=kind(1.0d0)) :: c, perCur
   real(kind=kind(1.0d0)) :: r
   real(kind=kind(1.0d0)) :: fx, fy, fz
   real(kind=kind(1.0d0)) :: fxsum, fysum, fzsum
   real(kind=kind(1.0d0)) :: startX, startY, startZ

   ! set 1,000,000 array
   type(particle), dimension(1000000) :: partarray
 

   !************************************************* 
   
   ! clear working data
   call execute_command_line("rm -f /mnt/kdrive/*.dat")

   ! Get config file
   n = command_argument_count()
   print *, "Number of arguments:", n 
   call get_command_argument(1, config_file_path)
   config_file_path = trim(config_file_path)
   print *, "Config File: ", config_file_path, len_trim(config_file_path)
   
   if( len_trim(config_file_path) == 0) then
           print *,"No name for config file"
           stop
   end if

   call read_config_file(config_file_path)


   centerMass =  bc%CenterMass

   particles = bc%ObjectCount !size(partarray,dim=1)

   call valueLargeBody(partarray(1),bc)
 
   allocate(units(10))
   allocate(units_blender(10))

   ! get initial positions of particles
   do n = 1, particles
     ! **** create main data file ****
     write(filename, '(A,I8.8,A)') '/mnt/kdrive/file_', n, '.dat'

     open(newunit=temp_id, file=filename, status='replace', &
             action='write', iostat=stat)

     if (stat /= 0) then
        print *, "Error opening file, iostat = ", stat
        stop
     end if
     
     write(temp_id,'(A)') "frame|x|y|z|u|v|w"
     units(n) = temp_id
     ! ****    ****

     ! **** create blender file ****
     write(filename_blender, '(A,I8.8,A)') '/mnt/kdrive/file_blender_', n, '.dat'

     open(newunit=temp_id, file=filename_blender, status='replace', &
             action='write', iostat=stat)

     if (stat /= 0) then
        print *, "Error opening file, iostat = ", stat
        stop
     end if
     
     write(temp_id,'(A)') "frame|x|y|z|u|v|w"
     units_blender(n) = temp_id

     ! ****    ****

     if( n > 1 ) call getpartparm(partarray(n),bc) 

     !write(*,*) "Partical", n, particles, partarray(n)%omega, &
     !    partarray(n)%e, partarray(n)%i, partarray(n)%omegaBIG, &
     !    partarray(n)%mass, partarray(n)%a, partarray(n)%b, &
     !    partarray(n)%nue, partarray(n)%mue

     !write(*,*) " "
   end do

   ! print initial values
   !do n = 1, particles
   !  call printparticle( n, partarray(n) )
   !end do

   startX = partarray(2)%x
   startY = partarray(2)%y
   startZ = partarray(2)%z

   ! set the blender file numbers
   n_blender = 1
   n_blender_limit = 2000
   n_blender_div = bc%Iterations/n_blender_limit
   ! ****************************

   write(*,*) "Start of Iterations"
   do n = 1, bc%Iterations
     blender= .false.
     call printparticles(n, partarray, units, particles,blender)

     ! test if writeing to blender
     if( modulo(n,n_blender_div) == 0 ) then
       blender= .true. ! set blender to true
       call printparticles(n_blender, partarray, units_blender, particles,blender)
       n_blender = n_blender + 1
       blender= .false. ! set blender to false
     end if

     do m = 1, particles

       if( partarray(m)%mass > 0.0 ) then

         fxsum = 0
         fysum = 0
         fzsum = 0

         do k = 1, particles

           if( k /= m .and. partarray(k)%mass > 0.0) then
             call forcevector(partarray(m),partarray(k), fx, fy, fz)
             fxsum = fxsum + fx
             fysum = fysum + fy
             fzsum = fzsum + fz
           end if

         end do

         call velocitychange(partarray(m), fxsum,fysum,fzsum)
         call positionchange(partarray(m))

       end if

     end do
     !  test if there are any collisions
     call collisionTest(partarray,particles)
     
   end do
   write(*,*) "End of Iterations"

   r = ( ((startX-partarray(2)%x)**2) + &
         ((startY-partarray(2)%y)**2) + &
         ((startZ-partarray(2)%z)**2) )**.5
   c = 2.0*pie*partarray(2)%a

   perCur = 100*r/c

   ! print final values
   !do n = 1, particles
     !call printparticle( n, partarray(n) )
   !end do


   do n = 1, particles
      close(units(n))
      close(units_blender(n))
   end do
End Program main
