!> \\file main.f90
Program main
   use readconfigmodule
   use particle_module
   use startparametersmodule
   use constantsmodule
   use gravitymodule
   use openmp_loop
   use omp_lib
   implicit none

   character(len=100) :: filename_blender
   character(len=256) :: config_file_path

   integer, allocatable :: units(:)
   integer, allocatable :: units_blender(:)
   integer :: i, n, particles, stat, temp_id
   integer :: n_blender, n_blender_div, n_blender_limit

   real(dp) :: start_time, end_time

   type(particle), allocatable :: partarray(:)

   !************************************************* 

   start_time = omp_get_wtime()

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

   call read_config_file(config_file_path,partarray)

   
   ! clear working data
   call execute_command_line("rm -f " // trim(bc%output_directory) &
           // "*.dat")

   particles = bc%ObjectCount !size(partarray,dim=1)
   write(*,*) "particle", particles

   allocate( units(bc%ObjectCount) )
   allocate( units_blender(bc%ObjectCount) )



   if( bc%file_type == "RandomCenter" ) then
      call valueLargeBody(partarray(1),bc)
      centerMass =  bc%CenterMass
   end if

   ! get initial positions of particles
   do n = 1, particles
     write(*,*) 'DEBUG dir: [', trim(bc%output_directory), ']'
     ! **** create blender file ****
     write(filename_blender, '(A,I8.8,A)') trim(bc%output_directory) &
             // 'file_blender_', n, '.dat'
     write(*,*) "Blenderfile:", filename_blender
     open(newunit=temp_id, file=filename_blender, status='replace', &
             action='write', iostat=stat)

     if (stat /= 0) then
        print *, "Error opening file, iostat = ", stat
        stop
     end if
     
     write(temp_id,'(A)') "frame|x|y|z|u|v|w|radius"
     units_blender(n) = temp_id

     if( n > 1 .and. bc%file_type == "RandomCenter" ) call getpartparm(partarray(n),bc) 

   end do


   ! set the blender file numbers
   n_blender = 1
   n_blender_limit = bc%blender_limit
   ! ****************************

   ! determine the times to write to file
   ! n_blender_limit >= bc%Iterations --> 1
   ! n_blender_limit >
   if( n_blender_limit >= bc%Iterations ) then
     n_blender_div = 1
   else
     n_blender_div = bc%Iterations/n_blender_limit
   end if
   ! ****************************

   write(*,*) "Start of Iterations"

   do n = 1, bc%Iterations
     ! test if writing to blender
     if( modulo(n, n_blender_div) == 0 ) then
       call printparticles(n_blender, partarray, units_blender, &
               particles)
       n_blender = n_blender + 1
     end if

     !  ### using the class ###
     ! --- half-kick (v += 0.5*a*dt) ---
     !$omp parallel private(i) shared(partarray)
       
       !$omp do
       do i = 1, particles
         call partarray(i)%half_kick(bc%dt)
       end do
       !$omp end do
       
       ! --- drift (x += v*dt) ---
       !$omp do
       do i = 1, particles
         call partarray(i)%drift(bc%dt)
       end do
       !$omp end do
       
       ! --- zero forces, recompute at new positions ---
       !$omp do
       do i = 1, particles
         call partarray(i)%zero_force()
       end do
       !$omp end do
       
     !$omp end parallel 
  
     !call forcevectorloop(partarray, particles)   ! still a standalone parallel routine
     call force_loop(partarray)   ! still a standalone parallel routine
     !call collisionTest(partarray, particles)     ! internally calls collide_with
       
     ! --- half-kick again ---
     !$omp parallel do private(i) shared(partarray)
     do i = 1, particles
       call partarray(i)%half_kick(bc%dt)
     end do
     !$omp end parallel do
     !   #########################


     ! Leapfrog: half-kick velocity, full drift position, 
     ! recompute forces, half-kick again
     ! call velocity_half_loop(partarray)    ! v += 0.5*a*dt
     ! call position_loop(partarray)         ! x += v*dt
     ! call force_loop(partarray)            ! recompute forces at new x
     ! call velocity_half_loop(partarray)    ! v += 0.5*a*dt
     !  # end of leapfrog!
     
    !
     ! ### OLD version ###
     ! *** call force loop **
     !call force_loop(partarray) 

     !  *****update velocity and position
     !call velocity_loop(partarray)

     !call position_loop(partarray)
     !  ############################ 

     !  test if there are any collisions
     !call collisionTest( partarray, n ) !bc%Iterations )

   end do

   write(*,*) "End of Iterations"

   !  close blender files
   do n = 1, particles
      close(units_blender(n))
   end do


   end_time = omp_get_wtime()
   print *, "Elapsed CPU time:", end_time - start_time, "seconds"

End Program main
