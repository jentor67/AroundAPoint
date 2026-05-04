!> \\file main.f95
Program main
   use readconfigmodule
   use blenderconfigmodule
   use startparametersmodule
   use constantsmodule
   use gravitymodule
   use openmp_loop
   use omp_lib
   implicit none

   !character(len=100) :: filename
   character(len=100) :: filename_blender
   character(len=256) :: config_file_path

   integer, allocatable :: units(:)
   integer, allocatable :: units_blender(:)
   integer :: n, particles, stat, temp_id
   integer :: n_blender, n_blender_div, n_blender_limit

   logical :: blender

   !real(dp) :: c, perCur
   !real(dp) :: r
   real(dp) :: startX, startY, startZ

   real(dp) :: start_time, end_time
   type(particle), allocatable :: partarray(:)

   !************************************************* 

   !call cpu_time(start_time)
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

   call read_config_file(config_file_path)

   call write_blender_file()

   allocate(partarray(bc%ObjectCount))

   ! clear working data
   call execute_command_line("rm -f " // trim(bc%output_directory) &
           // "*.dat")


   centerMass =  bc%CenterMass

   particles = bc%ObjectCount !size(partarray,dim=1)

   call valueLargeBody(partarray(1),bc)
 
   allocate( units(bc%ObjectCount) )
   allocate( units_blender(bc%ObjectCount) )

   ! get initial positions of particles
   do n = 1, particles
     ! **** create main data file ****
     !write(filename, '(A,I8.8,A)') trim(bc%output_directory) &
     !        // 'file_', n, '.dat'

     !open(newunit=temp_id, file=filename, status='replace', &
     !        action='write', iostat=stat)

     !if (stat /= 0) then
     !   print *, "Error opening file, iostat = ", stat
     !   stop
     !end if
     
     !write(temp_id,'(A)') "frame|x|y|z|u|v|w"
     !units(n) = temp_id
     ! ****    ****

     ! **** create blender file ****
     write(filename_blender, '(A,I8.8,A)') trim(bc%output_directory) &
             // 'file_blender_', n, '.dat'

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
   n_blender_limit = bc%blender_limit
   n_blender_div = bc%Iterations/n_blender_limit
   ! ****************************

   write(*,*) "Start of Iterations"
   do n = 1, bc%Iterations
     blender= .false.
     !call printparticles(n, partarray, units, particles,blender)

     ! test if writing to blender
     if( modulo(n,n_blender_div) == 0 ) then
       blender= .true. ! set blender to true
       call printparticles(n_blender, partarray, units_blender, &
               particles,blender)
       n_blender = n_blender + 1
       blender= .false. ! set blender to false
     end if
    
     ! *** call force loop **
     call force_loop(partarray) 

     !  *****update velocity and position
     call velocity_loop(partarray)

     call position_loop(partarray)

     !do m = 1, particles
     !  call velocitychange(partarray(m))
     !  call positionchange(partarray(m))
     !end do 
     ! ******************************

     !  test if there are any collisions
     call collisionTest(partarray,particles)
     
   end do
   write(*,*) "End of Iterations"

   !r = ( ((startX-partarray(2)%x)**2) + &
   !      ((startY-partarray(2)%y)**2) + &
   !      ((startZ-partarray(2)%z)**2) )**.5
   !c = 2.0*pie*partarray(2)%r

   !perCur = 100*r/c

   ! print final values
   !do n = 1, particles
     !call printparticle( n, partarray(n) )
   !end do


   do n = 1, particles
      !close(units(n))
      close(units_blender(n))
   end do


   !call cpu_time(end_time)
   end_time = omp_get_wtime()
   print *, "Elapsed CPU time:", end_time - start_time, "seconds"
End Program main
