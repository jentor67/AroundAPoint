program mpi_scatter_gather
    !*********compile program
    !>mpif90 -std=f2008 -Wall -fcheck=all run_mpi.f90 -o scatter_gather
    !********run program
    !>mpirun -np 4 ./scatter_gather

    use mpi
    implicit none

    integer :: ierr, rank, nsize
    integer, parameter :: N = 400000
    integer :: i, local_n

    integer, dimension(N) :: data, result
    integer, allocatable :: local_data(:), local_result(:)
    
    real :: start_time, end_time

    call cpu_time(start_time)

    !  MPI initial
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nsize, ierr)

    ! Ensure N is divisible by number of processes
    if (mod(N, nsize) /= 0) then
        if (rank == 0) print *, "N must be divisible by number of processes"
        call MPI_Finalize(ierr)
        stop
    end if

    local_n = N / nsize
    allocate(local_data(local_n), local_result(local_n))

    ! Initialize data on root process
    if (rank == 0) then
        do i = 1, N
            data(i) = i
        end do
        print *, "Original data:" !, data
    end if

    ! Scatter data to all processes
    call MPI_Scatter(data, local_n, MPI_INTEGER, &
                     local_data, local_n, MPI_INTEGER, &
                     0, MPI_COMM_WORLD, ierr)

    ! Each process computes: square the numbers
    do i = 1, local_n
        local_result(i) = start_inner_loop(local_data)
    end do

    print *, "Rank", rank !, "local result:", local_result

    ! Gather results back to root
    call MPI_Gather(local_result, local_n, MPI_INTEGER, &
                    result, local_n, MPI_INTEGER, &
                    0, MPI_COMM_WORLD, ierr)

    ! Print final result on root
    if (rank == 0) then
        print *, "Final gathered result:" !, result
    end if

    call MPI_Finalize(ierr)

    call cpu_time(end_time)
    print *, "Elapsed CPU time:", end_time - start_time, "seconds"

contains

     function start_inner_loop(arr1) result(val1)
         implicit none
         integer, intent(in) :: arr1(:)
         integer :: val1, j
     
         val1 = 0
         do j = 1, size(arr1)
             val1 = val1 + (arr1(j)**2) - (arr1(j)**.5)
             !val1 = val1 + arr1(j) - 2*arr1(j)
         end do
     end function start_inner_loop


end program mpi_scatter_gather
