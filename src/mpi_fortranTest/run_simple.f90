program simple
    implicit none

    integer, parameter :: N = 40000
    integer :: i, j, local_n
    real ::  start_time, end_time

    integer, dimension(N) :: dataIN, dataOUT


    ! Initialize data on root process
    call cpu_time(start_time)

    do i = 1, N
        dataIN(i) = i
    end do

    print *, "Start"

    ! Each process computes: square the numbers 
    do i = 1, N
        dataOUT(i) = start_inner_loop(dataIN, N)
    end do

    print *, "End:"
    call cpu_time(end_time)
    print *, "Elapsed CPU time:", end_time - start_time, "seconds"

contains

    function start_inner_loop(arr1, nsize) result(val1)
        implicit none
        integer :: nsize, val1, j
        integer, dimension(nsize) :: arr1
        val1 = 0
        do j = 1, N
            val1 = val1 + (arr1(j)**2) - (arr1(j)**.5)
        end do
    
    end function start_inner_loop

end program simple
