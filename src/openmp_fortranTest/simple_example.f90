program omp_example
  implicit none
  integer :: i, j
  real :: start_time, end_time
  integer, parameter :: n = 100000
  integer :: a(n)

  ! Initialize array
  do i = 1, n
     a(i) = 0
  end do

  call cpu_time(start_time)
  ! Parallel region with work-sharing DO loop
  do i = 1, n
     do j = 1, n
        a(i) = a(i) + i + j
     end do
     !print *, "Thread computing i =", i, " value =", a(i)
  end do

  call cpu_time(end_time)
  print *, "Elapsed CPU time:", end_time - start_time, "seconds"
end program omp_example
