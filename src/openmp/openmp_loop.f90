!> \\file open_loop.f90
module openmp_loop
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  use gravitymodule

  implicit none

  public :: force_loop, velocity_loop, position_loop, velocity_half_loop
contains

  subroutine force_loop(sel)
    integer :: i
    real(dp) :: fxsum, fysum, fzsum
    type(particle) :: sel(:) 

    ! suggestion from Claude AI
    real(dp), allocatable :: fx(:), fy(:), fz(:)   ! separate force arrays
    allocate(fx(size(sel)), fy(size(sel)), fz(size(sel)))
    ! #####################################
    
    ! Parallel region with work-sharing DO loop
    !$omp parallel do private(i, fxsum, fysum, fzsum) shared(sel)
    do i = 1, size(sel)
      call forcevectorloop(sel, i, size(sel), fxsum, fysum, fzsum)

      fx(i) = fxsum
      fy(i) = fysum
      fz(i) = fzsum

    end do
    !$omp end parallel do

    ! Write back after all threads are done
    do i = 1, size(sel)
      sel(i)%fx = fx(i)
      sel(i)%fy = fy(i)
      sel(i)%fz = fz(i)
    end do
  
    deallocate(fx, fy, fz)
  end subroutine force_loop


  subroutine velocity_loop(sel)
    integer :: i

    real(dp) :: masstime

    type(particle) :: sel(:) 

    !$omp parallel do private(i, masstime) shared(sel)
    do i = 1, size(sel)
      masstime = 0
      if( sel(i)%mass > 0 ) masstime = bc%dt/sel(i)%mass

      sel(i)%u = sel(i)%u + sel(i)%fx*masstime
      sel(i)%v = sel(i)%v + sel(i)%fy*masstime
      sel(i)%w = sel(i)%w + sel(i)%fz*masstime
    end do
    !$omp end parallel do

  end subroutine velocity_loop


  subroutine velocity_half_loop(sel)
    integer :: i

    real(dp) :: masstime

    type(particle) :: sel(:) 

    !$omp parallel do private(i, masstime) shared(sel)
    do i = 1, size(sel)
      masstime = 0
      if( sel(i)%mass > 0 ) masstime = .5*bc%dt/sel(i)%mass

      sel(i)%u = sel(i)%u + sel(i)%fx*masstime
      sel(i)%v = sel(i)%v + sel(i)%fy*masstime
      sel(i)%w = sel(i)%w + sel(i)%fz*masstime
    end do
    !$omp end parallel do

  end subroutine velocity_half_loop


  subroutine position_loop(sel)
    integer :: i

    type(particle) :: sel(:) 
    
    !$omp parallel do private(i) shared(sel)
    do i = 1, size(sel)
      sel(i)%x = sel(i)%x+sel(i)%u*bc%dt
      sel(i)%y = sel(i)%y+sel(i)%v*bc%dt
      sel(i)%z = sel(i)%z+sel(i)%w*bc%dt
    end do
    !$omp end parallel do

  end subroutine position_loop


end module openmp_loop

