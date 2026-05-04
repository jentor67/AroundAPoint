!> \\file gravityModule.f95
module openmp_loop
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  use gravitymodule

  implicit none

  public :: force_loop, velocity_loop, position_loop
contains

  subroutine force_loop(sel)
    integer :: i
    real(dp) :: fxsum, fysum, fzsum
    type(particle) :: sel(:) 

    ! Parallel region with work-sharing DO loop
    !$omp parallel do private(i, fxsum, fysum, fzsum) shared(sel)
    do i = 1, size(sel)
      fxsum = 0.0_dp
      fysum = 0.0_dp
      fzsum = 0.0_dp
      call forcevectorloop(sel, i, size(sel), fxsum, fysum, fzsum)
      sel(i)%fx = fxsum
      sel(i)%fy = fysum
      sel(i)%fz = fzsum
    end do
    !$omp end parallel do


  end subroutine force_loop


  subroutine velocity_loop(sel)
    integer :: i
    real(dp) :: masstime
    !velocitychange(partarray(m))
    type(particle) :: sel(:) 

    !$omp parallel do private(i, masstime) shared(sel)
    do i = 1, size(sel)
      masstime = timedisp/sel(i)%mass

      sel(i)%u = sel(i)%u + sel(i)%fx*masstime
      sel(i)%v = sel(i)%v + sel(i)%fy*masstime
      sel(i)%w = sel(i)%w + sel(i)%fz*masstime
    end do
    !$omp end parallel do

  end subroutine velocity_loop


  subroutine position_loop(sel)
    integer :: i
    !positionchange(partarray(m))
    type(particle) :: sel(:) 
    
    !$omp parallel do private(i) shared(sel)
    do i = 1, size(sel)
      sel(i)%x = sel(i)%x+sel(i)%u*timedisp
      sel(i)%y = sel(i)%y+sel(i)%v*timedisp
      sel(i)%z = sel(i)%z+sel(i)%w*timedisp
    end do
    !$omp end parallel do

  end subroutine position_loop


end module openmp_loop

