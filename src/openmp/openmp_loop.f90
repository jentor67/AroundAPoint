!> \\file gravityModule.f95
module openmp_loop
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  use gravitymodule

  implicit none

  public :: force_loop
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

end module openmp_loop

