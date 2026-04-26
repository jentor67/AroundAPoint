!> \\file gravityModule.f95
module mpi_loop
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

    do i = 1, size(sel)
      call forcevectorloop(sel, i, size(sel), fxsum, fysum, fzsum)

      sel(i)%fx = fxsum
      sel(i)%fy = fysum
      sel(i)%fz = fzsum
    end do


  end subroutine force_loop

end module mpi_loop

