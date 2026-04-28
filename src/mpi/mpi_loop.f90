!> \\file gravityModule.f95
module mpi_loop
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  use gravitymodule
  use mpi
  use iso_c_binding

  implicit none

  public :: force_loop
contains

  subroutine force_loop(sel)
    type(particle) :: sel(:) 

    integer :: blocklengths(2)
    integer(kind=MPI_ADDRESS_KIND) :: displacements(2)
    integer :: types(2)
    integer :: mpi_particle_type
    integer :: ierr

    integer :: i
    real(dp) :: fxsum, fysum, fzsum
    type(particle) :: sel(:) 

    blocklengths = [3, 1]
    types = [MPI_REAL, MPI_INTEGER]
    
    call MPI_Get_address(sel%x, displacements(1), ierr)
    call MPI_Get_address(sel%id, displacements(2), ierr)
    
    ! Normalize displacements
    displacements = displacements - displacements(1)
    ! ****

    do i = 1, size(sel)
      call forcevectorloop(sel, i, size(sel), fxsum, fysum, fzsum)

      sel(i)%fx = fxsum
      sel(i)%fy = fysum
      sel(i)%fz = fzsum
    end do



  end subroutine force_loop

end module mpi_loop

