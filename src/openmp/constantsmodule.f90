!> \\file constantsModule.f95
module constantsmodule
  implicit none
  integer, parameter :: dp = kind(1.0d0)

  real(dp) :: gcu = 6.674083E-11
  real(dp) :: pie = 4.0_dp * ATAN(1.0_dp)
  real(dp) :: SOLARMASS = 1.989E30 !; // kg
  real(dp) :: density_material = 11340.0
  real(dp) :: elastic = 1  ! // elastic of the collision
  real(dp) :: min_radius = 1.0E30
contains

  subroutine john()
    integer :: i = 0

    i = i +1
  end subroutine john


end module constantsmodule
