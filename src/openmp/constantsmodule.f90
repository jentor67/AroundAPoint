!> \\file constantsModule.f95
module constantsmodule
  implicit none
  integer, parameter :: dp = kind(1.0d0)

  real(dp), parameter :: gcu  = 6.674083e-11_dp
  real(dp), parameter :: pie  = 4.0_dp * atan(1.0_dp)
  real(dp), parameter :: SOLARMASS = 1.989E30_dp !; // kg
  real(dp), parameter :: density_material = 11340.0_dp
  !real(dp), parameter :: elastic = 1.0_dp  ! // elastic of the collision
  real(dp), parameter :: elastic = .99_dp  ! // elastic of the collision
  real(dp) :: min_radius = 1.0E30_dp
contains


end module constantsmodule
