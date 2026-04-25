!> \\file constantsModule.f95
module constantsmodule
  implicit none
  integer, parameter :: dp = kind(1.0d0)

  real(dp) :: gcu = 6.674083E-11
  real(dp) :: pie = 4.0 * ATAN(1.0) !3.1415926535897932384626
  real(dp) :: SOLARMASS = 1.989E30 !; // kg
  real(dp) :: density_material = 11340.0
  !real(kind=kind(1.0d0)) :: density_material = 11340.0
contains

  function john() result(t)
    integer :: t
    t=3
  end function john

end module constantsmodule
