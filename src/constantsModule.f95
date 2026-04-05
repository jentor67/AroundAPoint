!> \\file constantsModule.f95
module constantsModule
  implicit none

  !real*8 :: gcu = 6.674083E-11
  real(kind=kind(1.0d0)) :: gcu = 6.674083E-11
  real(kind=kind(1.0d0)) :: pie = 4.0 * ATAN(1.0) !3.1415926535897932384626
  real(kind=kind(1.0d0)) :: SOLARMASS = 1.989E30 !; // kg

contains

  function john() result(t)
    integer :: t
    t=3
  end function john
end module constantsModule
