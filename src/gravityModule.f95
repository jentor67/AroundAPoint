!> \\file gravityModule.f95
module gravityModule
  implicit none

  real :: G = 6.674083E-11

contains
  
  function acceleration(this) result(a)
    real :: a

    a = G*m/(r**2)
  end function acceleration


end module gravityModule

