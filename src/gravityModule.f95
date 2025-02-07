!> \\file gravityModule.f95
module gravityModule
  implicit none
  private
  public :: typeTest1, printTest1, acceleration

  real :: m, G = 6.674083E-11

  type typeTest1
    integer :: xValue
  end type typeTest1

contains
  subroutine printTest1(at)
    type(typeTest1) at

    write(*,*) at%xValue
    at%xValue = 3
    write(*,*) at%xValue
  end subroutine printTest1

  function acceleration(m,r) result(a) 
    real :: a, m, r

    a = G*m/(r**2)
  end function 

end module gravityModule

module testModule
  implicit none
  private
  public :: typeTest1, printTest1, acceleration

  real :: m, G = 6.674083E-11

  type typeTest1
    integer :: xValue
  end type typeTest1

contains
  subroutine printTest1(at)
    type(typeTest1) at

    write(*,*) at%xValue
    at%xValue = 3
    write(*,*) at%xValue
  end subroutine printTest1

  function acceleration(m,r) result(a) 
    real :: a, m, r

    a = G*m/(r**2)
  end function 

end module testModule

       

