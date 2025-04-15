!> \\file gravityModule.f95
module gravityModule
  implicit none
  public :: typeTest1, printTest1, acceleration, distance

  real :: mass 
  real :: gcu = 6.674083E-11
  real :: pie = 3.1415926535897932384626
  real :: mass_earth = 5.972168E24 
  real :: radius_earth = 6371000

  type typeTest1
    integer :: xValue
    integer :: yValue
  end type typeTest1

contains

  subroutine printTest1(at)
    type(typeTest1) at

    write(*,*) 'Before ',at%xValue
    at%xValue = 3
    write(*,*) 'After ',at%xValue
  end subroutine printTest1

  function acceleration(m,r) result(grav) 
    real :: grav, m, r

    grav = gcu*m/(r**2)
  end function 

  function distance(x1,y1,z1,x2,y2,z2) result(r) 
    real :: x1, y1, z1, x2, y2, z2, r 

    r = ( (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2 )**.5
  end function 

end module gravityModule

