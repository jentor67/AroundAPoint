!> \\file gravityModule.f95
module gravityModule
  implicit none
  public :: typeTest1, printTest1, acceleration, distance
  public :: valuessun, valuesearth, valuesjupiter, forcevector
  real :: mass 
  real :: gcu = 6.674083E-11
  real :: pie = 3.1415926535897932384626
  real :: mass_earth = 5.972168E24 
  real :: radius_earth = 6371000

  type particle
    real :: x
    real :: y
    real :: z
    real :: u
    real :: v
    real :: w
    real :: mass
  end type particle

  type typeTest1
    integer :: xValue
    integer :: yValue
  end type typeTest1

contains

  subroutine valuesearth(sel)
    type(particle) sel
    sel%x=0
    sel%y=5
    sel%z=0
    sel%u=1
    sel%v=0
    sel%w=.5
    sel%mass=.001
  end subroutine valuesearth

  subroutine valuesjupiter(sel)
    type(particle) sel
    sel%x=10
    sel%y=0
    sel%z=4
    sel%u=0
    sel%v=.01
    sel%w=.01
    sel%mass=1
  end subroutine valuesjupiter

  subroutine valuessun(sel)
    type(particle) sel
    sel%x=0
    sel%y=0
    sel%z=0
    sel%u=0
    sel%v=0
    sel%w=0
    sel%mass=10
  end subroutine valuessun

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

  function distance(a, b) result(r) 
    type(particle) a, b
    real :: r

    r = ( (b%x-a%x)**2 + (b%y-a%y)**2 + (b%z-a%z)**2 )**.5
  end function 

  subroutine forceVector(a, b, fx, fy, fz) 
    real :: fx, fy, fz, dis1, force
    type(particle) a, b

    dis1 = distance(a,b)

    force = Gcu*a%m*b%m/(dis1**2)

    fx = force*(b%x-a%x)/dis1
    fy = force*(b%y-a%y)/dis1
    fz = force*(b%z-a%z)/dis1
    
  end subroutine forceVector

end module gravityModule

