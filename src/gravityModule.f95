!> \\file gravityModule.f95
module gravityModule
  implicit none
  public :: typeTest1, printTest1, acceleration, distance
  public :: valuessun, forcevector
  public :: velocitychange,getpartparm, printparticle
  real :: mass 
  real :: gcu = 6.674083E-11
  real :: pie = 3.1415926535897932384626
  real :: mass_earth = 5.972168E24 
  real :: radius_earth = 6371000
  real :: timedisp = .000001
 

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

  subroutine getpartparm(sel)
    type(particle) sel
    real :: factor
    factor = .00001
    call random_number(sel%u)
    call random_number(sel%v)
    call random_number(sel%w)
    call random_number(sel%x)
    call random_number(sel%y)
    call random_number(sel%z)
    call random_number(sel%mass)

    sel%u = sel%u*factor
    sel%v = sel%v*factor
    sel%w = sel%w*factor
    sel%x = sel%x*factor
    sel%y = sel%y*factor
    sel%z = sel%z*factor
  end subroutine getpartparm


  subroutine positionchange(sel, sel1)
    type(particle) sel, sel1

    sel1%x = sel%x+sel%u*timedisp
    sel1%y = sel%y+sel%v*timedisp
    sel1%z = sel%z+sel%w*timedisp

  end subroutine positionchange
 
  subroutine velocitychange(sel, sel1, fx, fy, fz)
    real :: fx, fy, fz, masstime
    type(particle) sel, sel1

    masstime = timedisp/sel%mass

    sel1%u = sel%u+fx*masstime
    sel1%v = sel%v+fy*masstime
    sel1%w = sel%w+fz*masstime
    !write(*,*) sel%u, sel1%u, fx,sel%v, sel1%v, fy, sel%w, sel1%w, fz
  end subroutine velocitychange


  subroutine valuessun(sel)
    type(particle) sel
    sel%x=0
    sel%y=0
    sel%z=0
    sel%u=0
    sel%v=0
    sel%w=0
    sel%mass=10000000.0
  end subroutine valuessun

  subroutine printTest1(at)
    type(typeTest1) at

    at%xValue = 3
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


  subroutine forcevector(a, b, fx, fy, fz) 
    real :: fx, fy, fz, dis1, force
    type(particle) a, b

    dis1 = distance(a,b)

    force = Gcu*a%mass*b%mass/(dis1**2)

    fx = force*(b%x-a%x)/dis1
    fy = force*(b%y-a%y)/dis1
    fz = force*(b%z-a%z)/dis1
    
  end subroutine forceVector


  subroutine printparticle(i, sel)
    integer :: i
    type(particle) sel
    write(*,*) "P ", i, " ", sel%x, sel%y, sel%z, sel%u, sel%v, sel%w, sel%mass
  end subroutine printparticle


end module gravityModule

