!> \\file gravityModule.f95
module gravityModule
  use startParameters
  implicit none
  public :: acceleration, distance
  public :: valuetest, forcevector
  public :: velocitychange,getpartparm, printparticle, printparticles
  real :: mass 
  real :: gcu = 6.674083E-11
  real :: pie = 3.1415926535897932384626
  real :: timedisp = .000001
 

  type particle
    real :: x
    real :: y
    real :: z
    real :: u
    real :: v
    real :: w
    real :: omega
    real :: e
    real :: i
    real :: omegaBIG
    real :: mass
    real :: a
    real :: nue
  end type particle


contains

  subroutine getpartparm(sel)
    type(particle) sel
    real :: factor
    real :: omega, e, i, omegaBIG, a, nue
    real :: xt, yt, zt, ut, vt, wt;
    factor = .00001
    call random_number(sel%u)
    call random_number(sel%v)
    call random_number(sel%w)
    call random_number(sel%x)
    call random_number(sel%y)
    call random_number(sel%z)
    omega = randomArgumentOfPeriapsis()
    e = randomEccentricity() 
    i = randomInclination() 
    omegaBIG = randomLongitudeOfAscendingNode()
    sel%mass = randomMass()
    a = randomSimiMajorAxis()
    nue = randomTrueAnomaly() 

    
    ! Rotate nue  degrees
    r = a*(1-pow(e,2))/(1+e*cos(nue/180*pi));
    sel%x = r*cos(pi*nue/180);
    xp = a-rp+sel%x;
    sel%y = r*sin(pi*nue/180);
    rho = 180*atan( sel%y/xp )/pi;
    sel%z = 1.0;

    if( xp < 0 ) rho += 180;

    if( ( xp > 0 ) && ( sel%y < 0 ) ) rho += 360; 

    sel%v = pow( (2*mue/r) - (mue/a), .5);
    
    tangentVectorEllipse(xp, sel%y, a, b, sel%v, &ut, &vt);
    sel%u = ut;
    sel%v = vt;

    ! Rotate omega  degrees
    rotate2D(sel%x, sel%y, omega, &xt, &yt);
    sel%x = xt;
    sel%y = yt;

    rotate2D(sel%u, sel%v, omega, &ut, &vt);
    sel%u = ut;
    sel%v = vt;

    ! Rotate i degrees
    rotate2D(sel%y, sle%z, i, &yt, &zt);
    sel%y = yt;
    sel%z = zt;
   
    rotate2D(sel%v, sel%w,  i, &vt, &wt);
    sel%v = vt;
    sel%w = wt; 

    ! Rotate OMEGA degrees
    rotate2D(sel%x, sel%y, OMEGA, &xt, &yt);
    sel%x = xt;
    sel%y = yt;


    rotate2D(sel%u, sel%v, OMEGA, &ut, &vt);
    sel%u = ut;
    sel%v = vt;




    sel%u = 0 !(sel%u-.5)*factor
    sel%v = 0 !(sel%v-.5)*factor
    sel%w = 0 !(sel%w-.5)*factor
    sel%x = (sel%x-.5)*factor
    sel%y = (sel%y-.5)*factor
    sel%z = (sel%z-.5)*factor
    sel%mass = sel%mass*factor
  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*timedisp
    sel%y = sel%y+sel%v*timedisp
    sel%z = sel%z+sel%w*timedisp

  end subroutine positionchange


  subroutine velocitychange(sel, fx, fy, fz)
    real :: fx, fy, fz, masstime
    type(particle) sel 

    masstime = timedisp/sel%mass

    sel%u = sel%u+fx*masstime
    sel%v = sel%v+fy*masstime
    sel%w = sel%w+fz*masstime
    !write(*,*) sel%u, sel1%u, fx,sel%v, sel1%v, fy, sel%w, sel1%w, fz
  end subroutine velocitychange


  subroutine valuetest(sel)
    type(particle) sel
    sel%x=0
    sel%y=0
    sel%z=0
    sel%u=0
    sel%v=0
    sel%w=0
    sel%mass=10.0
  end subroutine valuetest


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
    real :: fx, fy, fz, dis1, force, constant
    type(particle) a, b

    dis1 = distance(a,b)

    force = Gcu*a%mass*b%mass/(dis1**2)
    
    constant = force/dis1

    fx = constant*(b%x-a%x)
    fy = constant*(b%y-a%y)
    fz = constant*(b%z-a%z)
    
  end subroutine forceVector


  subroutine printparticles(sel, io, particles)
    integer :: io, n, particles
    type(particle) sel(particles)
    !write(*,*) sel(1)%x
    do n = 1, particles
      write(*,*) sel(n)%e
      write(io,40) sel(n)%x, sel(n)%y, sel(n)%z
    end do

    10   format (e17.10,",",e17.10,",",e17.10,",")
    30   format (e17.10,e17.10,e17.10)
    40   format (e17.10," ",e17.10," ",e17.10)
    !20   format (e17.10,",",e17.10,",",e17.10)
  end subroutine printparticles

  subroutine printparticle(i, sel)
    integer :: i
    type(particle) sel
    write(*,*) "P ", i, " ", sel%x, sel%y, sel%z, sel%u, sel%v, sel%w, sel%mass
  end subroutine printparticle


end module gravityModule

