!> \\file gravityModule.f95
module gravityModule
  use startParameters
  use vectorModule
  implicit none
  public :: acceleration, distance
  public :: valuetest, forcevector
  public :: velocitychange,getpartparm, printparticle, printparticles
  real :: mass 
  real :: gcu = 6.674083E-11
  real :: pie = 3.1415926535897932384626
  real :: timedisp = .000001
  real :: SOLARMASS = 1.989E30 !; // kg
  real :: mSagA = 1.989E30*4.31e6!; // kg
  real :: mass1 = 10 

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

    real :: omega, e, i, omegaBIG, a, nue
    real :: xt, yt, zt, ut, vt, wt
    real :: rho, b, mue, r, ra, rp, T
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
    sel%mass = randomMass(4.0, 6.0)
    a = randomSimiMajorAxis(.000005, .00001)
    b = a*((1-(e**2))**.5);
    nue = randomTrueAnomaly() 

    call radiusVelocity(sel%mass, a, e, i, omegaBIG, omega, rp, ra, mue, T)

    call startPointVelocity(a,e,nue,rp,omega,i,omegaBIG,mue,b, &
            sel%x, sel%y, sel%z, sel%u, sel%v, sel%w)

  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*timedisp
    sel%y = sel%y+sel%v*timedisp
    sel%z = sel%z+sel%w*timedisp

  end subroutine positionchange

  subroutine startPointVelocity(a,e,nue,rp,omega,i,omegaBIG,mue,b,x,y,z,u,v,w)
    real :: a,e,nue,rp,omega,i,omegaBIG,mue,b,x,y,z,u,v,w
    real :: r, rho, xp;
    real :: xt, yt, zt, ut, vt, wt;

    !// Rotate nue  degrees
    r = a*(1-(e**2))/(1+e*cos(nue/180*pi))
    x = r*cos(pi*nue/180)
    xp = a-rp+x
    y = r*sin(pi*nue/180)
    rho = 180*atan( y/xp )/pi
    z = 1.0

    if( xp < 0 ) rho = rho + 180

    if( ( xp > 0 ) .and. ( y < 0 ) ) rho = rho +  360

    v = ( (2*mue/r) - (mue/a) )**.5


    call tangentVectorEllipse(xp, y, a, b, v, ut, vt)
    !write(*,*) "tangetVenctor",ut, vt
    u = ut
    v = vt


    !// Rotate omega  degrees
    call rotate2D(x, y, omega, xt, yt)
    x = xt
    y = yt

    call rotate2D(u, v, omega, ut, vt)
    u = ut
    v = vt

    !// Rotate i degrees
    call rotate2D(y, z, i, yt, zt)
    y = yt
    z = zt

    call rotate2D(v, w,  i, vt, wt)
    v = vt
    w = wt

    !// Rotate OMEGA degrees
    call rotate2D(x, y, OMEGA, xt, yt)
    x = xt
    y = yt


    call rotate2D(u, v, OMEGA, ut, vt)
    u = ut
    v = vt


  end subroutine startPointVelocity


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
    sel%mass=mass1
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


  !subroutine printparticles(sel, io, particles)
  subroutine printparticles(sel, units, particles)
    integer, intent(inout) :: units(:)
    integer :: n, particles
    type(particle) sel(particles)
    
    !write(*,*) particles, sel(1)%x
    do n = 1, particles
      !write(*,*) sel(n)%e
      write(units(n),40) sel(n)%x, sel(n)%y, sel(n)%z
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


  subroutine radiusVelocity(m, a, e, i,omegaBIG, omega, rp, ra,mue, T )
    real :: m, a, e, i,omegaBIG, omega, rp, ra,mue, T 
    real :: tmue;
    rp = (1-e)*a ! distance at perigee (m)
    ra = (1+e)*a ! distance at apogee (m)
    tmue = gcu*(mass1+m) ! standard gravitational parameters
    T = 2 * pi * (( (a**3) /tmue )**.5) ! Peroid
    mue = tmue
  end subroutine radiusVelocity


end module gravityModule

