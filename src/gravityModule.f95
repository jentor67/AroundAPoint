!> \\file gravityModule.f95
module gravityModule
  use startParameters
  use vectorModule
  implicit none
  public :: acceleration, distance
  public :: valuetest, forcevector
  public :: velocitychange,getpartparm, printparticle, printparticles
  real :: mass 
  real*8 :: gcu = 6.674083E-11
  real*8 :: pie = 3.1415926535897932384626
  real*8 :: timedisp = 1 !.000001
  real*8 :: SOLARMASS = 1.989E30 !; // kg
  real*8 :: mass1 = 10.0 ! = 1.989E30!; // kg
  !real*8 :: mass1 = 1.989E30!; // kg

  type particle
    real*8 :: x
    real*8 :: y
    real*8 :: z
    real*8 :: u
    real*8 :: v
    real*8 :: w
    real :: omega
    real :: e
    real :: i
    real :: omegaBIG
    real*8 :: mass
    real*8 :: a
    real*8 :: b
    real :: nue
    real :: mue
  end type particle


contains

  subroutine getpartparm(sel)
    type(particle) sel

    real*8 :: xt, yt, zt, ut, vt, wt, r, ra, rp, b
    real*8 :: T

    ! ***** Earth parameters *****
    !sel%omega = 114.20783 ! randomArgumentOfPeriapsis()
    !sel%e = 0.0167086 ! randomEccentricity() 
    !sel%i = 7.155 ! randomInclination() 
    !sel%omegaBIG = -11.26064 ! randomLongitudeOfAscendingNode()
    !sel%mass = 5.97217E24 + 7.346E22  ! randomMass(4.0, 6.0)
    !sel%a = 1.49598023E11 ! randomSimiMajorAxis(.000005, .00001)
    !sel%b = sel%a*((1-(sel%e**2))**.5);
    !sel%nue = 357.5 !randomTrueAnomaly() 

    sel%omega = randomArgumentOfPeriapsis()
    sel%e = randomEccentricity() 
    sel%i =  randomInclination() 
    sel%omegaBIG = randomLongitudeOfAscendingNode()
    sel%mass = randomMass(.4, .6)
    sel%a = randomSimiMajorAxis(5.0, 10.0)
    sel%b = sel%a*((1-(sel%e**2))**.5);
    sel%nue = randomTrueAnomaly() 
    write(*,*) "Values",  sel%omega, sel%e, sel%i, sel%omegaBIG, &
            sel%mass, sel%a, sel%b, sel%nue

    call radiusVelocity(sel%mass, sel%a, sel%e, sel%i, sel%omegaBIG, &
            sel%omega, rp, ra, sel%mue, T)
    write(*,*) "After radiusVelocity", rp, ra, sel%mue, T

    call startPointVelocity(sel,rp)
    write(*,*) "After startPointVelocity",sel%u,sel%v,sel%w
    !write(*,*) "Velocity",a,e,nue,rp,omega,i,omegaBIG,mue,b,sel%x, &
    !sel%y, sel%z, sel%u, sel%v, sel%w
    !write(*,*) "Velocity",sel%x, sel%y, sel%z, sel%u, sel%v, sel%w

  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*timedisp
    sel%y = sel%y+sel%v*timedisp
    sel%z = sel%z+sel%w*timedisp

  end subroutine positionchange

  subroutine startPointVelocity(sel,rp)
    type(particle) sel
    real*8 :: rp, rho, xp, r;
    real*8 :: xt, yt, zt, ut, vt, wt;

    !// Rotate nue  degrees
    r = sel%a*(1-(sel%e**2))/(1+sel%e*cos(sel%nue/180*pie))
    sel%x = r*cos(pie*sel%nue/180)
    xp = sel%a-rp+sel%x
    sel%y = r*sin(pie*sel%nue/180)
    rho = 180*atan( sel%y/xp )/pie
    sel%z = 1.0

    if( xp < 0 ) rho = rho + 180

    if( ( xp > 0 ) .and. ( sel%y < 0 ) ) rho = rho +  360

    sel%v = ( (2*sel%mue/r) - (sel%mue/sel%a) )**.5

    write(*,*) "v", sel%v

    !write(*,*) "tangentVenctor",xp, sel%y, sel%a, sel%b, sel%v, ut, vt
    call tangentVectorEllipse(xp, sel%y, sel%a,sel%b, sel%v, ut, vt)
    !write(*,*) "tangentVenctor",xp, sel%y, sel%a, sel%b, sel%v, ut, vt
    sel%u = ut
    sel%v = vt
    write(*,*) "v", sel%v


    !// Rotate omega  degrees
    call rotate2D(sel%x, sel%y, sel%omega, xt, yt)
    sel%x = xt
    sel%y = yt

    call rotate2D(sel%u, sel%v, sel%omega, ut, vt)
    sel%u = ut
    sel%v = vt

    !// Rotate i degrees
    call rotate2D(sel%y, sel%z, sel%i, yt, zt)
    sel%y = yt
    sel%z = zt

    call rotate2D(sel%v, sel%w,  sel%i, vt, wt)
    sel%v = vt
    sel%w = wt

    !// Rotate OMEGA degrees
    call rotate2D(sel%x, sel%y, sel%OMEGA, xt, yt)
    sel%x = xt
    sel%y = yt


    call rotate2D(sel%u, sel%v, sel%OMEGA, ut, vt)
    sel%u = ut
    sel%v = vt


  end subroutine startPointVelocity


  subroutine velocitychange(sel, fx, fy, fz)
    real*8 :: fx, fy, fz, masstime
    type(particle) sel 

    masstime = timedisp/sel%mass

    !write(*,*) "Mass",sel%mass

    !write(*,*) "Speed Old:", sel%u, sel%v, sel%w, fx, fy, fz
    sel%u = sel%u+fx*masstime
    sel%v = sel%v+fy*masstime
    sel%w = sel%w+fz*masstime
    !write(*,*) "Speed new:", sel%u, sel%v, sel%w, fx, fy, fz
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
    real*8 :: grav, m, r

    grav = gcu*m/(r**2)
  end function 


  function distance(a, b) result(r) 
    type(particle) a, b
    real*8 :: r  ! good

    !write(*,*) "TEST",b%x, r

    r = ( (b%x-a%x)**2 + (b%y-a%y)**2 + (b%z-a%z)**2 )**.5
    !write(*,*) "radius:",a%x, a%y, a%z, b%x, b%y, b%z, r
  end function


  subroutine forcevector(a, b, fx, fy, fz) 
    real*8 :: fx, fy, fz, dis1,force, constant
    type(particle) a, b

    dis1 = distance(a,b)

    !write(*,*) gcu, a%mass, b%mass, dis1
    force = gcu*a%mass/dis1*b%mass/dis1 !*b%mass/(dis1**2)
    
    constant = force/dis1
    !write(*,*) "Corridinates:",a%x, a%y, a%z, b%x, b%y, b%z, dis1, &
    !force, constant
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
      write(units(n),50) sel(n)%x, sel(n)%y, sel(n)%z, &
              sel(n)%u, sel(n)%v, sel(n)%w
    end do

    10   format (e17.10,",",e17.10,",",e17.10,",")
    30   format (e17.10,e17.10,e17.10)
    40   format (e17.10," ",e17.10," ",e17.10)
    50   format (e17.10," ",e17.10," ",e17.10," ",e17.10," ",e17.10," ",e17.10)
    !20   format (e17.10,",",e17.10,",",e17.10)
  end subroutine printparticles

  subroutine printparticle(i, sel)
    integer :: i
    type(particle) sel
    !write(*,*) "P ", i, " ", sel%x, sel%y, sel%z, sel%u, sel%v, sel%w, sel%mass
  end subroutine printparticle


  subroutine radiusVelocity(m, a, e, i,omegaBIG, omega, rp, ra,mue, T )
    real :: e, i, omegaBIG, omega, mue  
    real*8 :: m, a, rp, ra, T

    rp = (1-e)*a ! distance at perigee (m)
    ra = (1+e)*a ! distance at apogee (m)
    mue = gcu*(mass1+m) ! standard gravitational parameters
    T = 2 * pi * (( (a**3) /mue )**.5) ! Peroid
    
  end subroutine radiusVelocity


end module gravityModule

