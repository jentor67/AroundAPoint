!> \\file gravityModule.f95
module gravitymodule
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  implicit none

  public :: acceleration, distance
  public :: valueLargeBody, forcevector
  public :: velocitychange, getpartparm, printparticle, printparticles

  real :: mass 

  real(kind=kind(1.0d0)) :: timedisp = 1 !.000001
  real(kind=kind(1.0d0)) :: mass1 = 1000.0 ! = 1.989E30!; // kg
  !real(kind=kind(1.0d0)) :: mass1 = 1.989E30!; // kg

  type particle
    real(kind=kind(1.0d0)) :: x
    real(kind=kind(1.0d0)) :: y
    real(kind=kind(1.0d0)) :: z
    real(kind=kind(1.0d0)) :: u
    real(kind=kind(1.0d0)) :: v
    real(kind=kind(1.0d0)) :: w
    real :: omega
    real :: e
    real :: i
    real :: omegaBIG
    real(kind=kind(1.0d0)) :: mass
    real(kind=kind(1.0d0)) :: a
    real(kind=kind(1.0d0)) :: b
    real :: nue
    real :: mue
  end type particle


contains

  subroutine getpartparm(sel, cf)
    type(boundaryconditions) :: cf
    type(particle) sel

    real(kind=kind(1.0d0)) :: ra, rp
    real(kind=kind(1.0d0)) :: T

    ! ***** Earth parameters *****
    !sel%omega = 114.20783 ! randomArgumentOfPeriapsis()
    !sel%e = 0.0167086 ! randomEccentricity() 
    !sel%i = 7.155 ! randomInclination() 
    !sel%omegaBIG = -11.26064 ! randomLongitudeOfAscendingNode()
    !sel%mass = 5.97217E24 + 7.346E22  ! randomMass(4.0, 6.0)
    !sel%a = 1.49598023E11 ! randomSimiMajorAxis(.000005, .00001)
    !sel%b = sel%a*((1-(sel%e**2))**.5);
    !sel%nue = 357.5 !randomTrueAnomaly() 

    ! test if Argument of Periapsis is greater than -9999.9
    if( cf%omega > -9999.9 ) then
            sel%omega = cf%omega
    else
            sel%omega = randomArgumentOfPeriapsis(cf%omega_min, &
                    cf%omega_max)
    end if

    ! test if Eccentricity > -9999.9
    if( cf%e > -9999.9 ) then
            sel%e = cf%e
    else
            sel%e = randomEccentricity(cf%e_min, cf%e_max)
    end if

    ! test if Inclination > -9999.9
    if( cf%i > -9999.9 ) then
            sel%i = cf%i
    else
            sel%i =  randomInclination(cf%i_min, cf%i_max)
    end if

    ! test if Logitude of Ascending Node is > -9999.9
    if( cf%omegaBIG > -9999.9 ) then
            sel%omegaBIG = cf%omegaBIG
    else
            sel%omegaBIG = randomLongitudeOfAscendingNode( &
                    cf%omegabig_min, cf%omegabig_max)
    end if

    ! test if given a single mass
    if(cf%ObjectMass > -9999.9 ) then
            sel%mass = cf%ObjectMass
    else
            sel%mass = randomMass(cf%ObjectMass_min, cf%ObjectMass_max) 
    end if
 
    ! test if given a sigle SemiMajorAxis
    if( cf%a > -9999.9 ) then
            sel%a = cf%a
    else
            sel%a = randomSemiMajorAxis(cf%a_min, cf%a_max)
    end if

    ! test if given a single True Anomaly
    if( cf%nue > -9999.9) then
            sel%nue = cf%nue
    else
            sel%nue = randomTrueAnomaly(cf%nue_min, cf%nue_max)
    end if
    
    write(*,*) "Parameters: ",sel%omega, sel%e, sel%i, sel%omegaBIG, &
            sel%mass, sel%a, sel%nue

    sel%b = sel%a*((1-(sel%e**2))**.5);

    call radiusVelocity(sel%mass, sel%a, sel%e, &
            rp, ra, sel%mue, T)

    write(*,*) "After radiusVelocity: ",sel%mass, sel%a, sel%e, &
            rp, ra, sel%mue, T

    call startPointVelocity(sel,rp)
    write(*,*) "After startPointVelocity",sel%e,sel%u,sel%v,sel%w

  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*timedisp
    sel%y = sel%y+sel%v*timedisp
    sel%z = sel%z+sel%w*timedisp

  end subroutine positionchange

  subroutine startPointVelocity(sel,rp)
    type(particle) sel
    real(kind=kind(1.0d0)) :: rp, rho, xp, r;
    real(kind=kind(1.0d0)) :: xt, yt, zt, ut, vt, wt;

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


    call tangentVectorEllipse(xp, sel%y, sel%a,sel%b, sel%v, ut, vt)
    sel%u = ut
    sel%v = vt


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
    real(kind=kind(1.0d0)) :: fx, fy, fz, masstime
    type(particle) sel 

    masstime = timedisp/sel%mass

    sel%u = sel%u+fx*masstime
    sel%v = sel%v+fy*masstime
    sel%w = sel%w+fz*masstime
  end subroutine velocitychange


  subroutine valueLargeBody(sel,cf)
    type(boundaryconditions) :: cf
    type(particle) sel
    sel%x=0
    sel%y=0
    sel%z=0
    sel%u=0
    sel%v=0
    sel%w=0
    sel%mass=cf%CenterMass
  end subroutine valueLargeBody


  function acceleration(m,r) result(grav) 
    real(kind=kind(1.0d0)) :: grav, m, r

    grav = gcu*m/(r**2)
  end function 


  function distance(a, b) result(r) 
    type(particle) a, b
    real(kind=kind(1.0d0)) :: r  ! good

    !write(*,*) "TEST",b%x, r

    r = ( (b%x-a%x)**2 + (b%y-a%y)**2 + (b%z-a%z)**2 )**.5
    !write(*,*) "radius:",a%x, a%y, a%z, b%x, b%y, b%z, r
  end function


  subroutine forcevector(a, b, fx, fy, fz) 
    real(kind=kind(1.0d0)) :: fx, fy, fz, dis1,force, constant
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

    !10   format (e17.10,",",e17.10,",",e17.10,",")
    !30   format (e17.10,e17.10,e17.10)
    !40   format (e17.10," ",e17.10," ",e17.10)
    50   format (e17.10," ",e17.10," ",e17.10," ",e17.10," ",e17.10, &
            " ",e17.10)
    !20   format (e17.10,",",e17.10,",",e17.10)
  end subroutine printparticles

  subroutine printparticle(i, sel)
    integer :: i
    type(particle) sel

    write(*,*) "P ", i, " ", sel%x, sel%y, sel%z, sel%u, sel%v, sel%w, &
            sel%mass

  end subroutine printparticle


  subroutine radiusVelocity(m, a, e, rp, ra,mue, T)
    real :: e, mue  
    real(kind=kind(1.0d0)) :: m, a, rp, ra, T

    rp = (1-e)*a ! distance at perigee (m)
    ra = (1+e)*a ! distance at apogee (m)
    mue = real(gcu*(mass1+m),kind=4) ! standard gravitational parameters
    T = 2 * pie * (( (a**3) /mue )**.5) ! Peroid
    
  end subroutine radiusVelocity


end module gravitymodule

