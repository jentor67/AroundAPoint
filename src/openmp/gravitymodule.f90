!> \\file gravityModule.f95
module gravitymodule
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  implicit none

  public :: acceleration, distance
  public :: valueLargeBody, forcevector, forcevectorloop
  public :: velocitychange, getpartparm, printparticle, printparticles
  public :: collisionTest

  real(dp) :: mass 
  real(dp) :: timedisp = 1 !.000001
  real(dp) :: centerMass !mass1 = 1000.0 ! = 1.989E30!; // kg

  type particle
    real(dp) :: x
    real(dp) :: y
    real(dp) :: z
    real(dp) :: u
    real(dp) :: v
    real(dp) :: w
    real(dp) :: fx
    real(dp) :: fy
    real(dp) :: fz
    !real(dp) :: omega
    !real(dp) :: e
    !real(dp) :: i
    !real(dp) :: omegaBIG
    real(dp) :: radius
    real(dp) :: mass
    !real(dp) :: a
    !real(dp) :: b
    !real(dp) :: nue
    !real(dp) :: mue
  end type particle


contains

  subroutine collisionTest(sel,n_particals)
    integer :: n_primary, n_test, n_particals

    real(dp) :: dist_two_objects
    
    type(particle) sel(n_particals)


    do n_primary = 1, n_particals

      do n_test = 1, n_particals

        if( n_primary /= n_test .and. &
              sel(n_primary)%mass > 0.0 .and. &
              sel(n_test)%mass > 0.0 ) then

          dist_two_objects = distance( sel(n_primary), sel(n_test) )

          !if( dis1 < (a%radius+b%radius) ) then
          if( dist_two_objects < &
                  ( sel(n_primary)%radius + sel(n_test)%radius ) ) then
            write(*,*) "Collision"
            ! determine the largest object
            if( sel(n_primary)%mass >= sel(n_test)%mass ) then
              sel(n_primary)%mass = sel(n_primary)%mass + &
                      sel(n_test)%mass
              sel(n_test)%mass = -1
              sel(n_test)%x = -10000
              sel(n_test)%y = -10000
              sel(n_test)%z = -10000
            else
              sel(n_test)%mass = sel(n_primary)%mass + &
                      sel(n_test)%mass
              sel(n_primary)%mass = -1
              sel(n_primary)%x = -10000
              sel(n_primary)%y = -10000
              sel(n_primary)%z = -10000
            end if


          end if       

        end if

      end do

    end do

  end subroutine collisionTest

  subroutine getpartparm(sel, cf)
    type(boundaryconditions) :: cf
    type(particle) sel

    real(dp) :: ra, rp
    real(dp) :: a, b, e, i, nue, mue, omega, omegaBIG
    real(dp) :: T


    ! test if Argument of Periapsis is greater than -9999.9
    if( cf%omega > -9999.9 ) then
            omega = cf%omega
    else
            omega = randomArgumentOfPeriapsis(cf%omega_min, &
                    cf%omega_max)
    end if

    ! test if Eccentricity > -9999.9
    if( cf%e > -9999.9 ) then
            e = cf%e
    else
            e = randomEccentricity(cf%e_min, cf%e_max)
    end if

    ! test if Inclination > -9999.9
    if( cf%i > -9999.9 ) then
            i = cf%i
    else
            i =  randomInclination(cf%i_min, cf%i_max)
    end if

    ! test if Logitude of Ascending Node is > -9999.9
    if( cf%omegaBIG > -9999.9 ) then
            omegaBIG = cf%omegaBIG
    else
            omegaBIG = randomLongitudeOfAscendingNode( &
                    cf%omegabig_min, cf%omegabig_max)
    end if

    ! test if given a single mass
    if(cf%ObjectMass > -9999.9 ) then
            sel%mass = cf%ObjectMass
    else
            sel%mass = randomMass(cf%ObjectMass_min, cf%ObjectMass_max) 
    end if

    sel%radius = ( (sel%mass/density_material)*(3.0/4.0)/pie )**(0.3333)
    
    !write(*,*) sel%radius, sel%mass, density_material, pie

    ! test if given a sigle SemiMajorAxis
    if( cf%a > -9999.9 ) then
            a = cf%a
    else
            a = randomSemiMajorAxis(cf%a_min, cf%a_max)
    end if

    ! test if given a single True Anomaly
    if( cf%nue > -9999.9) then
            nue = cf%nue
    else
            nue = randomTrueAnomaly(cf%nue_min, cf%nue_max)
    end if
    
    !write(*,*) "i: ", i, "  omegaBig: ",  omegaBIG, &
    ! "  omega: ",omega, "  nue: ", nue, "  e: ",e, &
    ! "  a: ",a, "  mass: ", sel%mass

    b = a*((1-(e**2))**.5)

    call radiusVelocity(rp, ra, T, sel, a, e, mue)

    !write(*,*) "After radiusVelocity: ", rp, ra, mue, T

    call startPointVelocity(sel, a, e, i, mue, nue, omega, omegaBIG)
    !write(*,*) "u: ",sel%u, " v: ", sel%v, " w: ",sel%w

  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*timedisp
    sel%y = sel%y+sel%v*timedisp
    sel%z = sel%z+sel%w*timedisp

  end subroutine positionchange

  subroutine startPointVelocity(sel, a, e, i, mue, nue, omega, omegaBIG)
    type(particle) sel
    real(dp) :: r, nue_radians, vr, v0
    real(dp) :: con1
    real(dp) :: vmag  ! velocity magnitue
    !real(dp) :: mue  ! G*M(centerMass)
    real(dp) :: xt, yt, zt, ut, vt, wt;
    real(dp) :: a, e, i, mue, nue, omega, omegaBIG

    ! *** Rotate nue  degrees ***
    nue_radians = pie*nue/180

    ! distance from the focus(where the star is)
    r = a*( 1- (e**2) ) / (1 + e * cos( nue_radians) )

    !  x y location
    sel%x = r*cos(nue_radians)
    sel%y = r*sin(nue_radians)
    sel%z = 0

    ! ***determine the velocity at the true anomaly sel%nue****
    vmag = ( mue*( (2/r) - (1/a) ) )**.5 ! velocity magnitude
    mue = gcu*centerMass  !mue based on the centerMass or sun

    ! constant
    con1 = (  mue/( a*(1-(e**2)) ) )**.5

    !Radial component( toward/away from the star
    vr = con1*e*sin(nue_radians) 
    
    !Transverse component(sideways, along the orbit
    v0 = con1*( 1 + e*cos(nue_radians) )

    sel%u = vr*cos(nue_radians) - v0*sin(nue_radians)
    sel%v = vr*sin(nue_radians) + v0*cos(nue_radians)
    sel%w = 0
    ! *****************************


    !// Rotate omega  degrees
    ! position
    call rotate2D(sel%x, sel%y, omega, xt, yt)
    sel%x = xt
    sel%y = yt

    ! velocity
    call rotate2D(sel%u, sel%v, omega, ut, vt)
    sel%u = ut
    sel%v = vt

    !// Rotate i degrees
    ! position
    call rotate2D(sel%y, sel%z, i, yt, zt)
    sel%y = yt
    sel%z = zt

    ! velocity
    call rotate2D(sel%v, sel%w, i, vt, wt)
    sel%v = vt
    sel%w = wt

    !// Rotate OMEGA degrees
    ! position
    call rotate2D(sel%x, sel%y, omegaBIG, xt, yt)
    sel%x = xt
    sel%y = yt

    ! velocity
    call rotate2D(sel%u, sel%v, omegaBIG, ut, vt)
    sel%u = ut
    sel%v = vt


  end subroutine startPointVelocity


  subroutine velocitychange(sel)
    real(dp) :: masstime
    type(particle) sel 

    ! f=ma --> a=f/m
    !dv = a*dt --> f/m*dt
    masstime = timedisp/sel%mass

    sel%u = sel%u + sel%fx*masstime
    sel%v = sel%v + sel%fy*masstime
    sel%w = sel%w + sel%fz*masstime

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
    real(dp) :: grav, m, r

    grav = gcu*m/(r**2)
  end function 


  function distance(a, b) result(r) 
    type(particle) :: a, b
    real(dp) :: r  ! good

    r = ( (b%x-a%x)**2 + (b%y-a%y)**2 + (b%z-a%z)**2 )**.5
  end function


  subroutine forcevector(a, b, fx, fy, fz) 
    real(dp) :: fx, fy, fz
    real(dp) dis1,force, constant
    type(particle) :: a, b

    dis1 = distance(a,b)

    force = gcu*a%mass*b%mass/(dis1**2)
    
    constant = force/dis1

    fx = constant*(b%x-a%x)
    fy = constant*(b%y-a%y)
    fz = constant*(b%z-a%z)
    
  end subroutine forceVector


  subroutine forcevectorloop(sel, itest, n_particles, &
                  fxsum, fysum, fzsum)
    integer :: k
    integer :: itest, n_particles
    type(particle) ::  sel(n_particles)
    real(dp) :: fx, fy, fz
    real(dp) :: fxsum, fysum, fzsum

    fxsum = 0
    fysum = 0
    fzsum = 0

    if( sel(itest)%mass > 0.0 ) then

      do k = 1, n_particles

        if( k /= itest .and. sel(k)%mass > 0.0) then
          call forcevector(sel(itest),sel(k), fx, fy, fz)
          fxsum = fxsum + fx
          fysum = fysum + fy
          fzsum = fzsum + fz
        end if

      end do

    end if

  end subroutine forcevectorloop


  subroutine printparticles(iteration, sel, units, particles, b_blender)
    logical :: b_blender

    integer :: units(:)
    integer :: n, particles, iteration
    integer :: blender_factor 

    type(particle) sel(particles)
  

    blender_factor = 1 ! default
   
    ! test if this is blender file 
    if( b_blender ) then
      blender_factor = 1
      !blender_factor = 1000000000
    end if

    do n = 1, particles

      !if( sel(n)%mass > 0.0 ) then

        write(units(n),60) iteration, &
          sel(n)%x/blender_factor, sel(n)%y/blender_factor, &
          sel(n)%z/blender_factor, sel(n)%u, sel(n)%v, sel(n)%w

      !end if

    end do

    !10   format (e17.10,",",e17.10,",",e17.10,",")
    !30   format (e17.10,e17.10,e17.10)
    !40   format (e17.10," ",e17.10," ",e17.10)
    !50   format (e17.10," ",e17.10," ",e17.10," ",e17.10," ",e17.10, &
    !        " ",e17.10)
    60   format (i0, "|", e17.10, "|", e17.10, "|", e17.10, "|", &
            e17.10, "|", e17.10, "|", e17.10)
    !20   format (e17.10,",",e17.10,",",e17.10)
  end subroutine printparticles

  subroutine printparticle(i, sel)
    integer :: i
    type(particle) sel
    real(dp) :: r, v
   
    r =  magnitude(sel%x, sel%y, sel%z) 
    v = magnitude(sel%u, sel%v, sel%w)

    !write(*,*) "P ", i, " ", sel%x, sel%y, sel%z, r, &
    !        sel%u, sel%v, sel%w, v, & 
    !        "mass: ", sel%mass, " radius: ",sel%radius
    !!        sel%mass

  end subroutine printparticle


  subroutine radiusVelocity(rp, ra, T, sel, e, a, mue)
    type(particle) sel
    real(dp) :: rp, ra, T
    real(dp) :: e, a, mue

    rp = (1-e)*a ! distance at perigee (m)

    ra = (1+e)*a ! distance at apogee (m)

    ! standard gravitational parameters
    mue = real(gcu*(centerMass+sel%mass),kind=4) 

    T = 2 * pie * (( (a**3) /mue )**.5) ! Peroid

  end subroutine radiusVelocity


end module gravitymodule

