!> \\file gravityModule.f90
module gravitymodule
  use startparametersmodule
  use vectormodule
  use constantsmodule
  use readconfigmodule
  implicit none

  public :: acceleration, distance
  public :: valueLargeBody, forcevector, forcevectorloop
  public :: velocitychange, getpartparm, printparticles
  public :: collisionTest

  real(dp) :: mass 
  real(dp) :: centerMass !mass1 = 1000.0 ! = 1.989E30!; // kg


contains
  !  ##### NOT USED ##########
  subroutine collisionTest(sel, iteration)
    ! only works for 2 body collisions
    integer :: n_primary, n_test, iteration

    real(dp) :: dist_two_objects, p1(3), p2(3), vel1(3), vel2(3), m1, m2
    
    type(particle) sel(:)


    do n_primary = 1, size(sel)

      do n_test = n_primary+1, size(sel)

        if( size(sel) == 2  .and. n_test ==2 .and. n_primary == 1) then
           dist_two_objects = distance( sel(n_primary), sel(n_test) )

           if( dist_two_objects < min_radius ) then
             write(*,*) "min_dist", iteration, dist_two_objects
             min_radius = dist_two_objects
           end if

        end if

        if( n_primary /= n_test .and. &
              sel(n_primary)%mass > 0.0 .and. &
              sel(n_test)%mass > 0.0 ) then

          dist_two_objects = distance( sel(n_primary), sel(n_test) )

          !if( dis1 < (a%radius+b%radius) ) then
          if( dist_two_objects < &
                  ( sel(n_primary)%radius + sel(n_test)%radius ) ) then
            write(*,*) "Collision"
            
            p1 = [sel(n_test)%x, sel(n_test)%y, sel(n_test)%z]
            p2 = [sel(n_primary)%x, sel(n_primary)%y, sel(n_primary)%z]
            vel1 = [sel(n_test)%u, sel(n_test)%v, sel(n_test)%w]
            vel2 = [sel(n_primary)%u, sel(n_primary)%v, sel(n_primary)%w]
            m1 = sel(n_test)%mass 
            m2 = sel(n_primary)%mass
            
            call sphere_collision_3d(p1, p2, vel1, vel2, m1, m2)
            
            sel(n_test)%u = vel1(1)
            sel(n_test)%v = vel1(2)
            sel(n_test)%w = vel1(3)
            
            sel(n_primary)%u = vel2(1)
            sel(n_primary)%v = vel2(2)
            sel(n_primary)%w = vel2(3)
           
              
          end if       

        end if

      end do

    end do

  end subroutine collisionTest
  ! ########################################

  subroutine getpartparm(sel, cf)
    type(boundaryconditions) :: cf
    type(particle) sel

    real(dp) :: ra, rp
    real(dp) :: a, b, e, i, nue, omega, omegaBIG
    real(dp) :: T
    real(dp) :: mue = 0.0_dp


    ! test if Argument of Periapsis exists
    if( ieee_is_finite(cf%omega) ) then
            omega = cf%omega
    else
            omega = randomArgumentOfPeriapsis(cf%omega_min, &
                    cf%omega_max)
    end if

    ! test if Eccentricity exists
    if( ieee_is_finite(cf%e) ) then
            e = cf%e
    else
            e = randomEccentricity(cf%e_min, cf%e_max)
    end if

    ! test if Inclination exists
    if( ieee_is_finite(cf%i) ) then
            i = cf%i
    else
            i =  randomInclination(cf%i_min, cf%i_max)
    end if

    ! test if omegabig exists
    if( ieee_is_finite(cf%omegabig) ) then
            omegaBIG = cf%omegabig
    else
            omegaBIG = randomLongitudeOfAscendingNode( &
                    cf%omegabig_min, cf%omegabig_max)
    end if

    ! test if Object Mass exists
    if( ieee_is_finite(cf%ObjectMass) ) then
            sel%mass = cf%ObjectMass
    else
            sel%mass = randomMass(cf%ObjectMass_min, cf%ObjectMass_max) 
    end if

    sel%radius = ( (sel%mass/density_material)*(3.0_dp/4.0_dp)/pie )**(1.0_dp/3.0_dp)
    

    ! test if a exists
    if( ieee_is_finite(cf%a) ) then
            a = cf%a
    else
            a = randomSemiMajorAxis(cf%a_min, cf%a_max)
    end if

    ! test if nue exists
    if( ieee_is_finite(cf%nue) ) then
            nue = cf%nue
    else
            nue = randomTrueAnomaly(cf%nue_min, cf%nue_max)
    end if
    

    b = a*((1-(e**2))**.5)

    call radiusVelocity(rp, ra, T, sel, e, a, mue)


    call startPointVelocity(sel, a, e, i, mue, nue, omega, omegaBIG)

  end subroutine getpartparm

  subroutine positionchange(sel)
    type(particle) sel

    sel%x = sel%x+sel%u*bc%dt
    sel%y = sel%y+sel%v*bc%dt
    sel%z = sel%z+sel%w*bc%dt

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
    !mue = gcu*centerMass  !mue based on the centerMass or sun
    vmag = ( mue*( (2/r) - (1/a) ) )**.5 ! velocity magnitude

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
    masstime = bc%dt/sel%mass

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
    sel%radius = ( (sel%mass/density_material)*(3.0/4.0)/pie )**(1.0_dp/3.0_dp)
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
    real(dp) :: dis1, force, constant
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


  subroutine printparticles(iteration, sel, units, particles)
    integer :: units(:)
    integer :: n, particles, iteration

    type(particle) sel(particles)

    do n = 1, particles
      write(units(n),60) iteration, sel(n)%x, sel(n)%y, sel(n)%z, &
              sel(n)%u, sel(n)%v, sel(n)%w, sel(n)%radius
    end do

    60   format (i0, "|", e17.10, "|", e17.10, "|", e17.10, "|", &
            e17.10, "|", e17.10, "|", e17.10, "|", e17.10)

  end subroutine printparticles


  subroutine radiusVelocity(rp, ra, T, sel, e, a, mue)
    type(particle) sel
    real(dp) :: rp, ra, T
    real(dp) :: e, a, mue

    rp = (1-e)*a ! distance at perigee (m)

    ra = (1+e)*a ! distance at apogee (m)

    ! standard gravitational parameters
    mue = real(gcu*(centerMass+sel%mass),kind=dp) 

    T = 2 * pie * (( (a**3) /mue )**.5) ! Peroid

  end subroutine radiusVelocity


end module gravitymodule

