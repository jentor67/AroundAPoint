!> \\file vectorModule.f95
module vectormodule
  use constantsmodule
  implicit none

  public rotate2D, unitVector, Vector, magnitude, sphere_collision_3d


contains

  function magnitude(x, y, z) result(mag)
    real(dp) :: x, y, z, mag

    mag = sqrt(x**2 + y**2 + z**2)

  end function magnitude

  subroutine rotate2D(x, y, angle, x0, y0)
    real(dp) :: angle
    real(dp) :: x, y, x0, y0

    !//  angle will be in degrees
    x0 = x*cos(angle/180*pie) - y*sin(angle/180*pie)
    y0 = x*sin(angle/180*pie) + y*cos(angle/180*pie)

  end subroutine rotate2D 


  subroutine Vector(x1,  y1,  z1,  x2, y2,  z2,  x,  y,  z)
    real(dp) :: x1,  y1,  z1,  x2, y2,  z2,  x,  y,  z
  
    x = x2 - x1
    y = y2 - y1
    z = z2 - z1
  end subroutine Vector  
 

  subroutine unitVector(vx, vy, vz, ux, uy, uz)
      real(dp) :: vx, vy, vz, ux, uy, uz
      real(dp) :: v

      v = ( (vx**2) + (vy**2) + (vz**2) )**.5 
      ux = vx/v
      uy = vy/v
      uz = vz/v

  end subroutine unitVector


  subroutine  sphere_collision_3d(x1, x2, u1, u2, m1, m2) 

    real(dp) :: m1, m2
    real(dp) :: x1(3), x2(3)
    real(dp) :: u1(3), u2(3)
    real(dp) :: v1(3), v2(3)
    real(dp) :: n(3)
    real(dp) :: relv(3)
    real(dp) :: dist
    real(dp) :: dotprod
    real(dp) :: vn(3)
    real(dp) :: j(3)

    !---------------------------------------
    ! Example data
    !---------------------------------------

    !m1 = 1.0
    !m2 = 100.0

    ! Sphere centers at impact
    !x1 = [1.0, 0.0, 0.0]
    !x2 = [0.0, 0.0, 0.0]

    ! Initial velocities
    !u1 = [-1.0, 0.0, 0.0]
    !u2 = [1.0, 0.0, 0.0]

    !---------------------------------------
    ! Compute collision normal
    !---------------------------------------

    n = x1 - x2

    dist = sqrt(sum(n**2))

    if (dist <= 0.0) then
        print *, 'Error: sphere centers coincide'
        stop
    end if

    n = n / dist

    !---------------------------------------
    ! Relative velocity
    !---------------------------------------

    relv = u1 - u2

    dotprod = sum(relv * n)

    !---------------------------------------
    ! Elastic collision
    !---------------------------------------
    vn = dot_product(u1-u2, n)
    
    j = -(1.0 + elastic) * vn / (1.0/m1 + 1.0/m2)

    v1 = u1 + (j/m1) * n
    v2 = u2 - (j/m2) * n

    !---------------------------------------
    ! Output
    !---------------------------------------

    print *
    print *, 'Collision normal n ='
    print '(3F10.4)', n

    print *
    print *, 'Initial velocity sphere 1:'
    print '(3F10.4)', u1

    print *, 'Initial velocity sphere 2:'
    print '(3F10.4)', u2

    print *
    print *, 'Final velocity sphere 1:'
    print '(3F10.4)', v1

    print *, 'Final velocity sphere 2:'
    print '(3F10.4)', v2

  end subroutine sphere_collision_3d

end module vectormodule
