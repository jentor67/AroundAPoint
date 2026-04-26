!> \\file vectorModule.f95
module vectormodule
  use constantsmodule
  implicit none

  public rotate2D,  unitVector, Vector, magnitude


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


end module vectormodule

