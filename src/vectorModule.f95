!> \\file vectorModule.f95
module vectorModule
  implicit none
  public rotate2D, tangentVectorEllipse, unitVector, Vector

  real :: pi = 4.0 * ATAN(1.0)

contains

  subroutine rotate2D(x, y, angle, x0, y0)
    real :: angle
    real*8 :: x, y, x0, y0

    !//  angle will be in degrees
    x0 = x*cos(angle/180*pi) - y*sin(angle/180*pi)
    y0 = x*sin(angle/180*pi) + y*cos(angle/180*pi)

  end subroutine rotate2D 

  subroutine tangentVectorEllipse(x, y, a, b, v, vx, vy)
    real*8 :: x, y, a, b, v, vx, vy
    real*8 :: E, F, D, slope
    real*8 :: vx1, vy1, vz1
    real*8 :: vx0, vy0, vz0= 0.0
   
    write(*,*) "V tang", v 

    E = (b/a)**2
    F = ( (b**2) - E*(x**2) )**.5
    write(*,*) "F",b,E,x,(b-x)

    if( (x > 0) .and. (y == 0) ) then
        vx0 = 0.0;
        vy0 = 1.0;
    else if( (x < 0) .and. (y == 0) ) then
        vx0 = 0.0
        vy0 = -1.0
    else if( (x == 0) .and. (y > 0) ) then
        vx0 = -1.0
        vy0 = 0.0
    else if( (x == 0) .and. (y < 0) ) then
        vx0 = 1.0
        vy0 = 0.0
    else
        vx0 = -1.0
        if( y < 0 ) vx0 = 1.0
        vy0 = E*x/F
    end if
    write(*,*) "uvSub",vx0,vy0,vz0 
    call unitVector(vx0, vy0, vz0, vx1, vy1, vz1)
    write(*,*) "uv",vx1,vy1 
    vx = vx1*v
    vy = vy1*v

  end subroutine tangentVectorEllipse

  subroutine Vector(x1,  y1,  z1,  x2, y2,  z2,  x,  y,  z)
    real*8 :: x1,  y1,  z1,  x2, y2,  z2,  x,  y,  z
  
    x = x2 - x1
    y = y2 - y1
    z = z2 - z1
  end subroutine Vector  
  
  subroutine unitVector(vx, vy, vz, ux, uy, uz)
      real*8 :: vx, vy, vz, ux, uy, uz
      real*8 :: v

      v = ( (vx**2) + (vy**2) + (vz**2) )**.5 
      ux = vx/v
      uy = vy/v
      uz = vz/v

  end subroutine unitVector


end module vectorModule

