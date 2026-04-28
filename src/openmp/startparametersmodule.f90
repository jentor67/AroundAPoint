!> \\file startParameters.f95
module startparametersmodule
  use constantsmodule
  implicit none
  public :: randomArgumentOfPeriapsis
  public :: randomEccentricity 
  public :: randomInclination 
  public :: randomLongitudeOfAscendingNode 
  public :: randomMass 
  public :: randomSemiMajorAxis 
  public :: randomTrueAnomaly 
contains

  ! finds a random values between 0 and 360 degrees
  function randomArgumentOfPeriapsis(min, max) result(omega)
    real(dp) :: omega, min, max, diff

    diff = max - min

    call random_number(omega)

    omega = omega*diff
    omega = min + omega

  end function

  ! finds a randomn values between 0 and 1
  function randomEccentricity(min, max) result(e)
    real(dp) :: e, min, max, diff

    diff = max - min

    call random_number(e)

    e = e*diff
    e = min + e

  end function 

  ! finds a random values between 0 and 360 degrees
  function randomInclination(min, max) result(inclination)
    real(dp) :: inclination, min, max, diff

    diff = max - min

    call random_number(inclination)

    inclination = inclination*diff
    inclination = min + inclination

  end function

  ! finds a random values between 0 and 360 degrees
  function randomLongitudeOfAscendingNode(min, max) result(OMEGA)
    real(dp) :: OMEGA, min, max, diff
    
    diff = max - min

    call random_number(OMEGA)
    
    OMEGA = OMEGA*diff
    OMEGA = min+OMEGA

  end function

  ! determine the mass
  function randomMass(min, max) result(m)
    real(dp) :: m, min, max, diff

    diff = max - min

    call random_number(m)

    m = m*diff
    m = min+m

  end function 

  !  determine the semi-major axis
  function randomSemiMajorAxis(min, max) result(a)
    real(dp) :: a, min, max, diff
    
    diff = max - min

    call random_number(a)

    a = a*diff
    a = min+a

  end function

  ! finds a random values between 0 and 360 degrees
  function randomTrueAnomaly(min, max) result(nue)
    real(dp) :: nue, min, max, diff

    diff = max - min

    call random_number(nue)

    nue = nue*diff
    nue = min+nue

  end function

end module startparametersmodule
