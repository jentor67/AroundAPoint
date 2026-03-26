!> \\file startParameters.f95
module startParameters
  implicit none
  public :: randomArgumentOfPeriapsis
  public :: randomEccentricity 
  public :: randomInclination 
  public :: randomLongitudeOfAscendingNode 
  public :: randomMass 
  public :: randomSimiMajorAxis 
  public :: randomTrueAnomaly 
contains

  ! finds a random values between 0 and 360 degrees
  function randomArgumentOfPeriapsis() result(omega)
    real :: omega
    call random_number(omega)
    omega = omega*360
  end function

  function randomEccentricity() result(e)
    real :: e
    call random_number(e)
  end function 

  ! finds a random values between 0 and 360 degrees
  function randomInclination() result(inclination)
    real :: inclination
    call random_number(inclination)
    inclination = inclination*360
  end function

  ! finds a random values between 0 and 360 degrees
  function randomLongitudeOfAscendingNode() result(OMEGA)
    real :: OMEGA
    call random_number(OMEGA)
    OMEGA = OMEGA*360
  end function

  ! determine the mass
  function randomMass(min, max) result(m)
    real :: m, min, max, diff

    diff = max - min

    call random_number(m)

    m = m*diff
    m = min+m

  end function 

  !  determine the semi-major axis
  function randomSimiMajorAxis(min, max) result(a)
    real :: a
    real :: diff, max, min
    
    diff = max - min

    call random_number(a)

    a = a*diff
    a = min+a

  end function

  ! finds a random values between 0 and 360 degrees
  function randomTrueAnomaly() result(nue)
    real :: nue

    call random_number(nue)

    nue = nue*360

  end function

end module startParameters

