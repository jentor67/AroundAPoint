!> \\file startParameters.f95
module startParameters
  implicit none
contains


  function randomEccentricity() result(e)
    real :: e
    call random_number(e)
  end function 


end module startParameters

