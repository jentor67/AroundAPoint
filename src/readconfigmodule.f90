!> \\file readconfigmodule.f95
module readconfigmodule
  implicit none
  
  public :: read_config_file

  type boundaryconditions
    real(kind=kind(1.0d0)) :: a
    real(kind=kind(1.0d0)) :: a_min
    real(kind=kind(1.0d0)) :: a_max
    real(kind=kind(1.0d0)) :: CenterMass
    real(kind=kind(1.0d0)) :: ObjectMass
    real(kind=kind(1.0d0)) :: ObjectMass_min
    real(kind=kind(1.0d0)) :: ObjectMass_max
    
    real :: e
    real :: e_min
    real :: e_max
    real :: i
    real :: i_min
    real :: i_max
    real :: nue
    real :: nue_min
    real :: nue_max
    real :: omega
    real :: omega_min
    real :: omega_max
    real :: omegabig
    real :: omegabig_min
    real :: omegabig_max

    integer :: Iterations
    integer :: ObjectCount
  end type boundaryconditions
  
 
  type(boundaryconditions) :: bc
contains

  subroutine  read_config_file(filepath)
    implicit none
    integer :: unit, ios
    character(len=256) ::filepath
    character(len=256) ::attribute
    real(kind=kind(1.0d0)) :: attribute_value

    open(newunit=unit, file=filepath, status="old", action="read")

    bc%a = -10000.0
    bc%ObjectMass = -10000.0
    bc%e = -10000.0
    bc%i = -10000.0
    bc%nue = -10000.0
    bc%omega = -10000.0
    bc%omegabig = -10000.0

    do
      read(unit, *, iostat=ios) attribute, attribute_value
      if (ios /= 0) exit

      select case (attribute)
        case ("a")
          bc%a = attribute_value
          
        case ("a_min")
          bc%a_min = attribute_value

        case ("a_max")
          bc%a_max = attribute_value

        case ("CenterMass")
          bc%CenterMass = attribute_value
        
        case ("e")
          bc%e = real(attribute_value,kind=4)

        case ("e_min")
          bc%e_min = real(attribute_value,kind=4)

        case ("e_max")
          bc%e_max = real(attribute_value,kind=4)

        case ("i")
          bc%i = real(attribute_value,kind=4)

        case ("i_min")
          bc%i_min = real(attribute_value,kind=4)

        case ("i_max")
          bc%i_max = real(attribute_value,kind=4)

        case ("Iterations")
          bc%Iterations = int(attribute_value)

        case ("nue")
          bc%nue = real(attribute_value,kind=4)

        case ("nue_min")
          bc%nue_min = real(attribute_value,kind=4)

        case ("nue_max")
          bc%nue_max = real(attribute_value,kind=4)

        case ("ObjectCount")
          bc%ObjectCount = int(attribute_value)

        case ("ObjectMass")
          bc%ObjectMass = attribute_value

        case ("ObjectMass_min")
          bc%ObjectMass_min = attribute_value

        case ("ObjectMass_max")
          bc%ObjectMass_max = attribute_value

        case ("omega")
          bc%omega = real(attribute_value,kind=4)

        case ("omega_min")
          bc%omega_min = real(attribute_value,kind=4)

        case ("omega_max")
          bc%omega_max = real(attribute_value,kind=4)

        case ("omegaBig")
          bc%omegabig = real(attribute_value,kind=4)

        case ("omegaBig_min")
          bc%omegabig_min = real(attribute_value,kind=4)

        case ("omegaBig_max")
          bc%omegabig_max = real(attribute_value,kind=4)

        case default
          print *, "Unknown command --> ", attribute, attribute_value

      end select

    end do

    close(unit)
    
    write(*,*) bc%CenterMass, bc%Iterations, bc%ObjectMass_min, &
          bc%ObjectMass_max, bc%ObjectCount
  
  end subroutine read_config_file

end module readconfigmodule
