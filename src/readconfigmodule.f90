!> \\file readconfigmodule.f95
module readconfigmodule
  implicit none
  
  public :: read_config_file

  type boundaryconditions
    real(kind=kind(1.0d0)) :: a_min
    real(kind=kind(1.0d0)) :: a_max
    real(kind=kind(1.0d0)) :: CenterMass
    real(kind=kind(1.0d0)) :: ObjectMass_min
    real(kind=kind(1.0d0)) :: ObjectMass_max
    
    real :: e_min
    real :: e_max
    real :: i_min
    real :: i_max
    real :: nue_min
    real :: nue_max
    real :: omega_min
    real :: omega_max
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

    do
      read(unit, *, iostat=ios) attribute, attribute_value
      if (ios /= 0) exit

      select case (attribute)
        case ("a_min")
          bc%a_min = attribute_value

        case ("a_max")
          bc%a_max = attribute_value

        case ("CenterMass")
          bc%CenterMass = attribute_value

        case ("e_min")
          bc%e_min = real(attribute_value,kind=4)

        case ("e_max")
          bc%e_max = real(attribute_value,kind=4)

        case ("i_min")
          bc%i_min = real(attribute_value,kind=4)

        case ("i_max")
          bc%i_max = real(attribute_value,kind=4)

        case ("Iterations")
          bc%Iterations = int(attribute_value)

        case ("nue_min")
          bc%nue_min = real(attribute_value,kind=4)

        case ("nue_max")
          bc%nue_max = real(attribute_value,kind=4)

        case ("ObjectCount")
          bc%ObjectCount = int(attribute_value)

        case ("ObjectMass_min")
          bc%ObjectMass_min = attribute_value

        case ("ObjectMass_max")
          bc%ObjectMass_max = attribute_value

        case ("omega_min")
          bc%omega_min = real(attribute_value,kind=4)

        case ("omega_max")
          bc%omega_max = real(attribute_value,kind=4)

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
