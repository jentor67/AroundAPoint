!> \\file readconfigmodule.f95
module readconfigmodule
  use constantsmodule
  implicit none
  
  public :: read_config_file

  type boundaryconditions
    character(len=256) ::output_directory

    real(dp) :: a
    real(dp) :: a_min
    real(dp) :: a_max
    real(dp) :: CenterMass
    real(dp) :: ObjectMass
    real(dp) :: ObjectMass_min
    real(dp) :: ObjectMass_max
   
    real(dp) :: e
    real(dp) :: e_min
    real(dp) :: e_max
    real(dp) :: i
    real(dp) :: i_min
    real(dp) :: i_max
    real(dp) :: nue
    real(dp) :: nue_min
    real(dp) :: nue_max
    real(dp) :: omega
    real(dp) :: omega_min
    real(dp) :: omega_max
    real(dp) :: omegabig
    real(dp) :: omegabig_min
    real(dp) :: omegabig_max
   
    integer :: blender_limit 
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
    !real(dp) :: attribute_value
    character(len=256) :: attribute_value

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
          read(attribute_value,*) bc%a
          
        case ("a_min")
          read(attribute_value,*) bc%a_min 

        case ("a_max")
          read(attribute_value,*) bc%a_max 

        case("BlenderLimit")
          read(attribute_value,*) bc%blender_limit

        case ("CenterMass")
          read(attribute_value,*) bc%CenterMass 
       
        case ("e")
          read(attribute_value,*) bc%e

        case ("e_min")
          read(attribute_value,*) bc%e_min

        case ("e_max")
          read(attribute_value,*) bc%e_max

        case ("i")
          read(attribute_value,*) bc%i

        case ("i_min")
          read(attribute_value,*) bc%i_min

        case ("i_max")
          read(attribute_value,*) bc%i_max

        case ("Iterations")
          read(attribute_value,*) bc%Iterations

        case ("nue")
          read(attribute_value,*) bc%nue

        case ("nue_min")
          read(attribute_value,*) bc%nue_min

        case ("nue_max")
          read(attribute_value,*) bc%nue_max

        case ("ObjectCount")
          read(attribute_value,*) bc%ObjectCount

        case ("ObjectMass")
          read(attribute_value,*) bc%ObjectMass

        case ("ObjectMass_min")
          read(attribute_value,*) bc%ObjectMass_min

        case ("ObjectMass_max")
          read(attribute_value,*) bc%ObjectMass_max

        case ("omega")
          read(attribute_value,*) bc%omega

        case ("omega_min")
          read(attribute_value,*) bc%omega_min

        case ("omega_max")
          read(attribute_value,*) bc%omega_max

        case ("omegaBig")
          read(attribute_value,*) bc%omegabig

        case ("omegaBig_min")
          read(attribute_value,*) bc%omegabig_min

        case ("omegaBig_max")
          read(attribute_value,*) bc%omegabig_max
        
        case("outputDirectory")
          bc%output_directory = attribute_value

        case default
          print *, "Unknown command --> ", attribute, attribute_value

      end select

    end do

    close(unit)
    
    write(*,*) bc%CenterMass, bc%Iterations, bc%ObjectMass_min, &
          bc%ObjectMass_max, bc%ObjectCount
  
  end subroutine read_config_file

end module readconfigmodule
