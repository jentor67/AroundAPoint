!> \\file readconfigmodule.f95
module readconfigmodule
  implicit none
  
  public :: read_config_file

  type boundaryconditions
    real(kind=kind(1.0d0)) :: CenterMass
    real(kind=kind(1.0d0)) :: minObjectMass
    real(kind=kind(1.0d0)) :: maxObjectMass
    integer :: ObjectCount
    integer :: Iterations
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
        case ("CenterMass")
          bc%CenterMass = attribute_value
        case ("Iterations")
          bc%Iterations = int(attribute_value)
        case ("minObjectMass")
          bc%minObjectMass = attribute_value
        case ("maxObjectMass")
          bc%maxObjectMass = attribute_value
        case ("ObjectCount")
          bc%ObjectCount = int(attribute_value)
        case default
        print *, "Unknown command"
      end select

    end do

    close(unit)
    
    write(*,*) bc%CenterMass, bc%Iterations, bc%minObjectMass, &
          bc%maxObjectMass, bc%ObjectCount
  
  end subroutine read_config_file

end module readconfigmodule
