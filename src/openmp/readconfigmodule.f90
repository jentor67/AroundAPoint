!> \\file readconfigmodule.f95
module readconfigmodule
  use constantsmodule
  implicit none
  
  public :: read_config_file

  type particle
    real(dp) :: x
    real(dp) :: y
    real(dp) :: z
    real(dp) :: u
    real(dp) :: v
    real(dp) :: w
    real(dp) :: fx
    real(dp) :: fy
    real(dp) :: fz
    real(dp) :: radius
    real(dp) :: mass
  end type particle


  type boundaryconditions
    character(len=256) ::output_directory
    character(len=256) ::file_type

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

  subroutine  read_config_file(filepath, sel)
    implicit none
    integer :: unit, ios, particle_count
    character(len=256) ::filepath
    character(len=256) ::attribute
    character(len=256) ::line
    character(len=256) :: attribute_value
    !type(particle) sel
    !real, allocatable, intent(out) :: arr(:)
    type(particle), allocatable, intent(out) :: sel(:)

    open(newunit=unit, file=filepath, status="old", action="read")

    bc%a = -10000.0
    bc%ObjectMass = -10000.0
    bc%e = -10000.0
    bc%i = -10000.0
    bc%nue = -10000.0
    bc%omega = -10000.0
    bc%omegabig = -10000.0
    particle_count = 1

    do
      !read(unit, *, iostat=ios) attribute, attribute_value, a1, a2, a3, a4, a5, a6
      !if (ios /= 0) exit

      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      line = adjustl(line)

      ! Extract keyword (first whitespace-delimited token)
      call split_keyword(line, attribute, attribute_value)

      select case (trim(attribute))
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
        
        case ("LIST")
          ! read list of objects
          if (index(attribute_value, ',') > 0) then
            ! Data row: comma-separated values
            call replace_char(attribute_value, ',', ' ')
            read(attribute_value, *) sel(particle_count)%x, &
            sel(particle_count)%y, sel(particle_count)%z, &
            sel(particle_count)%u, sel(particle_count)%v, &
            sel(particle_count)%w, sel(particle_count)%mass


            !list_data(list_count, :) = [v1,v2,v3,v4,v5,v6,v7]
          end if
          ! else: "Type LIST" header already handled above


          write(*,*) "Value", sel(particle_count)%x, &
            sel(particle_count)%y, sel(particle_count)%z, &
            sel(particle_count)%u, sel(particle_count)%v, &
            sel(particle_count)%w, sel(particle_count)%mass
          particle_count = particle_count + 1
        
        case ("nue")
          read(attribute_value,*) bc%nue

        case ("nue_min")
          read(attribute_value,*) bc%nue_min

        case ("nue_max")
          read(attribute_value,*) bc%nue_max

        case ("ObjectCount")
          read(attribute_value,*) bc%ObjectCount
          allocate(sel(bc%ObjectCount))

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
          bc%output_directory = trim(adjustl(attribute_value))
          ! Remove surrounding double quotes if present
          if (bc%output_directory(1:1) == '"') then
              bc%output_directory = bc%output_directory(2:len_trim(bc%output_directory)-1)
          end if
        case("Type")
          bc%file_type = attribute_value

        case default
          print *, "Unknown command --> ", attribute, attribute_value

      end select

    end do

    close(unit)
    
    !write(*,*) bc%CenterMass, bc%Iterations, bc%ObjectMass_min, &
    !      bc%ObjectMass_max, bc%ObjectCount
  
  end subroutine read_config_file


    ! Split a line into its first token (keyword) and the rest (remainder)
  subroutine split_keyword(line, keyword, remainder)
    character(len=*), intent(in)  :: line
    character(len=*), intent(out) :: keyword, remainder
    integer :: pos

    pos = index(trim(line), ' ')
    if (pos == 0) then
      keyword   = trim(line)
      remainder = ''
    else
      keyword   = line(1:pos-1)
      remainder = adjustl(line(pos+1:))
    end if
  end subroutine

  ! Replace every occurrence of old character with new character
  subroutine replace_char(str, old, new)
    character(len=*), intent(inout) :: str
    character(1),     intent(in)    :: old, new
    integer :: i
    do i = 1, len(str)
      if (str(i:i) == old) str(i:i) = new
    end do
  end subroutine

end module readconfigmodule
