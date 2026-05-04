!> \\file blender_config_file.f90
module blenderconfigmodule
  use constantsmodule
  use readconfigmodule
  implicit none

  public :: write_blender_file

contains

  subroutine write_blender_file()
    type(boundaryconditions) :: cf
    character(len=250) :: blender_file
    character(len=250) :: file_name
    integer :: file_unit

    file_name = "blender_config_file.txt"

    blender_file = trim(adjustl(cf%output_directory)) // '/' // &
            trim(file_name)

    open(newunit=file_unit, file=trim(blender_file), &
            status="replace", action="write")

    write(file_unit,10) bc%ObjectCount, bc%blender_limit


    close(file_unit)

    10  format(i0,"|",i0)
  end subroutine write_blender_file


end module blenderconfigmodule
