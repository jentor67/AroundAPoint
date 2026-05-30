!> \\main.f90
program main
   use constantsmodule
   use vectormodule
   implicit none

   real(dp) :: mass1, mass2
   real(dp) :: p1(3), p2(3)
   real(dp) :: vel1(3), vel2(3)

   mass1 = 1.0
   mass2 = 100.0

    ! Sphere centers at impact
   p1 = [1.0, 0.0, 0.0]
   p2 = [0.0, 0.0, 0.0]

    ! Initial velocities
   vel1 = [-1.0, 0.0, 0.0]
   vel2 = [1.0, 0.0, 0.0]

   call sphere_collision_3d(p1, p2, vel1, vel2, mass1, mass2)

end program main
