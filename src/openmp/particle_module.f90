!> \\particalmodule.f90
module particle_module
  use constantsmodule, only: dp, pie, density_material
  implicit none
  private
  public :: particle

  type :: particle
    real(dp) :: x, y, z        ! position
    real(dp) :: u, v, w        ! velocity
    real(dp) :: fx, fy, fz     ! force accumulator
    real(dp) :: mass, radius
  contains
    procedure :: init_radius    => particle_init_radius
    procedure :: half_kick      => particle_half_kick
    procedure :: drift          => particle_drift
    procedure :: zero_force     => particle_zero_force
    procedure :: collide_with   => particle_collide_with
  end type particle

contains


  subroutine particle_init_radius(self)
    class(particle), intent(inout) :: self
    self%radius = (self%mass / density_material * 0.75_dp / pie) ** (1.0_dp / 3.0_dp)
  end subroutine


  subroutine particle_zero_force(self)
    class(particle), intent(inout) :: self
    self%fx = 0.0_dp
    self%fy = 0.0_dp
    self%fz = 0.0_dp
  end subroutine


  subroutine particle_half_kick(self, dt)
    class(particle), intent(inout) :: self
    real(dp),        intent(in)    :: dt
    real(dp) :: half_dt
    half_dt  = 0.5_dp * dt
    self%u = self%u + (self%fx / self%mass) * half_dt
    self%v = self%v + (self%fy / self%mass) * half_dt
    self%w = self%w + (self%fz / self%mass) * half_dt
  end subroutine


  subroutine particle_drift(self, dt)
    class(particle), intent(inout) :: self
    real(dp),        intent(in)    :: dt
    self%x = self%x + self%u * dt
    self%y = self%y + self%v * dt
    self%z = self%z + self%w * dt
  end subroutine


  subroutine particle_collide_with(self, other, elastic)
    class(particle), intent(inout) :: self
    type(particle),  intent(inout) :: other
    real(dp),        intent(in)    :: elastic
    real(dp) :: m1, m2, v1(3), v2(3), n(3), mag
    real(dp) :: rel(3), impulse

    ! unit normal between centres
    n   = [self%x - other%x, self%y - other%y, self%z - other%z]
    mag = sqrt(dot_product(n, n))
    if (mag <= 0.0_dp) return
    n = n / mag

    m1 = self%mass;  m2 = other%mass
    v1 = [self%u,  self%v,  self%w ]
    v2 = [other%u, other%v, other%w]

    rel     = v1 - v2
    impulse = (1.0_dp + elastic) * dot_product(rel, n) / (1.0_dp/m1 + 1.0_dp/m2)

    v1 = v1 - (impulse / m1) * n
    v2 = v2 + (impulse / m2) * n

    ! written back immediately — no caller writeback needed
    self%u  = v1(1);  self%v  = v1(2);  self%w  = v1(3)
    other%u = v2(1);  other%v = v2(2);  other%w = v2(3)
  end subroutine

end module particle_module

