!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none

   real :: force, gravity, distance1 
   type(typeTest1) :: a 
   type(particle) :: sun
   type(particle) :: earth
   type(particle) :: jupiter
  
   call valuessun(sun)
   call valuesearth(earth)
   call valuesjupiter(jupiter)
      
   call forcevector(earth,sun,fx,fy,fz)

   write(*,*) fx, fy, fz



End Program aroundAPoint
