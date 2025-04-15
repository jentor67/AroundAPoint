!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none

   real :: force, gravity 
   type(typeTest1) :: a 

   write(*,*) "Value of pi =",pie
    
   gravity = acceleration(mass_earth,radius_earth)
      
   write(*,*) "Gravity at earth surface is ", gravity


   a%xValue = 24
   a%yValue = 36

   write(*,*) a%xValue
   write(*,*) a%yValue

   call printTest1(a)

   write(*,*) a%xValue

   write(*,*) acceleration(mass_earth,radius_earth)

   write(*,*) distance(2.0,2.3,4.0,4.8,5.0,2.0)

End Program aroundAPoint
