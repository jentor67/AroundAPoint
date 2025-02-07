!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   use testModule 
   implicit none

   real :: me = 5.972168E24, re = 6371000
   real :: F, g, Gc = 6.674083E-11, pi=3.1415926535897932384626
   type(typeTest1) :: a 

   write(*,*) "Value of pi =",pi
    
   g = Gc*me/(re**2)
      
   write(*,*) "Gravity at earth surface is ", g


   a%xValue = 2

   write(*,*) a%xValue

   call printTest1(a)

   write(*,*) a%xValue

   write(*,*) acceleration(me,re)
End Program aroundAPoint
