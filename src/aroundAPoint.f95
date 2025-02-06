!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none

   real :: me = 5.972168E24, re = 6371000
   real :: F, g

   write(*,*) "Value of pi =",pi
    
   g = Gc*me/(re**2)
      
   write(*,*) "Gravity at earth surface is ", g

End Program aroundAPoint
