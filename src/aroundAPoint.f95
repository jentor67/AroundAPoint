!> \\file aroundAPoint.f
Program aroundAPoint
   use gravityModule 
   implicit none

   real :: pi =3.1359, Gc = 6.674083E-11
   real :: me = 5.972168E24, re = 6371000
   real :: F, g

   write(*,*) "Value of pi =",pi
   write(*,*) "Value of G =", Gc
    
   g = Gc*me/(re**2)
      
   write(*,*) "Gravity at earth surface is ", g

End Program aroundAPoint
