!> \\2objects.f90
program sphere_collision_3d
    implicit none

    real :: m1, m2
    real :: x1(3), x2(3)
    real :: u1(3), u2(3)
    real :: v1(3), v2(3)
    real :: n(3)
    real :: relv(3)
    real :: dist
    real :: dotprod
    real :: vn(3)
    real :: e
    real :: j(3)
    integer :: i

    e = 0.1
   
    !---------------------------------------
    ! Example data
    !---------------------------------------

    m1 = 1.0
    m2 = 100.0

    ! Sphere centers at impact
    x1 = [1.0, 0.0, 0.0]
    x2 = [0.0, 0.0, 0.0]

    ! Initial velocities
    u1 = [-1.0, 0.0, 0.0]
    u2 = [1.0, 0.0, 0.0]

    !---------------------------------------
    ! Compute collision normal
    !---------------------------------------

    n = x1 - x2

    dist = sqrt(sum(n**2))

    if (dist <= 0.0) then
        print *, 'Error: sphere centers coincide'
        stop
    end if

    n = n / dist

    !---------------------------------------
    ! Relative velocity
    !---------------------------------------

    relv = u1 - u2

    dotprod = sum(relv * n)

    !---------------------------------------
    ! Elastic collision
    !---------------------------------------
    vn = dot_product(u1-u2, n)
    
    j = -(1.0 + e) * vn / (1.0/m1 + 1.0/m2)

    v1 = u1 + (j/m1) * n
    v2 = u2 - (j/m2) * n

    !v1 = u1 - (2.0*m2/(m1+m2))*dotprod*n

    !v2 = u2 + (2.0*m1/(m1+m2))*dotprod*n

    !---------------------------------------
    ! Output
    !---------------------------------------

    print *
    print *, 'Collision normal n ='
    print '(3F10.4)', n

    print *
    print *, 'Initial velocity sphere 1:'
    print '(3F10.4)', u1

    print *, 'Initial velocity sphere 2:'
    print '(3F10.4)', u2

    print *
    print *, 'Final velocity sphere 1:'
    print '(3F10.4)', v1

    print *, 'Final velocity sphere 2:'
    print '(3F10.4)', v2

end program sphere_collision_3d
