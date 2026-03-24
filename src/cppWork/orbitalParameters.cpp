#include<iostream>
#include<cmath>
#include "constants.hpp"
#include "orbitalParameters.hpp"
#include "structures.hpp"
#include "vectors.hpp"

using namespace std;

double force(double m1, double m2, double r)
{
    double f;
    f = G * m1 * m2 / pow(r,2);

    return(f);
}

void radiusVelocity(double m, double a, double e, double i, 
    double OMEGA, double omega, double* rp, double* ra, 
    double* mue, double* T )
{
    double tmue;
    *rp = (1-e)*a; // distance at perigee (m)
    *ra = (1+e)*a; // distance at apogee (m)
    tmue = G*(mSagA+m); // standard gravitational parameters
    //tmue = mue;
    //*vp = pow(((1+e)*tmue)/((1-e)*a),.5); // velocity at perigee
    //*va = pow(((1-e)*tmue)/((1+e)*a),.5); // velocity at appegee
    *T = 2 * pi * pow( pow(a,3) /tmue , .5); // Peroid
    *mue = tmue;
}

double schwarzchildRadius(double m)
{
    double r;
    
    r = 2*G*m/pow(c,2);

    return(r);
}

void startPointVelocity(double a, double e, double nue, double rp, 
    double omega, double i, double OMEGA, double mue, double b, 
    double* x, double* y, double* z, double* u, double* v, double* w)
{
    double r, rho, xp;
    double xt, yt, zt, ut, vt, wt;

    // Rotate nue  degrees
    r = a*(1-pow(e,2))/(1+e*cos(nue/180*pi));
    *x = r*cos(pi*nue/180);
    xp = a-rp+*x;
    *y = r*sin(pi*nue/180);
    rho = 180*atan( *y/xp )/pi;
    *z = 1.0;

    if( xp < 0 ) rho += 180;

    if( ( xp > 0 ) && ( y < 0 ) ) rho += 360; 

    *v = pow( (2*mue/r) - (mue/a), .5);
    

    tangentVectorEllipse(xp, *y, a, b, *v, &ut, &vt);
    *u = ut;
    *v = vt;


    // Rotate omega  degrees
    rotate2D(*x, *y, omega, &xt, &yt);
    *x = xt;
    *y = yt;

    rotate2D(*u, *v, omega, &ut, &vt);
    *u = ut;
    *v = vt;

    // Rotate i degrees
    rotate2D(*y, *z, i, &yt, &zt);
    *y = yt;
    *z = zt;
   
    rotate2D(*v, *w,  i, &vt, &wt);
    *v = vt;
    *w = wt; 

    // Rotate OMEGA degrees
    rotate2D(*x, *y, OMEGA, &xt, &yt);
    *x = xt;
    *y = yt;


    rotate2D(*u, *v, OMEGA, &ut, &vt);
    *u = ut;
    *v = vt;


}


void stepMotion( BlackHole bh[] )
{
    int iStart1, iStart2;
    double fx0, fy0, fz0;
    double fx, fy, fz;
    double ax, ay, az;
    double vx, vy, vz;
    double x, y, z;
    double dummy;

    

    iStart1 = 0;

    while( iStart1 < objects)
    {
        iStart2 = 0;
        fx0=0;
        fy0=0;
        fz0=0;

        while( iStart2 < objects )
        {
            if( iStart1 != iStart2)
            {
                forceVectorResults(bh[iStart1],bh[iStart2], &fx, &fy, &fz, iStart1, iStart2);
                fx0 += fx;  
                fy0 += fy;  
                fz0 += fz;  
            }
            iStart2 +=1;
        }
        // find new acceleration
        accelerationVector(fx0, fy0, fz0, bh[iStart1].mass, &ax, &ay, &az);

        // find new velocity
        velocityVector( bh[iStart1], ax, ay, az, &vx, &vy, &vz);

        // update new velocity
        bh[iStart1].u = vx;
        bh[iStart1].v = vy;
        bh[iStart1].w = vz;

        // find new corridinates
        positionVector( bh[iStart1], &x, &y, &z);

        // update new corridinates
        bh[iStart1].x = x;
        bh[iStart1].y = y;
        bh[iStart1].z = z;
        
        iStart1 +=1;
  
    }    
//    return bh;
}

void forceVector(double ux, double uy, double uz, double F,
    double* fx, double* fy, double* fz)
{
    *fx = ux * F;
    *fy = uy * F;
    *fz = uz * F;
}

void accelerationVector(double fx, double fy, double fz, double m, 
    double* ax, double* ay, double* az)
{
    *ax = fx/m;
    *ay = fy/m;
    *az = fz/m;
}

//    double* u, double* v, double* w)
void velocityVector( BlackHole ob, double ax, double ay, 
    double az, double* u, double*  v, double* w)
{
    *u = ob.u + ax*TIMESTEP;
    *v = ob.v + ay*TIMESTEP;
    *w = ob.w + az*TIMESTEP;
}

//    double* x, double* y, double* z)
void positionVector( BlackHole ob, double* x, double* y, double* z) 
{
    *x = ob.x + ob.u*TIMESTEP;
    *y = ob.y + ob.v*TIMESTEP;
    *z = ob.z + ob.w*TIMESTEP;
}

double radius( BlackHole ob1, BlackHole ob2)
{
    double r;
    r = pow( pow(ob2.x-ob1.x, 2) + pow(ob2.y-ob1.y,2) + pow(ob2.z-ob1.z, 2), .5);

    return( r);
}

void forceVectorResults(BlackHole ob1, BlackHole ob2, double* fx, 
    double* fy, double* fz, int i, int j)
{
    double r;   
    r = pow( pow(ob2.x-ob1.x, 2) + pow(ob2.y-ob1.y,2) + pow(ob2.z-ob1.z, 2), .5);

    //if( r < (.001*LIGHTYEAR)) 
    //    cout << "DANGER CLOSE: " << r << " m" << " i=" << i << " j=" << j << '\n';

    double f;
    f = force(ob1.mass,ob2.mass,r);

    double vx, vy, vz;
    Vector(ob1.x, ob1.y, ob1.z, ob2.x, ob2.y, ob2.z, &vx, &vy, &vz);
    
    double ux, uy, uz;
    unitVector(vx, vy, vz, &ux, &uy, &uz);

    double fx0, fy0, fz0;
    forceVector(ux, uy, uz, f, &fx0, &fy0, &fz0);

    *fx = fx0;
    *fy = fy0;
    *fz = fz0;
}
